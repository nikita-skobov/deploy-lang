use std::collections::HashMap;
use std::error::Error;

use dcl_language::parse::{Logger, Section, SpannedDiagnostic};
use lsp_server::{Connection, Message, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{CompletionItem, CompletionOptions, CompletionParams, CompletionTextEdit, InsertReplaceEdit, TextEdit, Url};
use lsp_types::{
    Diagnostic,
    DiagnosticSeverity,
    DidChangeTextDocumentParams,
    InitializeParams,
    Position,
    PublishDiagnosticsParams,
    Range,
    ServerCapabilities,
    TextDocumentSyncCapability,
    TextDocumentSyncKind,
    // notifications
    notification::PublishDiagnostics,
};

fn main() {
    let (connection, io_thread) = Connection::stdio();

    // read initialize request
    let (id, params) =
        connection.initialize_start().unwrap();

    // advertised capabilities
    let caps = ServerCapabilities {
        // needed to get text document notifications
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        // needed to receive textDocument/completion requests
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string()]),
            ..Default::default()
        }),
        ..Default::default()
    };

    // send initialize response
    let init_result = serde_json::json!({
        "capabilities": caps,
        "offsetEncoding": ["utf-8"],
    });
    connection.initialize_finish(id, init_result).unwrap();
    send_log(&connection, "finished initialization");
    main_loop(connection, params).unwrap();
    io_thread.join().unwrap();
}

fn spanned_diag_to_lsp_diag(e: SpannedDiagnostic) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position { line: e.span.start.line as _, character: e.span.start.column as _ },
            end: Position { line: e.span.end.line as _, character: e.span.end.column as _ },
        },
        severity: Some(match e.severity {
            0 => DiagnosticSeverity::INFORMATION,
            1 => DiagnosticSeverity::HINT,
            2 => DiagnosticSeverity::WARNING,
            _ => DiagnosticSeverity::ERROR,
        }),
        code: None,
        code_description: None,
        source: Some("my-lsp".into()),
        message: e.message,
        related_information: None,
        tags: None,
        data: None,
    }
}
#[derive(Clone)]
pub struct CodeLogger<'a> {
    connection: &'a Connection,
}

impl<'a> Logger for CodeLogger<'a> {
    fn log(&mut self, log: &str) {
        send_log(self.connection, log);
    }
}

fn check_dcl_file<'a>(diagnostics: &mut Vec<Diagnostic>, valid_sections: &Vec<Section<'a>>, connection: &Connection) {
    let clg = CodeLogger { connection };
    if let Err(e) = dcl_language::parse::sections_to_dcl_file_with_logger(valid_sections, clg) {
        diagnostics.push(spanned_diag_to_lsp_diag(e));
    }
}

fn split_sections<'a>(document: &'a str, connection: &Connection) -> (Vec<Diagnostic>, Vec<Section<'a>>) {
    let clg = CodeLogger { connection };
    split_sections_with_logger(document, clg)
}

fn split_sections_with_logger<'a>(document: &'a str, logger: impl Logger) -> (Vec<Diagnostic>, Vec<Section<'a>>) {
    let sections = dcl_language::parse::parse_document_to_sections_with_logger(document, logger);
    let mut out = vec![];
    let mut valid_sections = vec![];
    for section in sections {
        let e = match section {
            Ok(o) => {
                valid_sections.push(o);
                continue;
            },
            Err(e) => e,
        };
        let diag = spanned_diag_to_lsp_diag(e);
        out.push(diag);
    }
    (out, valid_sections)
}

pub fn handle_completion_request_ex<'a>(
    params: CompletionParams,
    doc: &ParsedDoc<'a>,
) -> Option<Vec<CompletionItem>> {
    let original_pos = params.text_document_position.position.clone();
    let Position { line, character } = params.text_document_position.position;
    let line_index = line as usize;
    // remember a character position is really between two characters:
    // its the position of the cursor. so column 1 means user typed a char
    // at the start of the line, and their cursor is currently after that first character
    // so for us to look up the character they just typed, we'd need to get the character at index 0
    let column = character as usize;
    let char_index = if column == 0 { column } else { column - 1 };
    let line = doc.doc.lines().nth(line_index)?;
    let (trigger_index, trigger_character) = line.char_indices()
        .nth(char_index)?;
    // for now we'll only care about completions for json paths
    // TODO: support completions for filling in known template names
    // when defining resources, language keywords like "template", "resource", etc.
    if trigger_character != '.' {
        return None
    }
    // go backwards and parse until a '$' is found, indicating a json path query
    let mut json_path = String::with_capacity(char_index);
    for (i, c) in line.char_indices().rev() {
        // ignore everything including the trigger character index and after:
        if i >= trigger_index {
            continue;
        }
        json_path.insert(0, c);
        if c == '$' {
            break;
        }
    }
    // if the dollar sign was never found its not a json path:
    if !json_path.starts_with("$") {
        return None;
    }
    // ensure the json path is valid:
    let json_path_query = jsonpath_rust::parser::parse_json_path(&json_path).ok()?;

    // now determine the section its within from the already parsed sections:
    let mut found_section: Option<&Section<'a>> = None;
    let all_valid_sections = &doc.parsed;
    for section in all_valid_sections.iter() {
        if line_index >= section.start_line && line_index <= section.end_line {
            found_section = Some(section);
        }
    }
    // collect json object from all other resource sections:
    let found_section = found_section?;
    let mut dcl = dcl_language::DclFile::default();
    for section in all_valid_sections.iter() {
        if std::ptr::addr_eq(section, found_section) {
            // ignore the found section
            continue;
        }
        if section.typ == dcl_language::parse::resource::SECTION_TYPE {
            let _ = dcl_language::parse::resource::parse_resource_section(&mut dcl, section);
        }
    }
    // now create a json object from all of the resources that were found:
    // TODO: need to consider performance here...
    // might want to process the first segment in the json path out-of-band
    // to look up the resource, and then use json path query on its input
    let mut resource_inputs = serde_json::Map::new();
    for resource in dcl.resources.iter() {
        let val = resource.input.clone().to_serde_json_value();
        resource_inputs.insert(resource.resource_name.clone(), val);
    }
    let all_resource_inputs = serde_json::Value::Object(resource_inputs);

    let mut res = jsonpath_rust::query::js_path_process(
        &json_path_query, &all_resource_inputs
    ).ok()?;
    let last = res.pop()?;
    let val = last.val();
    let map = val.as_object()?;
    // return all keys of the object as valid completion items:
    let mut completions = Vec::with_capacity(map.len());
    for key in map.keys() {
        let mut completion_text_edit = None;
        let mut additional_text_edits = None;
        if key.contains(' ') {
            let mut original_pos_minus_1 = original_pos.clone();
            if original_pos_minus_1.character != 0 {
                original_pos_minus_1.character -= 1;
            }
            // let mut original_pos_plus_1 = original_pos.clone();
            // original_pos_plus_1.character += 1;
            // the insert range must be a prefix of the replace range, that is
            // both must start at the same position, and the replace range may be longer than the insert range.
            // also: it must be a single line, and must be on the same line from the original
            // completion params
            completion_text_edit = Some(CompletionTextEdit::InsertAndReplace(InsertReplaceEdit {
                new_text: format!("['{}']", key),
                insert: Range { start: original_pos, end: original_pos },
                replace: Range { start: original_pos, end: original_pos },
            }));
            additional_text_edits = Some(vec![TextEdit {
                range: Range { start: original_pos_minus_1, end: original_pos },
                new_text: "".to_string(),
            }]);
        }
        completions.push(CompletionItem {
            label: key.clone(),
            text_edit: completion_text_edit,
            additional_text_edits,
            ..Default::default()
        });
    }
    Some(completions)
}

pub fn handle_completion_request<'a>(
    params: CompletionParams,
    doc: &ParsedDoc<'a>,
) -> Option<serde_json::Value> {
    let completion_items = handle_completion_request_ex(params, doc)?;
    serde_json::to_value(completion_items).ok()
}

pub struct ParsedDoc<'a> {
    pub doc: &'a String,
    pub parsed: Vec<Section<'a>>,
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> std::result::Result<(), Box<dyn Error + Sync + Send>> {
    let mut known_docs: HashMap<Url, String> = HashMap::new();
    let mut parsed_docs: HashMap<Url, ParsedDoc> = HashMap::new();
    let _init: InitializeParams = serde_json::from_value(params)?;
    for msg in &connection.receiver {
        match msg {
            Message::Request(request) => {
                send_log(&connection, &format!("received request: {:?}", request));
                if request.method == "textDocument/completion" {
                    let params: CompletionParams = serde_json::from_value(request.params).unwrap();
                    let uri = &params.text_document_position.text_document.uri;
                    let parsed = parsed_docs.get(uri);
                    let parsed = if let Some(parsed) = parsed {
                        parsed
                    } else {
                        match known_docs.get(&uri) {
                            Some(s) => {
                                let (_, valid_sections) = split_sections(s, &connection);
                                parsed_docs.insert(uri.clone(), ParsedDoc { parsed: valid_sections, doc: s });
                            }
                            None => continue
                        };
                        match parsed_docs.get(&uri) {
                            Some(parsed) => parsed,
                            None => continue
                        }
                    };
                    let result = handle_completion_request(params, parsed);
                    let result = if result.is_none() {
                        Some(serde_json::Value::Null)
                    } else { result };
                    send_response(&connection, Response { id: request.id, result, error: None });
                    continue;
                }
            }
            Message::Response(response) => {
                send_log(&connection, &format!("unexpected response received: {:?}", response));
            }
            Message::Notification(notif) => {
                if notif.method == "textDocument/didChange" {
                    let params: DidChangeTextDocumentParams = serde_json::from_value(notif.params).unwrap();
                    let uri = params.text_document.uri;
                    let new_document = params.content_changes.first().map(|x| x.text.as_str()).unwrap_or_default();
                    parsed_docs = HashMap::new();
                    known_docs.insert(uri.clone(), new_document.to_string());
                    let diagnostics = if let Some(s) = known_docs.get(&uri) {
                        let (mut diagnostics, valid_sections) = split_sections(s, &connection);
                        check_dcl_file(&mut diagnostics, &valid_sections, &connection);
                        parsed_docs.insert(uri.clone(), ParsedDoc { parsed: valid_sections, doc: s });
                        diagnostics
                    } else {
                        // should be unreachable since we just inserted into known_docs
                        vec![]
                    };
                    let params = PublishDiagnosticsParams {
                        uri,
                        diagnostics: diagnostics,
                        version: None,
                    };
                    connection.sender.send(Message::Notification(
                        lsp_server::Notification {
                            method: PublishDiagnostics::METHOD.to_string(),
                            params: serde_json::to_value(params).unwrap(),
                        }
                    )).unwrap();
                    send_log(&connection, "sending diagnostics");
                } else {
                    send_log(&connection, &format!("received unexpected notification: {}", notif.method));
                }
            }
        }
    }
    Ok(())
}

fn send_response(
    connection: &Connection,
    resp: Response,
) {
    connection.sender.send(Message::Response(resp)).expect("failed to send response to client");
}

fn send_log<S: AsRef<str>>(
    connection: &Connection,
    message: S,
) {
    let log = serde_json::Value::String(message.as_ref().to_string());
    connection.sender.send(Message::Notification(
        lsp_server::Notification {
            method: "custom/log".to_string(),
            params: log,
        }
    )).expect("failed to send log notification to client");
}

#[cfg(test)]
mod test {
    use lsp_types::{PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams, WorkDoneProgressParams};

    use super::*;

    fn completion_params_with_position(
        line: u32,
        column: u32
    ) -> CompletionParams {
        CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier {
                    uri: Url::parse("https://example.net").unwrap(),
                },
                position: Position { line: line, character: column },
            },
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: PartialResultParams {
                partial_result_token: None,
            },
            context: None,
        }
    }

    #[test]
    fn can_provide_completions_for_json_paths() {
        let completion_request_line = 1;
        let completion_request_column = 21;
        let other_completion_column = 36;
        // $. at column 21
        // $.other_resource. at column 36
        let document = r#"resource some_template(a)
   {"a": "b", "c": $.other_resource.}

resource some_template(other_resource)
  {"option1": null, "option2": null}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_column);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $. so it should recommend resource names, of which theres only 1 "other_resource"
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 1);
        assert_eq!(res.remove(0).label, "other_resource");

        // the column is at $.other_resource. so it should recommend fields within other_resource
        let params = completion_params_with_position(completion_request_line, other_completion_column);
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 2);
        res.sort_by(|a, b| a.label.cmp(&b.label));
        assert_eq!(res.remove(0).label, "option1");
        assert_eq!(res.remove(0).label, "option2");
    }

    #[test]
    fn json_path_completions_use_bracketed_syntax_if_theres_a_space() {
        // $.other_resource. at column 36
        let document = r#"resource some_template(a)
   {"a": "b", "c": $.other_resource.}

resource some_template(other_resource)
  {"opti on1": null}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(1, 36);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $.other_resource.
        // so it should recomment the keys within "other_resource"
        // but since it has a space, it should recommend an insert with ['opti on1']
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 1);
        let completion = res.remove(0);
        assert_eq!(completion.label, "opti on1");
        assert_eq!(completion.additional_text_edits.unwrap().len(), 1);
        let text_edit = completion.text_edit.unwrap();
        match text_edit {
            CompletionTextEdit::InsertAndReplace(insert_replace_edit) => {
                assert_eq!(insert_replace_edit.new_text, "['opti on1']");
            }
            CompletionTextEdit::Edit(_) => panic!("it should be a insert and replace")
        }
    }
}
