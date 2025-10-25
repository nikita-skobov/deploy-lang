use std::collections::HashMap;
use std::error::Error;

use deploy_language::parse::{Logger, Section, SpannedDiagnostic};
use lsp_server::{Connection, Message, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{CompletionItem, CompletionOptions, CompletionParams, CompletionTextEdit, DidOpenTextDocumentParams, Documentation, Hover, HoverContents, HoverParams, HoverProviderCapability, InsertReplaceEdit, MarkupContent, TextEdit, Url};
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
        hover_provider: Some(HoverProviderCapability::Simple(true)),
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

fn check_dpl_file<'a>(diagnostics: &mut Vec<Diagnostic>, valid_sections: &Vec<Section<'a>>, connection: &Connection) {
    let clg = CodeLogger { connection };
    match deploy_language::parse::sections_to_dpl_file_with_logger(valid_sections, clg) {
        Ok(dpl) => {
            let spanned_diag = deploy_language::validate::validate_dpl_file(&dpl);
            for diag in spanned_diag {
                diagnostics.push(spanned_diag_to_lsp_diag(diag));
            }
        }
        Err(e) => {
            diagnostics.push(spanned_diag_to_lsp_diag(e));
        }
    }
}

fn split_sections<'a>(document: &'a str, connection: &Connection) -> (Vec<Diagnostic>, Vec<Section<'a>>) {
    let clg = CodeLogger { connection };
    split_sections_with_logger(document, clg)
}

fn split_sections_with_logger<'a>(document: &'a str, logger: impl Logger) -> (Vec<Diagnostic>, Vec<Section<'a>>) {
    let sections = deploy_language::parse::parse_document_to_sections_with_logger(document, logger);
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
    let found_section = found_section?;
    match found_section.typ.s {
        deploy_language::parse::resource::SECTION_TYPE => {
            handle_resource_jsonpath_completion_request(all_valid_sections, found_section, json_path_query, original_pos)
        },
        deploy_language::parse::template::SECTION_TYPE => {
            handle_template_jsonpath_completion_request(all_valid_sections, found_section, json_path_query, original_pos)
        }
        _ => None,
    }
}

pub fn handle_template_jsonpath_completion_request<'a>(
    _all_valid_sections: &Vec<Section<'a>>,
    _found_section: &Section<'a>,
    json_path_query: jsonpath_rust::parser::model::JpQuery,
    _original_pos: Position,
) -> Option<Vec<CompletionItem>> {
    // any jsonpath within a template section must start with $.input $.output $.accum or $.name
    // the only exception is within directives, which may reference arbitrary output object shapes
    // but we do not know those shapes here, so we cannot assist.
    // the only thing we can assist with is if the user is typing $.
    // then we can assume they will type one of the known prefixes
    if !json_path_query.segments.is_empty() {
        // we dont know the shape of anything beyond the initial known prefix
        return Some(vec![])
    }
    let completions = input_output_accum_name_completions();
    Some(completions)
}

pub fn input_output_accum_name_completions() -> Vec<CompletionItem> {
    let mut completions = Vec::with_capacity(4);
    completions.push(CompletionItem {
        label: "accum".to_string(),
        documentation: Some(Documentation::MarkupContent(MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: r#"templates can reference values from resources.
referencing `$.accum` reads from the accumulator json object during deploy.
the accumulator object is an ephemeral object that simply contains the accumulated output state
of the resource across all of the commands defined in the template the resource references.
the accumulator object is distinct from `$.output` because `$.output` is a static object that contains
the entire json object output from the last time the resource was deployed. this is used for templates
that need to reference previous values, while not affecting the current deployment's output.
every command in a template will merge its output into the accumulator. after the final command runs in a template,
the accumulator object becomes the output for this resource."#.to_string(),
        })),
        ..Default::default()
    });
    completions.push(CompletionItem {
        label: "input".to_string(),
        documentation: Some(Documentation::MarkupContent(MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: r#"templates can reference values from resources.
referencing `$.input` reads the input json object of the resource.
the input does not change during the course of a deploy."#.to_string(),
        })),
        ..Default::default()
    });
    completions.push(CompletionItem {
        label: "name".to_string(),
        documentation: Some(Documentation::MarkupContent(MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: r#"templates can reference values from resources.
referencing `$.name` simply reads the current resource's name.
this value does not change during the course of a deploy."#.to_string(),
        })),
        ..Default::default()
    });
    completions.push(CompletionItem {
        label: "output".to_string(),
        documentation: Some(Documentation::MarkupContent(MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: r#"templates can reference values from resources.
referencing `$.output` reads from the output json object from the **last** deploy.
this value is needed by templates that require referencing prior state to complete an update/delete.
note that this value does not change during the course of a deploy. if your template requires
referencing values between commands, that must be done via `$.accum` not `$.output`."#.to_string(),
        })),
        ..Default::default()
    });
    completions
}

pub fn handle_resource_jsonpath_completion_request<'a>(
    all_valid_sections: &Vec<Section<'a>>,
    found_section: &Section<'a>,
    json_path_query: jsonpath_rust::parser::model::JpQuery,
    original_pos: Position,
) -> Option<Vec<CompletionItem>> {
    // we're processing a json path within a resource section.
    // collect json object from all other resource sections:
    let mut dpl = deploy_language::DplFile::default();
    for section in all_valid_sections.iter() {
        if std::ptr::addr_eq(section, found_section) {
            // ignore the found section
            continue;
        }
        if section.typ == deploy_language::parse::resource::SECTION_TYPE {
            let _ = deploy_language::parse::resource::parse_resource_section(&mut dpl, section);
        } else if section.typ == deploy_language::parse::function::SECTION_TYPE {
            let _ = deploy_language::parse::function::parse_function_section(&mut dpl, section);
        } else if section.typ == deploy_language::parse::constant::SECTION_TYPE {
            let _ = deploy_language::parse::constant::parse_const_section(&mut dpl, section);
        }
    }
    let resource_name = match json_path_query.segments.first() {
        Some(name) if json_path_query.segments.len() == 1 => {
            // there's only one json path query segment, that means
            // its either a resource name, in which case recommend input/output/accum/name
            // or if its a function name, recommend other resource names that can be passed into the function
            // or if its a const name, recommend fields within that const
            let name_str = name.to_string();
            match dpl.functions.iter().find(|x| x.function_name.as_str() == &name_str) {
                Some(_) => {
                    // first is a function, return all resource names and const names
                    let names = dpl.resources.iter().map(|x| &x.resource_name.s);
                    let names = names.chain(dpl.constants.iter().map(|x| &x.const_name.s));
                    let completions = get_completion_from_keys(
                        original_pos,
                        names,
                    );
                    return Some(completions);
                }
                None => {
                    if dpl.resources.iter().find(|x| x.resource_name.as_str() == name_str).is_some() {
                        // first is a resource, return input/output/accum/name
                        return Some(input_output_accum_name_completions());
                    }
                }
            }
            // its a constant, fall through and recommend completions within the constant:
            name_str
        }
        None => {
            // if there's no segments, simply give recommendation of all the other resource names + function names + const names:
            let resource_names = dpl.resources.iter().map(|x| &x.resource_name.s);
            let function_names = dpl.functions.iter().map(|x| &x.function_name.s);
            let const_names = dpl.constants.iter().map(|x| &x.const_name.s);
            let names = resource_names.chain(function_names).chain(const_names);
            let completions = get_completion_from_keys(
                original_pos,
                names
            );
            return Some(completions);
        }
        // more than 1 segments. fall through to try to dynamically look up the input field
        // from the resource of interest
        Some(x) => {
            x.to_string()
        }
    };
    // it might be a constant, so check if its a constant first, and if so, recommend
    // completions within the constant:
    // check if its a const first:
    if let Some(cnst) = dpl.constants.iter().find(|x| x.const_name.as_str() == &resource_name) {
        // referencing a constant, provide completions for the constant
        let mut jpq = json_path_query.clone();
        // remove the name of the constant, such that the remaining segments are into the constant json val:
        jpq.segments.remove(0);
        return get_arbitrary_json_completion(&cnst.body, original_pos, jpq);
    }
    // now we know we're referencing a resource, check that the 2nd segment is "input"
    // otherwise, we cannot give completions for output/accum because we're not looking up the state to
    // be able to access their fields
    // TODO: in the future might wish to offer completions for output! can be very useful when the state is a local file...
    //
    // ensure the first segment is referencing a valid resource:
    let resource = dpl.resources.iter().find(|x| x.resource_name.s == resource_name)?;
    // ensure the 2nd segment is referencing input
    let second_segment = json_path_query.segments.get(1)?;
    if second_segment.to_string() != "input" {
        return None;
    }
    // now simply do a json path lookup using the remaining segments
    // on the input of this resource
    let remaining_segments = json_path_query.segments.get(2..)?.to_vec();
    let jpq = jsonpath_rust::parser::model::JpQuery { segments: remaining_segments };
    return get_arbitrary_json_completion(&resource.input, original_pos, jpq);
}

pub fn get_arbitrary_json_completion(
    val: &json_with_positions::Value,
    original_pos: Position,
    jpq: jsonpath_rust::parser::model::JpQuery,
) -> Option<Vec<CompletionItem>> {
    // need to convert to a serde json value. any json paths simply replace with null
    let resource_input_val = val.to_serde_json_value_with_replace_func(&mut |_| {
        Ok(serde_json::Value::Null)
    }).ok()?;
    let mut res = jsonpath_rust::query::js_path_process(
        &jpq, &resource_input_val
    ).ok()?;
    let last = res.pop()?;
    let val = last.val();
    let map = val.as_object()?;
    // return all keys of the object as valid completion items:
    let completions = get_completion_from_keys(original_pos, map.keys());
    Some(completions)
}

pub fn get_completion_from_keys<'a>(
    original_pos: Position,
    keys: impl Iterator<Item = &'a String>,
) -> Vec<CompletionItem> {
    let mut completions = vec![];
    for key in keys {
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
            label: key.to_string(),
            text_edit: completion_text_edit,
            additional_text_edits,
            ..Default::default()
        });
    }
    completions
}

pub fn handle_completion_request<'a>(
    params: CompletionParams,
    doc: &ParsedDoc<'a>,
) -> Option<serde_json::Value> {
    let completion_items = handle_completion_request_ex(params, doc)?;
    serde_json::to_value(completion_items).ok()
}

pub fn handle_hover_request<'a>(
    params: HoverParams,
    doc: &ParsedDoc<'a>,
) -> Option<Hover> {
    let Position { line, character } = params.text_document_position_params.position;
    let line_index = line as usize;
    // remember a character position is really between two characters:
    // its the position of the cursor. so column 1 means user typed a char
    // at the start of the line, and their cursor is currently after that first character
    // so for us to look up the character they just typed, we'd need to get the character at index 0
    let column = character as usize;
    // get the section that contains the hover pos
    let section = doc.parsed.iter().find(|x| line_index >= x.start_line && line_index <= x.end_line)?;
    match section.typ.s {
        deploy_language::parse::template::SECTION_TYPE => {
            let body_line = section.body.iter().find(|x| x.line == line_index)?;
            let word = body_line.split_ascii_whitespace().into_iter().find(|x| {
                let word_start = x.col;
                if column < word_start {
                    return false;
                }
                let word_end = word_start + x.s.chars().count();
                column >= word_start && column <= word_end
            })?;
            if word.s.starts_with('@') {
                let (_, directive_type) = word.s.split_once("@")?;
                let value = match directive_type.trim() {
                    "dropoutput" => r#"causes the command's output to be ignored entirely and not merged into the accumulator.
this directive doesnt take any args. it should just be the directive keyword eg:
```text
@dropoutput
```"#,
                    "accum" => r#"insert a value into the accumulator after this command runs. the accum_path can leave out the $.accum since it's implied
the value is going into the accumulator. the src_path can be a json path starting with $.input $.name $.accum or $.output.
the syntax is a json array with 2 values. the first is the src_path of where to read from
and the 2nd is the accum_path which is where the value will be inserted to
eg
```text
@accum [$.accum.somefield, $.nested.fieldvalue]
```
the above example would run the command, merge into the accumulator,
then after it merges, it reads the value of the accumulator "somefield"
and it inserts that value again into the accumulator but at a different path "nested.fieldvalue"
                    "#,
                    "diff" => r#"only relevant to update commands: a diff directive
requires that the value the query resolves to must be different
than the same value from the last time this resource was transitioned.
authors can specify multiple queries in one directives optionally by placing them in a json array
such as `[$.a, $.b]`, which are ANDed together.
multiple diff and same directives are ORed together
eg if an author specifies 

```text
@diff [$.a, $.b]
@diff $.c
```
then the command below this directive will be ran if the value of $.c is different from prev and current
OR if the value of $.a AND the value of $.b is different from prev and current
                    "#,
                    "same" => r#"only relevant to update commands: a same directitve
requires that the value the query resolves to must be the same
as the value from the last time this resource was transitioned.
authors can specify multiple quries, which are ANDed together.
multiple diff and same directives are ORed together
eg if an author specifies

```text
@same [$.a, $.b]
@same $.c
```

then the command below this directive will be ran if the value of $.c is the same in prev and current
OR if the value of $.a AND the value of $.b is the same in prev and current
                    "#,
                    _ => return None,
                }.to_string();
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: lsp_types::MarkupKind::Markdown,
                        value: value,
                    }),
                    range: None,
                });
            }
            None
        }
        // other section types not supported for now
        _ => None,
    }
}

#[derive(Clone, Debug)]
pub struct ParsedDoc<'a> {
    pub doc: &'a String,
    pub parsed: Vec<Section<'a>>,
}


macro_rules! get_parsed_doc {
    ($parsed_docs: ident, $connection: ident, $uri: ident, $known_docs: ident) => {
        {
            let parsed = $parsed_docs.get($uri);
            let parsed = if let Some(parsed) = parsed {
                parsed
            } else {
                match $known_docs.get(&$uri) {
                    Some(s) => {
                        let (_, valid_sections) = split_sections(s, &$connection);
                        $parsed_docs.insert($uri.clone(), ParsedDoc { parsed: valid_sections, doc: s });
                    }
                    None => continue
                };
                match $parsed_docs.get(&$uri) {
                    Some(parsed) => parsed,
                    None => continue
                }
            };
            parsed
        }
    };
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
                    let parsed = get_parsed_doc!(parsed_docs, connection, uri, known_docs);
                    let result = handle_completion_request(params, parsed);
                    let result = if result.is_none() {
                        Some(serde_json::Value::Null)
                    } else { result };
                    send_response(&connection, Response { id: request.id, result, error: None });
                    continue;
                }
                if request.method == "textDocument/hover" {
                    let params: HoverParams = serde_json::from_value(request.params).unwrap();
                    let uri = &params.text_document_position_params.text_document.uri;
                    // params.work_done_progress_params.work_done_token.unwrap()
                    let parsed = get_parsed_doc!(parsed_docs, connection, uri, known_docs);
                    let result = handle_hover_request(params, parsed);
                    let result = match result.and_then(|x| serde_json::to_value(x).ok()) {
                        Some(x) => Some(x),
                        None => Some(serde_json::Value::Null),
                    };
                    send_response(&connection, Response { id: request.id, result, error: None });
                    continue;
                }
            }
            Message::Response(response) => {
                send_log(&connection, &format!("unexpected response received: {:?}", response));
            }
            Message::Notification(notif) => {
                if notif.method == "textDocument/didChange" || notif.method == "textDocument/didOpen" {
                    let (uri, new_document) = match notif.method.as_str() {
                        "textDocument/didChange" => {
                            let params: DidChangeTextDocumentParams = serde_json::from_value(notif.params).unwrap();
                            (params.text_document.uri, params.content_changes.first().map(|x| x.text.clone()).unwrap_or_default())
                        }
                        "textDocument/didOpen" => {
                            let params: DidOpenTextDocumentParams = serde_json::from_value(notif.params).unwrap();
                            (params.text_document.uri, params.text_document.text)
                        }
                        _ => {
                            continue;
                        }
                    };
                    parsed_docs = HashMap::new();
                    known_docs.insert(uri.clone(), new_document.to_string());
                    let diagnostics = if let Some(s) = known_docs.get(&uri) {
                        let (mut diagnostics, valid_sections) = split_sections(s, &connection);
                        check_dpl_file(&mut diagnostics, &valid_sections, &connection);
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
                    continue;
                }
                send_log(&connection, &format!("received unexpected notification: {}", notif.method));
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

    fn hover_params_with_position(
        line: u32,
        column: u32
    ) -> HoverParams {
        HoverParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: Url::parse("https://example.net").unwrap() },
                position: Position { line, character: column }
            },
            work_done_progress_params: WorkDoneProgressParams { work_done_token: None },
        }
    }

    #[test]
    fn can_provide_completions_for_json_paths() {
        let completion_request_line = 1;
        let completion_request_column = 21;
        let other_completion_column = 36;
        let other_input_completion_column = 42;
        // $. at column 21
        // $.other_resource. at column 36
        // #.other_resource.input. at column 42
        let document = r#"resource some_template(a)
   {"a": "b", "c": $.other_resource.input.}

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

        // the column is at $.other_resource. so it should recommend input/output/name/accum
        let params = completion_params_with_position(completion_request_line, other_completion_column);
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 4);
        res.sort_by(|a, b| a.label.cmp(&b.label));
        assert_eq!(res.remove(0).label, "accum");
        assert_eq!(res.remove(0).label, "input");
        assert_eq!(res.remove(0).label, "name");
        assert_eq!(res.remove(0).label, "output");

        // the column is at $.other_resource.input.
        // it should give option1/option2
        let params = completion_params_with_position(completion_request_line, other_input_completion_column);
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 2);
        res.sort_by(|a, b| a.label.cmp(&b.label));
        assert_eq!(res.remove(0).label, "option1");
        assert_eq!(res.remove(0).label, "option2");
    }

    #[test]
    fn can_provide_completions_for_resources_referencing_functions() {
        let completion_request_line = 1;
        let completion_request_col = 14;
        // $. at column 14
        let document = r#"resource some_template(a)
   {"func": $.}

function javascript(myfunc)
  console.log(1);

function javascript(other_func)
  console.log(1);
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_col);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $. so it should recommend resource names and function names, of which theres only 2 function names:
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 2);
        assert_eq!(res.remove(0).label, "myfunc");
        assert_eq!(res.remove(0).label, "other_func");
    }

    #[test]
    fn can_provide_completions_for_resources_referencing_constants() {
        let completion_request_line = 1;
        let completion_request_col = 15;
        // $. at column 15
        let document = r#"resource some_template(a)
   {"const": $.}

const a
    {}

const b
    {}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_col);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $. so it should recommend resource names and function names, of which theres only 2 function names:
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 2);
        assert_eq!(res.remove(0).label, "a");
        assert_eq!(res.remove(0).label, "b");
    }

    #[test]
    fn can_provide_completions_for_const_fields() {
        let completion_request_line = 1;
        let completion_request_col = 17;
        // $.a. at column 17
        let document = r#"resource some_template(a)
   {"const": $.a.}

const a
    {"b": "e", "c": 1}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_col);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $.a. so it should recommend
        // the values within the "a" constant (it has fields "b" and "c")
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        res.sort_by(|a, b| a.label.cmp(&b.label));
        assert_eq!(res.len(), 2);
        assert_eq!(res.remove(0).label, "b");
        assert_eq!(res.remove(0).label, "c");
    }

    #[test]
    fn can_provide_completions_for_const_fields_deep() {
        let completion_request_line = 1;
        let completion_request_col = 21;
        // $.a.b.c. at column 21
        let document = r#"resource some_template(a)
   {"const": $.a.b.c.}

const a
    {
        "b": {
            "c": { "e": 1, "d": false, "f": [] }
        }
    }
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_col);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $.a.b.c. so it should recommend
        // the values within the "a" constant deeply
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 3);
        res.sort_by(|a, b| a.label.cmp(&b.label));
        assert_eq!(res.remove(0).label, "d");
        assert_eq!(res.remove(0).label, "e");
        assert_eq!(res.remove(0).label, "f");
    }

    #[test]
    fn can_provide_completions_for_functions_being_passed_resource_names() {
        let completion_request_line = 1;
        let completion_request_col = 21;
        // $. at column 21
        let document = r#"resource some_template(a)
   {"func": $.myfunc.}

function javascript(myfunc)
  console.log(1);

function javascript(other_func)
  console.log(1);

resource some_template(other_resource1)
    {}

resource some_template(other_resource2)
    {}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_col);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $.myfunc. so it should recommend resource names
        // that this function can call. it should not recommend resource 'a' since that is where the function call is coming from
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 2);
        assert_eq!(res.remove(0).label, "other_resource1");
        assert_eq!(res.remove(0).label, "other_resource2");
    }

    #[test]
    fn can_provide_completions_for_functions_being_passed_const_names() {
        let completion_request_line = 1;
        let completion_request_col = 21;
        // $. at column 21
        let document = r#"resource some_template(a)
   {"func": $.myfunc.}

function javascript(myfunc)
  console.log(1);

function javascript(other_func)
  console.log(1);

const myconst1
    {}

const other_const
  "e"
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_col);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $.myfunc. so it should recommend const names
        // that this function can call.
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 2);
        assert_eq!(res.remove(0).label, "myconst1");
        assert_eq!(res.remove(0).label, "other_const");
    }

    #[test]
    fn dont_provide_completions_for_function_calls_with_already_2_segments() {
        let completion_request_line = 1;
        let completion_request_col = 32;
        // $. at column 32
        let document = r#"resource some_template(a)
   {"func": $.myfunc.other_func.}

function javascript(myfunc)
  console.log(1);

function javascript(other_func)
  console.log(1);

resource some_template(other_resource1)
    {}

resource some_template(other_resource2)
    {}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, completion_request_col);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // the column is at the $.myfunc. so it should recommend resource names
        // that this function can call. it should not recommend resource 'a' since that is where the function call is coming from
        let res = handle_completion_request_ex(params, &parsed);
        assert!(res.is_none());
    }

    #[test]
    fn should_not_provide_completions_for_json_path_output() {
        let completion_request_line = 1;
        let other_output_completion_column = 43;
        // #.other_resource.output. at column 43
        let document = r#"resource some_template(a)
   {"a": "b", "c": $.other_resource.output.}

resource some_template(other_resource)
  {"option1": null, "option2": null}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, other_output_completion_column);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // we cannot give completion items for output of a resource since it may not exist/state
        // may not necessarily be local
        let res = handle_completion_request_ex(params, &parsed);
        assert!(res.is_none());
    }

    #[test]
    fn should_not_provide_completions_for_json_path_accum() {
        let completion_request_line = 1;
        let other_accum_completion_column = 42;
        // #.other_resource.accum. at column 42
        let document = r#"resource some_template(a)
   {"a": "b", "c": $.other_resource.accum.}

resource some_template(other_resource)
  {"option1": null, "option2": null}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(completion_request_line, other_accum_completion_column);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // we cannot give completion items for accum of a resource since its ephemeral. only exists during deploy
        let res = handle_completion_request_ex(params, &parsed);
        assert!(res.is_none());
    }

    #[test]
    fn json_path_completions_use_bracketed_syntax_if_theres_a_space() {
        // $.other_resource. at column 42
        let document = r#"resource some_template(a)
   {"a": "b", "c": $.other_resource.input.}

resource some_template(other_resource)
  {"opti on1": null}
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(1, 42);
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

    #[test]
    fn template_jsonpath_completions_should_be_root_keywords_only() {
        let document = r#"
template something
  create
    # some comment
    @accum [$.input, $.something]
    echo hello
      ... $.

resource something(abc)
    {}
"#.to_string();

        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(6, 12);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        let mut res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 4);
        res.sort_by(|a, b| a.label.cmp(&b.label));
        assert_eq!(&res[0].label, "accum");
        assert_eq!(&res[1].label, "input");
        assert_eq!(&res[2].label, "name");
        assert_eq!(&res[3].label, "output");
    }

    #[test]
    fn shouldnt_give_jsonpath_completions_for_templates_deeper_than_root() {
        let document = r#"
template something
  create
    @accum [$.input, $.something]
    echo hello
      # user already typed $.input.thing, we dont know the shape of "thing" so shouldnt give completions
      ... $.input.thing.

resource something(abc)
    {}
"#.to_string();

        let (_, sections) = split_sections_with_logger(&document, ());
        let params = completion_params_with_position(6, 24);
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        let res = handle_completion_request_ex(params, &parsed).unwrap();
        assert_eq!(res.len(), 0);
    }

    #[test]
    fn can_give_hover_hints_for_directives() {
        let document = r#"
template ewqew
  create
    @dropoutput # comment
    echo
"#.to_string();
        let (_, sections) = split_sections_with_logger(&document, ());
        let expected_hint = "causes the command's output to be ignored entirely";
        let expected_range = 0..expected_hint.len();
        let parsed = ParsedDoc {
            doc: &document,
            parsed: sections,
        };
        // its before the @ so it shouldnt give anything
        let params = hover_params_with_position(3, 3);
        let x = handle_hover_request(params, &parsed);
        assert!(x.is_none());

        // the @ character should give a hint
        let params = hover_params_with_position(3, 4);
        let x = handle_hover_request(params, &parsed).unwrap();
        let markup = match x.contents {
            HoverContents::Markup(markup_content) => markup_content,
            _ => panic!("wrong hover content type")
        };
        assert_eq!(markup.value.get(expected_range.clone()).unwrap(), expected_hint);

        // in the middle of the directive should give a hint
        let params = hover_params_with_position(3, 9);
        let x = handle_hover_request(params, &parsed).unwrap();
        let markup = match x.contents {
            HoverContents::Markup(markup_content) => markup_content,
            _ => panic!("wrong hover content type")
        };
        assert_eq!(markup.value.get(expected_range.clone()).unwrap(), expected_hint);

        // the last character should still give a hint
        let params = hover_params_with_position(3, 15);
        let x = handle_hover_request(params, &parsed).unwrap();
        let markup = match x.contents {
            HoverContents::Markup(markup_content) => markup_content,
            _ => panic!("wrong hover content type")
        };
        assert_eq!(markup.value.get(expected_range.clone()).unwrap(), expected_hint);

        // the space between the directive and the comment shouldnt give any hint
        let params = hover_params_with_position(3, 16);
        let x = handle_hover_request(params, &parsed);
        assert!(x.is_none());
    }
}
