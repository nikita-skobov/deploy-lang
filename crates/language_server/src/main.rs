use std::collections::HashMap;
use std::error::Error;

use dcl_language::parse::{Logger, SpannedDiagnostic};
use lsp_server::{Connection, Message, Response};
use lsp_types::notification::Notification as _;
use lsp_types::{CompletionOptions, CompletionParams, Url};
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

/// given the current document, generate all diagnostics that should be emitted.
/// this represents the current "state" of the diagnostics. whatever the server returns
/// here, is what the client will show, so if the user previously had diagnostic errors,
/// then they fixed them, this should return an empty vec of diagnostics, causing the client
/// to remove past errors
fn generate_diagnostics_from_current_document<'a>(document: &'a str, connection: &Connection) -> Vec<Diagnostic> {
    let clg = CodeLogger { connection };
    let sections = dcl_language::parse::parse_document_to_sections_with_logger(document, clg.clone());
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
    // now with the valid sections, try to parse/validate as a Dcl file:
    // TODO: sections_to_dcl_file should support multiple diagnostics, not just 1 error
    if let Err(e) = dcl_language::parse::sections_to_dcl_file_with_logger(valid_sections, clg) {
        out.push(spanned_diag_to_lsp_diag(e));
    }
    out
}

pub fn handle_completion_request<'a>(
    params: CompletionParams,
    doc: Option<&'a String>,
) -> Option<serde_json::Value> {
    let doc = doc?;
    let Position { line, character } = params.text_document_position.position;
    let line = line as usize;
    let column = character as usize;

    None
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> std::result::Result<(), Box<dyn Error + Sync + Send>> {
    let mut known_docs: HashMap<Url, String> = HashMap::new();
    let _init: InitializeParams = serde_json::from_value(params)?;
    for msg in &connection.receiver {
        match msg {
            Message::Request(request) => {
                send_log(&connection, &format!("received request: {:?}", request));
                if request.method == "textDocument/completion" {
                    let params: CompletionParams = serde_json::from_value(request.params).unwrap();
                    let doc = known_docs.get(&params.text_document_position.text_document.uri);
                    let result = handle_completion_request(params, doc);
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
                    known_docs.insert(uri.clone(), new_document.to_string());
                    let diagnostics = generate_diagnostics_from_current_document(new_document, &connection);
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
