use std::error::Error;

use lsp_server::{Connection, Message};
use lsp_types::notification::Notification as _;
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

/// given the current document, generate all diagnostics that should be emitted.
/// this represents the current "state" of the diagnostics. whatever the server returns
/// here, is what the client will show, so if the user previously had diagnostic errors,
/// then they fixed them, this should return an empty vec of diagnostics, causing the client
/// to remove past errors
fn generate_diagnostics_from_current_document<'a>(document: &'a str) -> Vec<Diagnostic> {
    let sections = dcl_language::parse_document_to_sections(document);
    let mut out = vec![];
    for section in sections {
        if let Err(e) = section {
            let diag = Diagnostic {
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
            };
            out.push(diag);
        }
    }
    out
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> std::result::Result<(), Box<dyn Error + Sync + Send>> {
    let _init: InitializeParams = serde_json::from_value(params)?;
    for msg in &connection.receiver {
        if let Message::Notification(notif) = msg {
            if notif.method == "textDocument/didChange" {
                let params: DidChangeTextDocumentParams = serde_json::from_value(notif.params).unwrap();
                let uri = params.text_document.uri;
                let new_document = params.content_changes.first().map(|x| x.text.as_str()).unwrap_or_default();
                let diagnostics = generate_diagnostics_from_current_document(new_document);
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
            }
        }
    }
    Ok(())
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
