use std::{error::Error, io::Write};

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
    let mut f = std::fs::File::options()
        .append(true)
        .create(true)
        .write(true)
        .open("/tmp/serverlog2.txt").unwrap();
    f.write_all(b"we starty?\n").unwrap();


    let (connection, io_thread) = Connection::stdio();

    // read initialize request
    let (id, params) =
        connection.initialize_start().unwrap();
    let formatted = format!("got client params: {:?}\n", params);
    f.write_all(formatted.as_bytes()).unwrap();

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
    let formatted = format!("sending server capabilities to client: {:?}\n", init_result);
    f.write_all(formatted.as_bytes()).unwrap();
    connection.initialize_finish(id, init_result).unwrap();
    f.write_all(b"finished initialization!\n").unwrap();

    // now you can start your main loop
    main_loop(connection, params, f).unwrap();
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
    mut f: std::fs::File,
) -> std::result::Result<(), Box<dyn Error + Sync + Send>> {
    let _init: InitializeParams = serde_json::from_value(params)?;
    f.write_all(b"in main loopy\n").unwrap();
    for msg in &connection.receiver {
        let formatted = format!("got message: {:?}\n", msg);
        f.write_all(formatted.as_bytes()).unwrap();
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
            }
        }
    }
    Ok(())
}
