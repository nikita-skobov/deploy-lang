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
                let diag = Diagnostic {
                    range: Range {
                        start: Position { line: 0, character: 0 },
                        end: Position { line: 0, character: 1 },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("my-lsp".into()),
                    message: "This is a test diagnostic".into(),
                    related_information: None,
                    tags: None,
                    data: None,
                };
                let diags = vec![diag];

                let params = PublishDiagnosticsParams {
                    uri,
                    diagnostics: diags,
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
