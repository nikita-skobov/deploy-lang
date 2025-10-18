use deploy_language::parse::SpannedDiagnostic;
use simple_logger::SimpleLogger;

#[tokio::main]
async fn main() {
    let dpl_file_path = std::env::args().nth(1).expect("must provide path to a .dpl file");
    let dpl_file = std::fs::read_to_string(&dpl_file_path)
        .map_err(|e| format!("Failed to open and read file'{}': {:?}", dpl_file_path, e))
        .unwrap();
    if let Err(diagnostics) = main_or_err(dpl_file).await {
        let error_s = if diagnostics.len() == 1 { "error" } else { "errors" };
        eprintln!("Found {} {} in {}:", diagnostics.len(), error_s, dpl_file_path);
        for err in diagnostics {
            eprintln!("Line {}:", err.span.start.line);
            eprintln!("{}\n", err.message);
        }
        std::process::exit(1);
    }
}

async fn main_or_err(
    dpl_file: String,
) -> Result<(), Vec<SpannedDiagnostic>> {
    SimpleLogger::new().init().map_err(|e| {
        vec![SpannedDiagnostic::new(format!("failed to initiate logger: {:?}", e), 0, 0)]
    })?;
    let dpl = deploy_language::parse_and_validate(dpl_file)?;
    let (state, state_file_path) = cli_engine::load_state(&dpl).map_err(|e| vec![e])?;
    let logger = log::logger();
    let state = cli_engine::perform_update(logger, dpl, state).await
        .map_err(|e| vec![SpannedDiagnostic::new(e, 0, 0)])?;
    cli_engine::save_state(&state_file_path, &state)
        .map_err(|e| vec![SpannedDiagnostic::new(e, 0, 0)])
}
