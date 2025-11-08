use std::path::PathBuf;

use deploy_language::parse::SpannedDiagnostic;
use simple_logger::SimpleLogger;

#[tokio::main]
async fn main() {
    let dpl_file_path = std::env::args().nth(1).expect("must provide path to a .dpl file");
    let dpl_file = std::fs::read_to_string(&dpl_file_path)
        .map_err(|e| format!("Failed to open and read file'{}': {:?}", dpl_file_path, e))
        .unwrap();
    // TODO: allow users to disable replay or customize file path
    let replay_file = get_replay_file(&dpl_file_path);
    replays::start_replay_log();
    replays::set_replay_out_file(replay_file);
    let result = main_or_err(dpl_file).await;
    replays::finish_replay();
    if let Err(diagnostics) = result {
        let error_s = if diagnostics.len() == 1 { "error" } else { "errors" };
        eprintln!("Found {} {} in {}:", diagnostics.len(), error_s, dpl_file_path);
        for err in diagnostics {
            eprintln!("Line {}:", err.span.start.line);
            eprintln!("{}\n", err.message);
        }
        std::process::exit(1);
    }
}

fn get_replay_file(dpl_file_path: &String) -> PathBuf {
    let mut dpl_file_p = PathBuf::from(dpl_file_path);
    let replay_file = dpl_file_p.file_stem().expect("provided file path is a directory");
    let mut file_name = replay_file.to_string_lossy().to_string();
    file_name.push_str("-replay.jsonl");
    dpl_file_p.pop();
    dpl_file_p.push(file_name);
    dpl_file_p
}

async fn main_or_err(
    dpl_file: String,
) -> Result<(), Vec<SpannedDiagnostic>> {
    SimpleLogger::new().init().map_err(|e| {
        vec![SpannedDiagnostic::new(format!("failed to initiate logger: {:?}", e), 0, 0)]
    })?;
    let dpl = deploy_language::parse_and_validate(dpl_file)?;
    // TODO: user option to prevent replay logging if undesired
    for resource in dpl.resources.iter() {
        let input_json = resource.input.clone().to_serde_json_value();
        replays::resource_read(&resource.resource_name.s, input_json);
    }
    let (state, state_file_path) = cli_engine::load_state(&dpl).map_err(|e| vec![e])?;
    let logger = log::logger();
    let state = cli_engine::perform_update(logger, dpl, state).await
        .map_err(|e| vec![SpannedDiagnostic::new(e, 0, 0)])?;
    cli_engine::save_state(&state_file_path, &state)
        .map_err(|e| vec![SpannedDiagnostic::new(e, 0, 0)])
}
