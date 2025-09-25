use dcl_language::{parse::SpannedDiagnostic, DclFile};

// TODO: need tp update DclFile definitions to preserve section line positions.
// current reporting all errors on line 0 :/
pub fn load_state(dcl: &DclFile) -> Result<serde_json::Value, SpannedDiagnostic> {
    let state = dcl.state.as_ref().ok_or("no state file provided")
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
    let file = std::fs::OpenOptions::new().read(true).open(&state.file)
        .map_err(|e| format!("Failed to read state file '{}': {:?}", state.file, e))
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
    let buf = std::io::BufReader::new(file);
    let out: serde_json::Value = serde_json::from_reader(buf)
        .map_err(|e| format!("Failed to read state file '{}' as json: {:?}", state.file, e))
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
    Ok(out)
}
