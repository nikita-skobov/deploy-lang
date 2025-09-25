use dcl_language::{parse::SpannedDiagnostic, DclFile};

// TODO: need tp update DclFile definitions to preserve section line positions.
// current reporting all errors on line 0 :/
/// loads the state file and parses as a json object. if the state file doesnt exist, it will be
/// created, and treated as an empty {} object
pub fn load_state(dcl: &DclFile) -> Result<serde_json::Value, SpannedDiagnostic> {
    let state = dcl.state.as_ref().ok_or("no state file provided")
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
    let file = match std::fs::OpenOptions::new().read(true).open(&state.file) {
        Ok(o) => o,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => {
                // create a new empty state file:
                std::fs::write(&state.file, "{}")
                    .map_err(|e| SpannedDiagnostic::new(
                        format!("Failed to create empty state file '{}': {:?}", state.file, e),
                        0, 999)
                    )?;
                return Ok(serde_json::Value::Object(Default::default()));
            },
            e => {
                return Err(SpannedDiagnostic::new(
                    format!("Failed to read state file '{}': {:?}", state.file, e),
                    0, 999
                ));
            }
        }
    };
    let buf = std::io::BufReader::new(file);
    let out: serde_json::Value = serde_json::from_reader(buf)
        .map_err(|e| format!("Failed to read state file '{}' as json: {:?}", state.file, e))
        .map_err(|e| SpannedDiagnostic::new(e, 0, 999))?;
    Ok(out)
}
