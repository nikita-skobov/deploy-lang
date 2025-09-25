use dcl_language::parse::SpannedDiagnostic;

fn main() {
    let dcl_file_path = std::env::args().nth(1).expect("must provide path to a .dcl file");
    let dcl_file = std::fs::read_to_string(&dcl_file_path)
        .map_err(|e| format!("Failed to open and read file'{}': {:?}", dcl_file_path, e))
        .unwrap();
    if let Err(diagnostics) = main_or_err(dcl_file) {
        let error_s = if diagnostics.len() == 1 { "error" } else { "errors" };
        eprintln!("Found {} {} in {}:", diagnostics.len(), error_s, dcl_file_path);
        for err in diagnostics {
            eprintln!("Line {}:", err.span.start.line);
            eprintln!("{}\n", err.message);
        }
        std::process::exit(1);
    }
}

fn main_or_err(
    dcl_file: String,
) -> Result<(), Vec<SpannedDiagnostic>> {
    let sections = dcl_language::parse::parse_document_to_sections(&dcl_file);
    let mut filtered_sections = Vec::with_capacity(sections.len());
    let mut err_diagnostics = Vec::with_capacity(sections.len());
    for section in sections {
        match section {
            Ok(s) => filtered_sections.push(s),
            Err(e) => err_diagnostics.push(e),
        }
    }
    // error early if parsing failures:
    if !err_diagnostics.is_empty() {
        return Err(err_diagnostics);
    }

    let dcl = dcl_language::parse::sections_to_dcl_file(filtered_sections)
        .map_err(|e| vec![e])?;

    cli_engine::load_state(&dcl).map_err(|e| vec![e])?;
    println!("{:#?}", dcl);
    Ok(())
}
