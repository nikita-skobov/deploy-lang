fn main() {
    let dcl_file_path = std::env::args().nth(1).expect("must provide path to a .dcl file");
    let dcl_file = std::fs::read_to_string(&dcl_file_path)
        .map_err(|e| format!("Failed to open and read file'{}': {:?}", dcl_file_path, e))
        .unwrap();
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
        let error_s = if err_diagnostics.len() == 1 { "error" } else { "errors" };
        eprintln!("Found {} {} in {}:", err_diagnostics.len(), error_s, dcl_file_path);
        for err in err_diagnostics {
            eprintln!("Line {}:", err.span.start.line);
            eprintln!("{}\n", err.message);
        }
        std::process::exit(1);
    }

    let dcl = match dcl_language::parse::sections_to_dcl_file(filtered_sections) {
        Ok(o) => o,
        Err(e) => {
            eprintln!("Found 1 error in {}:", dcl_file_path);
            eprintln!("Line {}:", e.span.start.line);
            eprintln!("{}\n", e.message);
            std::process::exit(1);
        }
    };
    println!("{:#?}", dcl);
}
