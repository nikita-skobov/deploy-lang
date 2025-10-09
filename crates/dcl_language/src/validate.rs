//! module for the various validations that should happen to a DclFile once it has been successfully parsed

use crate::{parse::{resource::ResourceSection, SpannedDiagnostic}, DclFile};

pub fn validate_dcl_file(dcl: &DclFile) -> Vec<SpannedDiagnostic> {
    let mut diagnostics = vec![];
    validate_resources(dcl, &mut diagnostics);
    diagnostics
}

pub fn validate_resources(dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    for resource in dcl.resources.iter() {
        resource_has_corresponding_template(resource, dcl, diagnostics);
    }
}

pub fn resource_has_corresponding_template(resource: &ResourceSection, dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    if !dcl.templates.iter().any(|x| resource.template_name == x.template_name) {
        diagnostics.push(SpannedDiagnostic::from_str_at_line(
            &resource.template_name,
            format!(
                    "template '{}' not found (referenced by resource '{}')",
                    resource.template_name,
                    resource.resource_name
                ),
            ));
    }
}

// pub fn every_json_path_query_references_valid_path(dcl: &DclFile) -> Result<(), SpannedDiagnostic> {
//     for resource in dcl.resources.iter() {
//         // resource.input
//     }
//     Ok(())
// }

#[cfg(test)]
mod test {
    use crate::parse_and_validate;

    use super::*;

    /// extracts a diagnostics into a tuple containing
    /// (the message, the source string that it came from)
    fn extract_source(diag: SpannedDiagnostic, src: &str) -> (String, String) {
        let mut lines: Vec<_> = src.lines().collect();
        if diag.span.start.line == diag.span.end.line {
            let line = lines.remove(diag.span.start.line);
            let mut out_s = String::new();
            for (i, c) in line.chars().enumerate() {
                if i >= diag.span.start.column && i < diag.span.end.column {
                    out_s.push(c);
                }
            }
            (diag.message, out_s)
        } else {
            let mut out_s = String::new();
            for (i, line) in lines.iter().enumerate() {
                if i == diag.span.start.line {
                    for (j, c) in line.chars().enumerate() {
                        if j >= diag.span.start.column {
                            out_s.push(c);
                        }
                    }
                    out_s.push('\n');
                } else if i > diag.span.start.line && i < diag.span.end.line {
                    out_s.push_str(&line);
                    out_s.push('\n');
                } else if i == diag.span.end.line {
                    for (j, c) in line.chars().enumerate() {
                        if j < diag.span.end.column {
                            out_s.push(c);
                        }
                    }
                }
            }
            (diag.message, out_s)
        }
    }

    fn extract_sources(mut diagnostics: Vec<SpannedDiagnostic>, src: &str) -> Vec<String> {
        diagnostics.drain(..).map(|diag| {
            let out = extract_source(diag, src);
            out.1
        }).collect::<Vec<_>>()
    }

    #[test]
    fn invalid_template_reference_works() {
        let file = r#"
resource nonexistant_template(my_resource)
    {}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let source_errors = extract_sources(diagnostics, file);
        assert_eq!(source_errors, vec![
            "nonexistant_template"
        ]);
    }

    #[test]
    fn invalid_json_works() {
        let file = r#"
resource nonexistant_template(my_resource)
    {
        "a
    }
    # not included in the error
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let source_errors = extract_sources(diagnostics, file);
        assert_eq!(source_errors, vec![
            "    {\n        \"a\n    }"
        ]);
    }
}
