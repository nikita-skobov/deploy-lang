//! module for the various validations that should happen to a DclFile once it has been successfully parsed

use jsonpath_rust::query::{state::State, Query};

use crate::{parse::{resource::ResourceSection, SpannedDiagnostic}, DclFile};

pub fn validate_dcl_file(dcl: &DclFile) -> Vec<SpannedDiagnostic> {
    let mut diagnostics = vec![];
    validate_resources(dcl, &mut diagnostics);
    diagnostics
}

pub fn validate_resources(dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    for resource in dcl.resources.iter() {
        resource_has_corresponding_template(resource, dcl, diagnostics);
        resource_has_valid_jsonpath_references(resource, dcl, diagnostics);
    }
}

pub fn resource_has_corresponding_template(resource: &ResourceSection, dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    if !dcl.templates.iter().any(|x| resource.template_name.s == x.template_name.s) {
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

pub fn resource_has_valid_jsonpath_references(resource: &ResourceSection, dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    let jsonpaths = resource.input.get_all_json_paths();
    for jp in jsonpaths {
        let mut jpq = match jsonpath_rust::parser::parse_json_path(&jp.s) {
            Ok(o) => o,
            Err(e) => {
                diagnostics.push(SpannedDiagnostic::from_str_at_line(jp.clone(), format!("failed to parse json path query '{}' for resource '{}': {:?}", jp.as_str(), resource.resource_name.as_str(), e)));
                continue;
            }
        };
        // cannot be empty:
        if jpq.segments.is_empty() {
            diagnostics.push(SpannedDiagnostic::from_str_at_line(jp.clone(), format!("resource '{}' cannot have empty json path query '{}'", resource.resource_name.as_str(), jp.as_str())));
            continue;
        }
        let first_segment = jpq.segments.remove(0);
        // must be a valid resource:
        let referenced_resource_name = first_segment.to_string();
        let referenced_resource = match dcl.resources.iter().find(|r| r.resource_name.as_str() == referenced_resource_name.as_str()) {
            Some(r) => r,
            None => {
                diagnostics.push(SpannedDiagnostic::from_str_at_line(jp.clone(), format!("resource '{}' references a non-existant resource '{}'", resource.resource_name.as_str(), referenced_resource_name)));
                continue;
            }
        };
        // must have one of input/output/accum/name
        if jpq.segments.is_empty() {
            // for now, allow it to reference a resource entirely
            // TODO: but perhaps we should not allow this in the future? as in it must
            // reference one of $.resource.name|output|accum|input
            continue;
        }
        let next = jpq.segments.remove(0);
        let next_str = next.to_string();
        match next_str.as_str() {
            "input" => {}
            "output" | "accum" => {
                // both of these are dynamic and we dont
                // validate the next values they reference, so can exit now
                continue;
            }
            "name" => {
                // only validation for name is that it shouldnt reference anything after name:
                if !jpq.segments.is_empty() {
                    diagnostics.push(SpannedDiagnostic::from_str_at_line(jp.clone(), format!("resource '{}' json path query '{}' references a value after 'name'. the name field is a string, there is nothing further to access", resource.resource_name.as_str(), jp.as_str())));
                }
                continue;
            }
            _ => {
                diagnostics.push(SpannedDiagnostic::from_str_at_line(jp.clone(), format!("resource '{}' json path query '{}' references an invalid field after the resource name. must be one of input, output, accum, or name", resource.resource_name.as_str(), jp.as_str())));
                continue;
            }
        }

        // if we got here it means we're referencing the input of another resource
        // look up the value to ensure it exists:
        let resource_input = referenced_resource.input.to_serde_json_value_with_replace_func(&mut |_| {
            Ok(serde_json::Value::Null)
        }).ok().unwrap_or_default();
        let jpq_res = jpq.process(State::root(&resource_input));
        if jpq_res.is_nothing() {
            diagnostics.push(SpannedDiagnostic::from_str_at_line(jp.clone(), format!("resource '{}' json path query '{}' references a nonexistant input field of resource '{}'", resource.resource_name.as_str(), jp.as_str(), referenced_resource.resource_name.as_str())));
        }
    }
}

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

    fn extract_sources_with_messages(mut diagnostics: Vec<SpannedDiagnostic>, src: &str) -> Vec<(String, String)> {
        diagnostics.drain(..).map(|diag| {
            let out = extract_source(diag, src);
            out
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
    fn invalid_jsonpath_reference_nonexistant_resource() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(my_resource)
    {"a": $.nonexistant_resource.input}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "resource 'my_resource' references a non-existant resource 'nonexistant_resource'");
        assert_eq!(first_err.1, "$.nonexistant_resource.input");
    }

    #[test]
    fn invalid_jsonpath_reference_empty() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(my_resource)
    {"a": $}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "resource 'my_resource' cannot have empty json path query '$'");
        assert_eq!(first_err.1, "$");
    }

    #[test]
    fn invalid_jsonpath_reference_not_one_of_input_output_accum_name() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.blah}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "resource 'my_resource' json path query '$.other.blah' references an invalid field after the resource name. must be one of input, output, accum, or name");
        assert_eq!(first_err.1, "$.other.blah");
    }

    #[test]
    fn invalid_jsonpath_reference_nothing_to_reference_after_name() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.name.aaaa}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "resource 'my_resource' json path query '$.other.name.aaaa' references a value after 'name'. the name field is a string, there is nothing further to access");
        assert_eq!(first_err.1, "$.other.name.aaaa");
    }

    #[test]
    fn valid_jsonpath_reference_name() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.name}
"#;
        let _dcl = parse_and_validate(file).expect("it should have no validation errors");
    }

    #[test]
    fn valid_jsonpath_reference_input() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.input}
"#;
        let _dcl = parse_and_validate(file).expect("it should have no validation errors");
    }

    #[test]
    fn valid_jsonpath_reference_output() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.output}
"#;
        let _dcl = parse_and_validate(file).expect("it should have no validation errors");
    }

    #[test]
    fn valid_jsonpath_reference_accum() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.accum}
"#;
        let _dcl = parse_and_validate(file).expect("it should have no validation errors");
    }

    #[test]
    fn valid_jsonpath_reference_input_field() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.input.blah}
"#;
        let _dcl = parse_and_validate(file).expect("it should have no validation errors");
    }

    #[test]
    fn invalid_jsonpath_reference_input_field_nonexistant() {
        let file = r#"
template my_template
  create
    echo hi

resource my_template(other)
    {"blah": 1}

resource my_template(my_resource)
    {"a": $.other.input.beep}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "resource 'my_resource' json path query '$.other.input.beep' references a nonexistant input field of resource 'other'");
        assert_eq!(first_err.1, "$.other.input.beep");
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
