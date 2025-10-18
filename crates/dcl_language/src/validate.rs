//! module for the various validations that should happen to a DclFile once it has been successfully parsed

use jsonpath_rust::query::{state::State, Query};
use str_at_line::StringAtLine;

use crate::{parse::{resource::ResourceSection, SpannedDiagnostic}, DclFile};

pub fn validate_dcl_file(dcl: &DclFile) -> Vec<SpannedDiagnostic> {
    let mut diagnostics = vec![];
    validate_resources(dcl, &mut diagnostics);
    validate_functions(dcl, &mut diagnostics);
    diagnostics
}

pub fn validate_functions(dcl: &DclFile, diagnostics: &mut Vec<SpannedDiagnostic>) {
    for function in dcl.functions.iter() {
        if function.function_type != "javascript" {
            diagnostics.push(SpannedDiagnostic::from_str_at_line(&function.function_type, format!("unsupported function type '{}' currently only javascript is supported", function.function_type.as_str())));
        }
        if function.function_name.s.is_empty() {
            diagnostics.push(SpannedDiagnostic::from_str_at_line(&function.function_type, format!("function name cannot be empty")));
        }
        if let Some(r) = dcl.resources.iter().find(|x| x.resource_name.as_str() == function.function_name.as_str()) {
            diagnostics.push(SpannedDiagnostic::from_str_at_line(&function.function_name, format!("cannot name a function the same name as a resource. found resource '{}' with same name as this function", r.resource_name.as_str())));
        }
    }
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

/// returns Some(_) if the function name was found to be a function
/// in the dcl file, None otherwise. this function will submit diagnostics as-needed.
/// such that if it returns Some(_) there may be 0 or more diagnostics added, and the caller
/// does not need to add more. if it returns None there are guaranteed to be no diagnostics added
/// and the caller should continue checking if its a resource reference instead.
pub fn resource_has_valid_function_reference(
    resource: &ResourceSection,
    dcl: &DclFile,
    diagnostics: &mut Vec<SpannedDiagnostic>,
    function_name: &str,
    jpq: &jsonpath_rust::parser::model::JpQuery,
    jpq_location: &StringAtLine,
) -> Option<()> {
    let _function = dcl.functions.iter().find(|x| x.function_name.as_str() == function_name)?;
    // we know this reference is a function, perform validations that
    // it only has 1 more segment
    let last = match jpq.segments.last() {
        Some(l) => l,
        None => {
            diagnostics.push(SpannedDiagnostic::from_str_at_line(jpq_location, format!("function reference '{}' must be followed by a resource name", jpq_location.as_str())));
            return Some(());
        }
    };
    // there should be 2 segments, but the function name is the first, and it was already removed from the segments.
    if jpq.segments.len() != 1 {
        diagnostics.push(
            SpannedDiagnostic::from_str_at_line(
                jpq_location,
                format!(
                    "function reference '{}' must only have 2 segments: the function name, followed by the resource name to be passed into the function",
                    jpq_location.as_str()
                )
            )
        );
        return Some(());
    }
    // ensure it references a valid resource:
    let mut function_arg_resource = match last {
        jsonpath_rust::parser::model::Segment::Selector(selector) => {
            match selector {
                jsonpath_rust::parser::model::Selector::Name(s) => {
                    s.clone()
                }
                _ => {
                    diagnostics.push(
                        SpannedDiagnostic::from_str_at_line(
                            jpq_location,
                            format!(
                                "function reference '{}' 2nd segment must be a named selector",
                                jpq_location.as_str()
                            )
                        )
                    );
                    return Some(());
                }
            }
        },
        _ => {
            diagnostics.push(
                SpannedDiagnostic::from_str_at_line(
                    jpq_location,
                    format!(
                        "function reference '{}' 2nd segment must be a selector",
                        jpq_location.as_str()
                    )
                )
            );
            return Some(());
        }
    };
    unquote_bracketed_selector(&mut function_arg_resource);
    if function_arg_resource == resource.resource_name.as_str() {
        diagnostics.push(
            SpannedDiagnostic::from_str_at_line(
                jpq_location,
                format!(
                    "resource '{}' cannot call function '{}' with itself as the input",
                    resource.resource_name,
                    function_name,
                )
            )
        );
        return Some(());
    }
    match dcl.resources.iter().find(|x| x.resource_name.as_str() == function_arg_resource) {
        Some(_) => {}
        None => {
            diagnostics.push(
                SpannedDiagnostic::from_str_at_line(
                    jpq_location,
                    format!(
                        "function reference '{}' attempts to call function '{}' with nonexistant resource '{}'",
                        jpq_location.as_str(),
                        function_name,
                        function_arg_resource,
                    )
                )
            );
        }
    }
    Some(())
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
        // must be a valid resource or function:
        let referenced_resource_name = first_segment.to_string();
        // first, try to check if its a valid function:
        if let Some(_) = resource_has_valid_function_reference(resource, dcl, diagnostics, &referenced_resource_name, &jpq, &jp) {
            continue;
        }
        // if its not a function reference, then its a resource reference:
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

pub fn unquote_bracketed_selector(s: &mut String) {
    remove_bracketed_selector_quotes::<'"'>(s);
    remove_bracketed_selector_quotes::<'\''>(s);
}

pub fn remove_bracketed_selector_quotes<const C: char>(s: &mut String) {
    while s.starts_with(C) && s.ends_with(C) && s.len() > 1 {
        s.remove(0);
        s.pop();
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

    #[test]
    fn invalid_jsonpath_function_reference() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(other)
    {"blah": $.myfunc}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        assert_eq!(source_errors.len(), 1);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "function reference '$.myfunc' must be followed by a resource name");
        assert_eq!(first_err.1, "$.myfunc");
    }

    #[test]
    fn jsonpath_function_references_nonexistant_resource() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(other)
    {"blah": $.myfunc['this_resource_doesnt_exist']}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have validation errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        assert_eq!(source_errors.len(), 1);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "function reference '$.myfunc['this_resource_doesnt_exist']' attempts to call function 'myfunc' with nonexistant resource 'this_resource_doesnt_exist'");
        assert_eq!(first_err.1, "$.myfunc['this_resource_doesnt_exist']");
    }

    #[test]
    fn jsonpath_function_ok() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(other)
    {"blah": $.myfunc['this_resource_exists']}

resource my_template(this_resource_exists)
    {}
"#;
        let _dcl = parse_and_validate(file).expect("it should not error");
    }

    #[test]
    fn jsonpath_function_nonquoted_reference_ok() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(other)
    {"blah": $.myfunc.this_resource_exists}

resource my_template(this_resource_exists)
    {}
"#;
        let _dcl = parse_and_validate(file).expect("it should not error");
    }

    #[test]
    fn jsonpath_function_only_two_segments() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(other)
    {"blah": $.myfunc.this_resource_exists.canthaveanythinghere}

resource my_template(this_resource_exists)
    {}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have diagnostic errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        assert_eq!(source_errors.len(), 1);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "function reference '$.myfunc.this_resource_exists.canthaveanythinghere' must only have 2 segments: the function name, followed by the resource name to be passed into the function");
        assert_eq!(first_err.1, "$.myfunc.this_resource_exists.canthaveanythinghere");
    }

    #[test]
    fn jsonpath_function_resource_must_be_named_selector() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(other)
    {"blah": $.myfunc[0]}

resource my_template(this_resource_exists)
    {}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have diagnostic errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        assert_eq!(source_errors.len(), 1);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "function reference '$.myfunc[0]' 2nd segment must be a named selector");
        assert_eq!(first_err.1, "$.myfunc[0]");
    }

    #[test]
    fn jsonpath_function_resource_must_not_be_same_as_caller() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(other)
    {"blah": $.myfunc['other']}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have diagnostic errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        assert_eq!(source_errors.len(), 1);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "resource 'other' cannot call function 'myfunc' with itself as the input");
        assert_eq!(first_err.1, "$.myfunc['other']");
    }

    #[test]
    fn cannot_have_functions_with_same_name_as_resources() {
        let file = r#"
template my_template
  create
    echo hi

function javascript(myfunc)
  console.log(1);

resource my_template(myfunc)
    {}
"#;
        let diagnostics = parse_and_validate(file).expect_err("it should have diagnostic errors");
        let mut source_errors = extract_sources_with_messages(diagnostics, file);
        assert_eq!(source_errors.len(), 1);
        let first_err = source_errors.remove(0);
        assert_eq!(first_err.0, "cannot name a function the same name as a resource. found resource 'myfunc' with same name as this function");
        assert_eq!(first_err.1, "myfunc");
    }
}
