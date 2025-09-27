//! a resource in declarative-cli is a json object (with optional settings [TODO]).
//! this module defines the shape of a resource section

use json_with_positions::{CharIterator, Value};

use crate::{parse::{Section, SpannedDiagnostic}, DclFile};

pub const SECTION_TYPE: &str = "resource";

#[derive(Debug, PartialEq)]
pub struct ResourceSection {
    /// name of the resource to be created/updated/deleted. must be unique
    /// across all resources
    pub resource_name: String,
    /// name of the template that this resource should be passed into
    pub template_name: String,
    pub input: Value,
}

pub fn parse_resource_section<'a>(dcl: &mut DclFile, section: &Section<'a>) -> Result<(), SpannedDiagnostic> {
    let params = section.parameters
        .ok_or("resource must have a name and a template")
        .map_err(|e| {
            SpannedDiagnostic::new(e.to_string(), section.start_line, SECTION_TYPE.len())
        })?;
    let first_line_len = SECTION_TYPE.len() + params.s.chars().count() + 1;
    let (template_name, resource_name) = match params.split_once("(") {
        Some((l, r)) => {
            let resource_name = r.trim();
            if !resource_name.s.ends_with(")") {
                return Err(SpannedDiagnostic::new("resource name must be inside parentheses".to_string(), section.start_line, first_line_len));
            }
            let mut resource_name = resource_name.to_string();
            resource_name.pop();
            (l.trim().to_string(), resource_name)
        },
        None => {
            return Err(SpannedDiagnostic::new("resource must have a template and a name. example `resource my_template(my_resource)`".to_string(), section.start_line, first_line_len));
        }
    };
    if template_name.is_empty() {
        return Err(SpannedDiagnostic::new("resource missing template name".to_string(), section.start_line, first_line_len));
    }
    if resource_name.is_empty() {
        return Err(SpannedDiagnostic::new("resource missing name".to_string(), section.start_line, first_line_len));
    }
    if section.body.is_empty() {
        return Err(SpannedDiagnostic::new(format!("resource '{}' missing a body", resource_name), section.start_line, first_line_len));
    }
    let iter = section.body.clone().into_iter();
    let mut iter = CharIterator::new(iter).peekable();
    let input = json_with_positions::parse_json_value_from_iter_no_recursion(&mut iter)
        .map_err(|e| {
            let mut diag = SpannedDiagnostic::new(
                format!("failed to parse resource '{}' body: {}", resource_name, e),
                section.body.first().map(|l| l.line).unwrap_or(section.start_line),
                999
            );
            diag.span.end.line = section.end_line;
            diag            
        })?;
    let parsed = ResourceSection {
        resource_name,
        template_name,
        input,
    };
    dcl.resources.push(parsed);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::parse::{parse_document_to_sections, sections_to_dcl_file};
    use json_with_positions::Value;

    #[test]
    fn can_parse_resource_sections() {
        let document = r#"
resource some_template(my_name)
  {"hello":"world"}"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dcl = sections_to_dcl_file(sections).unwrap();
        assert_eq!(dcl.resources.len(), 1);
        assert_eq!(dcl.resources[0].resource_name, "my_name");
        assert_eq!(dcl.resources[0].template_name, "some_template");
        assert_eq!(dcl.resources[0].input.clone().to_serde_json_value(), serde_json::json!({"hello":"world"}));
    }

    #[test]
    fn can_parse_resource_sections_with_dynamic_json_path_queries() {
        let document = r#"
resource some_template(my_name)
  {
    "hello": $.other_resource.output.thing
  }

resource other_template(other_resource)
  {
    "a":"b"
  }

"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let mut dcl = sections_to_dcl_file(sections).unwrap();
        assert_eq!(dcl.resources.len(), 2);
        let resource = dcl.resources.remove(0);
        assert_eq!(resource.resource_name, "my_name");
        assert_eq!(resource.template_name, "some_template");
        let val = &resource.input["hello"];
        match val {
            Value::JsonPath { val, ..  } => assert_eq!(val.s, "$.other_resource.output.thing"),
            _ => panic!("it should be a path query")
        }
        let resource = dcl.resources.remove(0);
        assert_eq!(resource.resource_name, "other_resource");
        assert_eq!(resource.template_name, "other_template");
        assert_eq!(resource.input.to_serde_json_value(), serde_json::json!({"a":"b"}));
    }

    #[test]
    fn should_error_for_missing_name() {
        let document = r#"
resource some_template()
  {"hello":"world"}"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let err = sections_to_dcl_file(sections).expect_err("it should err");
        assert_eq!(err.message, "resource missing name");
        assert_eq!(err.span.start.line, 1);
        assert_eq!(err.span.end.line, 1);
        assert_eq!(err.span.start.column, 0);
        assert_eq!(err.span.end.column, 24);
    }
}
