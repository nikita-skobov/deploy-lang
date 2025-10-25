use json_with_positions::{CharIterator, Value};
use str_at_line::StringAtLine;

use crate::{parse::{Section, SpannedDiagnostic}, DplFile};

pub const SECTION_TYPE: &str = "const";

#[derive(Debug, PartialEq, Default, Clone)]
pub struct ConstSection {
    /// a unique identifier for this constant item
    pub const_name: StringAtLine,
    pub body: Value,
}


pub fn parse_const_section<'a>(dpl: &mut DplFile, section: &Section<'a>) -> Result<(), SpannedDiagnostic> {
    let const_name = section.parameters
        .ok_or("const section must be followed by the const name")
        .map_err(|e| SpannedDiagnostic::from_str_at_line(section.typ, e))?;
    let const_name = const_name.trim().to_owned();

    let iter = section.body.clone().into_iter();
    let mut iter = CharIterator::new(iter).peekable();
    let body = json_with_positions::parse_json_value_from_iter_no_recursion(&mut iter)
        .map_err(|e| {
            let mut diag = SpannedDiagnostic::new(
                format!("failed to parse const '{}' body: {}", const_name, e),
                section.body.first().map(|l| l.line).unwrap_or(section.start_line),
                999
            );
            diag.span.end.line = section.end_line;
            diag            
        })?;
    // ensure there's no json paths. to_value_with_replaced_json_paths
    // will only call the callback with json paths, otherwise the body is returned as-is.
    let mut err_diag = None;
    let body = body.to_value_with_replaced_json_paths(&mut |s, _| {
        let diag = SpannedDiagnostic::from_str_at_line(
            &s,
            format!("const '{}' body cannot contain any json path queries. Found json path '{}'", const_name, s),
        );
        err_diag = Some(diag);
        return Err("e".to_string());
    });
    let body = match body {
        Ok(o) => o,
        Err(_e) => {
            if let Some(diag) = err_diag {
                return Err(diag);
            }
            return Err(SpannedDiagnostic::from_str_at_line(&const_name, format!("const '{}' body cannot contain any json path queries", const_name)));
        }
    };

    dpl.constants.push(ConstSection { const_name, body });
    Ok(())
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use json_with_positions::Value;

    #[test]
    fn can_parse_const_sections() {
        let document = r#"
const my_constant
  "abc"
"#;
        let mut dpl = crate::parse_and_validate(document).expect("it should be a valid dpl");
        assert_eq!(dpl.constants.len(), 1);
        let constant = dpl.constants.remove(0);
        assert_eq!(constant.const_name, "my_constant");
        assert_matches!(constant.body, Value::String { val, .. } => {
            assert_eq!(val, "abc");
        });
    }

    #[test]
    fn can_parse_const_sections_json_array() {
        let document = r#"
const my_constant2
  [1, 2, "3", null]
"#;
        let mut dpl = crate::parse_and_validate(document).expect("it should be a valid dpl");
        assert_eq!(dpl.constants.len(), 1);
        let constant = dpl.constants.remove(0);
        assert_eq!(constant.const_name, "my_constant2");
        let serde_json_val = constant.body.to_serde_json_value();
        assert_eq!(serde_json_val, serde_json::json!([1, 2, "3", null]));
    }

    #[test]
    fn can_parse_const_sections_json_obj() {
        let document = r#"
const my_constant3
  {
    "a": false,
    "b": { "c": "d" },
    "e": []
  }
"#;
        let mut dpl = crate::parse_and_validate(document).expect("it should be a valid dpl");
        assert_eq!(dpl.constants.len(), 1);
        let constant = dpl.constants.remove(0);
        assert_eq!(constant.const_name, "my_constant3");
        let serde_json_val = constant.body.to_serde_json_value();
        assert_eq!(serde_json_val, serde_json::json!({
            "a": false,
            "b": { "c": "d" },
            "e": []
        }));
    }

    #[test]
    fn const_section_cannot_have_json_paths() {
        let document = r#"
const my_constant3
  {
    "a": false,
    "b": { "c": "d" },
    "e": $.a
  }
"#;
        let mut errs = crate::parse_and_validate(document).expect_err("it should error");
        assert_eq!(errs.len(), 1);
        let err = errs.remove(0);
        assert_eq!(err.message, "const 'my_constant3' body cannot contain any json path queries. Found json path '$.a'");
    }
}
