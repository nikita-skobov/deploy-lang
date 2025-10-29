use str_at_line::StringAtLine;

use crate::{parse::{Section, SpannedDiagnostic}, DplFile};

pub const SECTION_TYPE: &str = "function";

#[derive(Debug, PartialEq, Default, Clone)]
pub struct FunctionSection {
    /// langauge of the function. currently only javascript is supported
    pub function_type: StringAtLine,
    /// a unique identifier for this function
    pub function_name: StringAtLine,
    pub body: Vec<StringAtLine>,
    pub end_line: usize,
}

impl FunctionSection {
    pub fn get_body(&self) -> String {
        let num_lines = self.body.len();
        let total_size: usize = self.body.iter().map(|x| x.s.len()).sum();
        let mut out = String::with_capacity(total_size + num_lines);
        for line in self.body.iter() {
            out.push_str(line.as_str());
            out.push('\n');
        }
        // remove trailing newline
        out.pop();
        out
    }
}

pub fn parse_function_section_as_value<'a>(section: &Section<'a>) -> Result<FunctionSection, SpannedDiagnostic> {
    let function_name_and_type = section.parameters
        .ok_or("function section must be followed by the function type and the function name")
        .map_err(|e| SpannedDiagnostic::from_str_at_line(section.typ, e))?;
    let (function_type, function_name) = function_name_and_type.split_once("(")
        .ok_or("function name and type must be formatted as `language(func_name)`")
        .map_err(|e| SpannedDiagnostic::from_str_at_line(function_name_and_type, e))?;
    let function_type = function_type.trim().to_owned();
    let mut function_name = function_name.to_owned();
    let end_line = section.end_line;
    let popped_parens = function_name.pop();
    if popped_parens != Some(')') {
        return Err(SpannedDiagnostic::from_str_at_line(function_name_and_type, "missing closing parentheses in function name"));
    }
    let body = section.body.clone().drain(..).map(|x| x.to_owned()).collect();
    let function_section = FunctionSection {
        function_name,
        function_type,
        body,
        end_line,
    };
    Ok(function_section)
}

pub fn parse_function_section<'a>(dpl: &mut DplFile, section: &Section<'a>) -> Result<(), SpannedDiagnostic> {
    let function_section = parse_function_section_as_value(section)?;
    dpl.functions.push(function_section);
    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn can_parse_function_sections() {
        let document = r#"
function javascript(my_func)
  return "abc"
"#;
        let mut dpl = crate::parse_and_validate(document).expect("it should be a valid dpl");
        assert_eq!(dpl.functions.len(), 1);
        let func = dpl.functions.remove(0);
        assert_eq!(func.function_type, "javascript");
        assert_eq!(func.function_name, "my_func");
        assert_eq!(func.get_body(), "return \"abc\"");
    }

    #[test]
    fn functions_can_have_multi_line_bodies() {
        let document = r#"
function javascript(my_func)
  let x = 23;
  let xstr = x.toString();
  let char3 = xstr.charAt(1);
  if (char3 === '3') {
    return '2';
  }
  return char3;

template mytemplate
  create
    echo hi
"#;
        let mut dpl = crate::parse_and_validate(document).expect("it should be a valid dpl");
        assert_eq!(dpl.functions.len(), 1);
        let func = dpl.functions.remove(0);
        assert_eq!(func.function_type, "javascript");
        assert_eq!(func.function_name, "my_func");
        assert_eq!(func.get_body(), r#"let x = 23;
let xstr = x.toString();
let char3 = xstr.charAt(1);
if (char3 === '3') {
  return '2';
}
return char3;"#);
    }

    #[test]
    fn errors_on_unknown_function_type() {
        let document = r#"
function ruby(my_func)
  return "abc"
"#;
        let err = crate::parse_and_validate(document).expect_err("it should fail");
        assert_eq!(err.len(), 1);
        assert_eq!(&err[0].message, "unsupported function type 'ruby' currently only javascript is supported");
    }

    #[test]
    fn errors_on_empty_function_name() {
        let document = r#"
function javascript()
  return "abc"
"#;
        let err = crate::parse_and_validate(document).expect_err("it should fail");
        assert_eq!(err.len(), 1);
        assert_eq!(&err[0].message, "function name cannot be empty");
    }
}
