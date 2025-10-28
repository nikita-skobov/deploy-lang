//! traits and implementations for language features to make hover/completions easier

use crate::parse;

pub struct Pos {
    pub line: usize,
    pub col: usize,
}

pub trait InRange {
    fn in_range(&self, pos: Pos) -> bool;
}

pub trait HoverHint: InRange {
    fn hover(&self, pos: Pos) -> Option<String>;
}

pub trait CompletionHint: InRange {
    fn completion(&self, pos: Pos) -> Option<String>;
}

impl<'a> InRange for parse::Section<'a> {
    fn in_range(&self, pos: Pos) -> bool {
        pos.line >= self.typ.line && pos.line <= self.end_line
    }
}

impl InRange for parse::function::FunctionSection {
    fn in_range(&self, pos: Pos) -> bool {
        pos.line >= self.function_name.line && pos.line <= self.end_line
    }
}

impl InRange for parse::state::StateSection {
    fn in_range(&self, pos: Pos) -> bool {
        pos.line >= self.start_line && pos.line <= self.end_line
    }
}

impl InRange for parse::constant::ConstSection {
    fn in_range(&self, pos: Pos) -> bool {
        pos.line >= self.const_name.line && pos.line <= self.end_line
    }
}

impl InRange for parse::resource::ResourceSection {
    fn in_range(&self, pos: Pos) -> bool {
        pos.line >= self.resource_name.line && pos.line <= self.end_line
    }
}

impl InRange for parse::template::TemplateSection {
    fn in_range(&self, pos: Pos) -> bool {
        pos.line >= self.template_name.line && pos.line <= self.end_line
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse_and_validate;

    const DOCUMENT: &str = r#"
# line2 v
const my_const
  {}

# line6 v
template my_template
  create
    echo hello

# line 11 v
resource my_template(my_resource)
  {}

# line 15 v
function javascript(my_function)
  return ""

# line 19 v
state
  file hello.txt
"#;

    #[test]
    fn const_in_range_works() {
        let parsed = parse_and_validate(DOCUMENT).unwrap();
        let const_section = parsed.constants.first().unwrap();
        assert!(!const_section.in_range(Pos { line: 1, col: 0 }));
        assert!(const_section.in_range(Pos { line: 2, col: 0 }));
        assert!(const_section.in_range(Pos { line: 3, col: 0 }));
        assert!(!const_section.in_range(Pos { line: 4, col: 0 }));
    }

    #[test]
    fn state_in_range_works() {
        let parsed = parse_and_validate(DOCUMENT).unwrap();
        let state_section = parsed.state.unwrap();
        assert!(!state_section.in_range(Pos { line: 18, col: 0 }));
        assert!(state_section.in_range(Pos { line: 19, col: 0 }));
        assert!(state_section.in_range(Pos { line: 20, col: 0 }));
        assert!(!state_section.in_range(Pos { line: 21, col: 0 }));
    }

    #[test]
    fn function_in_range_works() {
        let parsed = parse_and_validate(DOCUMENT).unwrap();
        let func_section = parsed.functions.first().unwrap();
        assert!(!func_section.in_range(Pos { line: 14, col: 0 }));
        assert!(func_section.in_range(Pos { line: 15, col: 0 }));
        assert!(func_section.in_range(Pos { line: 16, col: 0 }));
        assert!(!func_section.in_range(Pos { line: 17, col: 0 }));
    }

    #[test]
    fn template_in_range_works() {
        let parsed = parse_and_validate(DOCUMENT).unwrap();
        let templ_section = parsed.templates.first().unwrap();
        assert!(!templ_section.in_range(Pos { line: 5, col: 0 }));
        assert!(templ_section.in_range(Pos { line: 6, col: 0 }));
        assert!(templ_section.in_range(Pos { line: 8, col: 0 }));
        assert!(!templ_section.in_range(Pos { line: 9, col: 0 }));
    }

    #[test]
    fn resource_in_range_works() {
        let parsed = parse_and_validate(DOCUMENT).unwrap();
        let res_section = parsed.resources.first().unwrap();
        assert!(!res_section.in_range(Pos { line: 10, col: 0 }));
        assert!(res_section.in_range(Pos { line: 11, col: 0 }));
        assert!(res_section.in_range(Pos { line: 12, col: 0 }));
        assert!(!res_section.in_range(Pos { line: 13, col: 0 }));
    }
}
