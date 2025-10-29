//! traits and implementations for language features to make hover/completions easier

use str_at_line::{StrAtLine, StringAtLine};

use crate::{parse::{self, state::StateSection}, ParsedSection, SectionOrParsed};

#[derive(Debug, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

pub struct WalkContext<'a> {
    pub sections: Vec<SectionOrParsed<'a>>,
}

pub trait HoverInfo {
    type Ctx;

    fn get_hover_info(&self, ctx: &Self::Ctx, pos: Pos) -> Option<String>;
}

pub trait InRange {
    fn in_range(&self, pos: Pos) -> bool;
}

pub trait CompletionHint: InRange {
    fn completion(&self, pos: Pos) -> Option<String>;
}

impl<'a> InRange for StrAtLine<'a> {
    fn in_range(&self, pos: Pos) -> bool {
        let end_col = self.col + self.s.chars().count();
        self.line == pos.line && pos.col >= self.col && pos.col <= end_col 
    }
}

impl InRange for StringAtLine {
    fn in_range(&self, pos: Pos) -> bool {
        let end_col = self.col + self.s.chars().count();
        self.line == pos.line && pos.col >= self.col && pos.col <= end_col 
    }
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

impl InRange for ParsedSection {
    fn in_range(&self, pos: Pos) -> bool {
        match self {
            ParsedSection::Template(inner_section) => inner_section.in_range(pos),
            ParsedSection::State(inner_section) => inner_section.in_range(pos),
            ParsedSection::Resource(inner_section) => inner_section.in_range(pos),
            ParsedSection::Function(inner_section) => inner_section.in_range(pos),
            ParsedSection::Const(inner_section) => inner_section.in_range(pos),
        }
    }
}

impl<'a> InRange for SectionOrParsed<'a> {
    fn in_range(&self, pos: Pos) -> bool {
        match self {
            SectionOrParsed::Section(section) => section.in_range(pos),
            SectionOrParsed::Parsed(parsed_section) => parsed_section.in_range(pos),
        }
    }
}

impl<'a> HoverInfo for &'a StateSection {
    type Ctx = WalkContext<'a>;

    fn get_hover_info(&self, _ctx: &Self::Ctx, pos: Pos) -> Option<String> {
        // state can only give hover info for the 'file' keyword:
        if !self.file_kw.in_range(pos) {
            return None
        }
        return Some(format!("the file where state is read/written from. if the file doesnt exist, it will be created."))
    }
}

impl<'a> HoverInfo for WalkContext<'a> {
    type Ctx = Self;

    fn get_hover_info(&self, ctx: &Self::Ctx, pos: Pos) -> Option<String> {
        // walk through all sections, if the position is inside a particular section
        // call its hover info func
        for section in self.sections.iter() {
            if section.in_range(pos) {
                match section {
                    SectionOrParsed::Section(section) => {
                        // for an unparsed section, we can only give hover hint for the section type
                        if !section.typ.in_range(pos) {
                            return None
                        }
                        // TODO: get better hover comments here
                        match section.typ.s {
                            parse::constant::SECTION_TYPE => {
                                return Some("a constant section".to_string())
                            },
                            parse::function::SECTION_TYPE => {
                                return Some("a function section".to_string())
                            },
                            parse::resource::SECTION_TYPE => {
                                return Some("a resource section".to_string())
                            },
                            parse::state::SECTION_TYPE => {
                                return Some("a state section".to_string())
                            },
                            parse::template::SECTION_TYPE => {
                                return Some("a template section".to_string())
                            },
                            _ => return None,
                        }
                    },
                    // call the hover func for the specific type of section:
                    SectionOrParsed::Parsed(parsed_section) => match parsed_section {
                        ParsedSection::State(state_section) => return state_section.get_hover_info(ctx, pos),
                        // TODO: implement walking hover info for these section types:
                        ParsedSection::Template(_template_section) => return None,
                        ParsedSection::Resource(_resource_section) => return None,
                        ParsedSection::Function(_function_section) => return None,
                        ParsedSection::Const(_const_section) => return None,
                    }
                }
            }
        }
        // no section in position
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{get_sections_or_parsed, parse_and_validate};

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

    #[test]
    fn can_get_hover_hint_for_state_file_keyword() {
        let sections = get_sections_or_parsed(DOCUMENT);
        let walk = WalkContext { sections };
        let hover_info = walk.get_hover_info(&walk, Pos { line: 20, col: 2 }).unwrap();
        let expected_substr = "the file where state";
        assert_eq!(hover_info.get(0..expected_substr.len()).unwrap(), expected_substr);
    }

    #[test]
    fn state_keyword_does_not_give_file_hint() {
        let sections = get_sections_or_parsed(DOCUMENT);
        let walk = WalkContext { sections };
        let hover_info = walk.get_hover_info(&walk, Pos { line: 19, col: 22 });
        assert!(hover_info.is_none(), "expected to not get anything, found: {:?}", hover_info);
    }
}
