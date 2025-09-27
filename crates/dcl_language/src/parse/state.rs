//! state in declarative-cli is just a json object.
//! this module defines the shape of the state section
//! which defines how/where state is loaded

use crate::{parse::{Section, SpannedDiagnostic}, DclFile};

pub const SECTION_TYPE: &str = "state";

#[derive(Debug, PartialEq, Eq)]
pub struct StateSection {
    /// state section requires loading state from a json file
    /// for now this is the only way to load state.
    pub file: String,
}

pub fn parse_state_section<'a>(dcl: &mut DclFile, section: &Section<'a>) -> Result<(), SpannedDiagnostic> {
    if dcl.state.is_some() {
        let mut diag = SpannedDiagnostic::new(
            "cannot have multiple state sections".to_string(), section.start_line, 999);
        diag.span.end.line = section.end_line;
        return Err(diag);
    }
    let file_line = section.body.iter().find(|x| x.s.starts_with("file"))
        .ok_or("state section missing 'file' option in its body").map_err(|e| {
            let mut diag = SpannedDiagnostic::new(e.to_string(), section.start_line, 999);
            diag.span.end.line = section.end_line;
            diag
        })?;
    let file = file_line.s.replace("file", "").trim().to_string();
    let parsed = StateSection { file };
    dcl.state = Some(parsed);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::parse::{parse_document_to_sections, sections_to_dcl_file};

    use super::*;

    #[test]
    fn can_parse_state_sections() {
        let document = r#"
state
  file hello.txt"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dcl = sections_to_dcl_file(&sections).unwrap();
        assert_eq!(dcl.state, Some(StateSection { file: "hello.txt".to_string() }));
    }

    #[test]
    fn invalid_state_section_errors() {
        let document = r#"
state
  otherthing hello.txt"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let diag = sections_to_dcl_file(&sections).expect_err("dsa");
        assert_eq!(diag.span.start.line, 1);
        assert_eq!(diag.span.end.line, 2);
        assert_eq!(diag.message, "state section missing 'file' option in its body");
    }

    #[test]
    fn cant_have_multiple_state_sections() {
        let document = r#"
state
  file hello.txt

state
  file hello2.txt  
"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let diag = sections_to_dcl_file(&sections).expect_err("dsa");
        assert_eq!(diag.span.start.line, 4);
        assert_eq!(diag.span.end.line, 5);
        assert_eq!(diag.message, "cannot have multiple state sections");
    }
}
