//! state in deploylang is just a json object.
//! this module defines the shape of the state section
//! which defines how/where state is loaded

use str_at_line::StringAtLine;

use crate::{parse::{Section, SpannedDiagnostic}, DplFile};

pub const SECTION_TYPE: &str = "state";

#[derive(Debug, PartialEq, Eq)]
pub struct StateSection {
    pub file_kw: StringAtLine,
    /// state section requires loading state from a json file
    /// for now this is the only way to load state.
    pub file: StringAtLine,
    pub start_line: usize,
    pub end_line: usize,
}

pub fn parse_state_section_as_value<'a>(section: &Section<'a>) -> Result<StateSection, SpannedDiagnostic> {
    let file_line = section.body.iter().find(|x| x.s.starts_with("file"))
        .ok_or("state section missing 'file' option in its body").map_err(|e| {
            let mut diag = SpannedDiagnostic::new(e.to_string(), section.start_line, 999);
            diag.span.end.line = section.end_line;
            diag
        })?;
    let (file_kw, file) = file_line.split_once(" ")
        .ok_or("no 'file' specified")
        .map_err(|e| SpannedDiagnostic::from_str_at_line(file_line, e))?;
    let file_kw = file_kw.to_owned();
    let file = file.trim().to_owned();
    let parsed = StateSection { file_kw, file, start_line: section.start_line, end_line: section.end_line };
    Ok(parsed)
}

pub fn parse_state_section<'a>(dpl: &mut DplFile, section: &Section<'a>) -> Result<(), SpannedDiagnostic> {
    if dpl.state.is_some() {
        let mut diag = SpannedDiagnostic::new(
            "cannot have multiple state sections".to_string(), section.start_line, 999);
        diag.span.end.line = section.end_line;
        return Err(diag);
    }
    let parsed = parse_state_section_as_value(section)?;
    dpl.state = Some(parsed);
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::parse::{parse_document_to_sections, sections_to_dpl_file};

    #[test]
    fn can_parse_state_sections() {
        let document = r#"
state
  file hello.txt"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let dpl = sections_to_dpl_file(&sections).unwrap();
        let state = dpl.state.unwrap();
        assert_eq!(state.file_kw, "file");
        assert_eq!(state.file_kw.line, 2);
        assert_eq!(state.file_kw.col, 2);
        assert_eq!(state.file, "hello.txt");
        assert_eq!(state.start_line, 1);
        assert_eq!(state.end_line, 2);
    }

    #[test]
    fn invalid_state_section_errors() {
        let document = r#"
state
  otherthing hello.txt"#;
        let mut sections = parse_document_to_sections(document);
        let sections: Vec<_> = sections.drain(..).map(|x| x.unwrap()).collect();
        let diag = sections_to_dpl_file(&sections).expect_err("dsa");
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
        let diag = sections_to_dpl_file(&sections).expect_err("dsa");
        assert_eq!(diag.span.start.line, 4);
        assert_eq!(diag.span.end.line, 5);
        assert_eq!(diag.message, "cannot have multiple state sections");
    }
}
