//! crate for parsing a .dcl file
//! a dcl file is composed of sections
//! each section has a required type, followed by optional parameters on the same line.
//! the following line becomes part of the section body as long as they start with at least one
//! indentation character (a space or a tab). all following lines after that also become
//! part of the section body as long as they have the same number of indentation characters.
//! 

use std::str::Lines;

use crate::line_count::PeekableLineCount;

pub mod line_count;

pub const COMMENT_CHAR: char = '#';

#[derive(Clone, Copy, Debug)]
pub enum IndentaionCharacter {
    Tab,
    Space,
}

impl IndentaionCharacter {
    pub fn as_character(&self) -> char {
        match self {
            IndentaionCharacter::Tab => '\t',
            IndentaionCharacter::Space => ' ',
        }
    }
    pub fn as_debug(&self) -> &str {
        match self {
            IndentaionCharacter::Tab => "'\\t'",
            IndentaionCharacter::Space => "' '",
        }
    }
}

#[derive(Debug)]
pub struct Section<'a> {
    pub typ: &'a str,
    pub parameters: Option<&'a str>,
    pub indentation_char: IndentaionCharacter,
    pub indentation_count: usize,
    pub body: Vec<&'a str>,
}

#[derive(Default, Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Default, Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Default, Debug)]
pub struct SpannedDiagnostic {
    pub span: Span,
    pub severity: u32,
    pub message: String,
}

impl PartialEq<&str> for SpannedDiagnostic {
    fn eq(&self, other: &&str) -> bool {
        self.message == *other
    }
}

pub fn consume_until_empty<'a>(lines: &mut PeekableLineCount<Lines<'a>>) -> Option<()> {
    loop {
        let line = lines.peek()?;
        if line.is_empty() {
            break;
        }
        lines.next();
    }
    None
}

pub fn parse_document_to_sections<'a>(document: &'a str) -> Vec<Result<Section<'a>, SpannedDiagnostic>> {
    let mut out = vec![];
    let mut lines: PeekableLineCount<Lines<'a>> = PeekableLineCount::new(document.lines());
    while let Some(line) = lines.next() {
        if line.is_empty() { continue; }
        if line.starts_with(|c: char| c.is_ascii_alphabetic()) {
            // start of section: parse it
            let section = match parse_section_starting_with_line(line, &mut lines) {
                Ok(o) => o,
                Err(e) => {
                    let mut diag = SpannedDiagnostic::default();
                    diag.message = e;
                    out.push(Err(diag));
                    // must advance lines til next section, otherwise will get
                    // unnecessary errors re-parsing the same section from the middle
                    consume_until_empty(&mut lines);
                    continue;
                }
            };
            out.push(Ok(section));
            continue;
        }
        // must be a comment, parse and validate its a valid comment (#)
        let is_invalid = match line.split_once(COMMENT_CHAR) {
            Some((left, _right)) => {
                !left.chars().all(|c| c.is_ascii_whitespace())
            }
            None => true,
        };
        if is_invalid {
            let mut diag = SpannedDiagnostic::default();
            let err_line = lines.last_line_index();
            diag.span.start.line = err_line;
            diag.span.end.line = err_line;
            diag.span.start.column = 0;
            diag.span.end.column = line.len();
            diag.message = format!("invalid line '{}' must be a section or a comment", line);
            out.push(Err(diag));
        }
        // otherwise its a line with a comment. can be ignored
    }
    out
}

pub fn parse_section_starting_with_line<'a>(
    first_line: &'a str,
    next_lines: &mut PeekableLineCount<Lines<'a>>,
) -> Result<Section<'a>, String> {
    // a section must start with a type
    // a type can be optionally followed by parameters
    let (typ, parameters) = match first_line.split_once(' ') {
        Some((l, r)) => (l, Some(r)),
        None => (first_line, None),
    };
    // a type must be ascii alphanumeric characters
    // and whose first character must be alphabetic:
    if !typ.starts_with(|c: char| c.is_ascii_alphabetic()) {
        return Err(format!("invalid section type '{}': must start with ascii alphabetic character", typ));
    }
    if !typ.chars().all(|c| c.is_ascii_alphanumeric()) {
        return Err(format!("invalid section type '{}': must only contain ascii alphanumeric characters", typ));
    }
    // remove from the parameters the text after the comment character, if present
    let parameters = parameters.map(|x| match x.split_once(COMMENT_CHAR) {
        Some((l, _comment)) => l,
        None => x,
    });
    // for debug purposes we can render the parameters as a string. if there are none, its an empty string:
    let parameters_debug = parameters.unwrap_or_default();

    let first_body_line = match next_lines.next() {
        Some(l) => l,
        None => {
            // end of document, likely the section has no body
            // return a fake empty string to make the empty body check simpler:
            ""
        }
    };
    // get the first indentation character
    let mut first_body_line_chars = first_body_line.chars().peekable();
    let first_indentation_character = match first_body_line_chars.next() {
        Some(c) => c,
        None => {
            // empty section body detected. the next line has no characters
            return Ok(Section {
                typ,
                parameters,
                indentation_char: IndentaionCharacter::Space,
                indentation_count: 0,
                body: Vec::new(),
            })
        }
    };
    let indentation_char = match first_indentation_character {
        ' ' => IndentaionCharacter::Space,
        '\t' => IndentaionCharacter::Tab,
        c => return Err(format!(
            "section body of '{}{}' has invalid starting character '{}' must be a space or a tab",
            typ, parameters_debug,
            c
        )),
    };
    let indentation_character = indentation_char.as_character();
    // once an indentation character is found, all the following characters must be the same indentation character
    // until a non-space or non-tab is found
    let mut indentation_count = 1;
    while let Some(c) = first_body_line_chars.peek() {
        if *c != indentation_character {
            if *c == ' ' || *c == '\t' {
                let c_debug = if *c == '\t' {
                    "\\t"
                } else { " " };
                return Err(format!(
                    "section indentation must be same character. found {} occurrences of {} followed by '{}'",
                    indentation_count,
                    indentation_char.as_debug(),
                    c_debug,
                ))
            }
            break;
        }
        indentation_count += 1;
        first_body_line_chars.next();
    }
    // now that we know the indentation character, and the number of indentations
    // we can collect the body:
    let body = parse_section_body(indentation_char, indentation_count, first_body_line, next_lines)?;
    Ok(Section {
        typ,
        parameters,
        indentation_char,
        indentation_count,
        body
    })
}

pub fn add_body_line<'a>(
    body: &mut Vec<&'a str>,
    line: &'a str,
    indentation_count: usize,
    indentation_char: IndentaionCharacter,
    invalid_utf8_err: &str,
) -> Result<(), String> {
    let indentation_character = indentation_char.as_character();
    let (indentation, remaining) = line.split_at_checked(indentation_count).ok_or(invalid_utf8_err)?;
    if !indentation.chars().all(|c| c == indentation_character) {
        return Err(format!("body must start with {} characters of {}", indentation_count, indentation_char.as_debug()));
    }
    let body_line = match remaining.split_once(COMMENT_CHAR) {
        Some((l, _comment)) => l,
        None => remaining,
    };
    if !body_line.is_empty() {
        body.push(body_line);
    }
    Ok(())
}

pub fn parse_section_body<'a>(
    indentation_char: IndentaionCharacter,
    indentation_count: usize,
    first_body_line: &'a str,
    next_lines: &mut PeekableLineCount<Lines<'a>>,
) -> Result<Vec<&'a str>, String> {
    let mut body = vec![];
    let invalid_utf8_err = format!("invalid utf8 sequence from {}..", indentation_count);
    // get first line into body since it was already determined to be part of the body:
    add_body_line(&mut body, first_body_line, indentation_count, indentation_char, &invalid_utf8_err)?;

    // put all lines into body until encounter an empty line
    // a section MUST be followed by an empty line
    // therefore its ok to consume it here: if we detect its empty
    // we break out, and the caller knows to start the next section
    while let Some(next_line) = next_lines.next() {
        if next_line.is_empty() {
            break;
        }
        add_body_line(&mut body, next_line, indentation_count, indentation_char, &invalid_utf8_err)?;
    }
    Ok(body)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_parse_single_sections_eof() {
        let document = "sectionA\n   e\n   b";
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 1);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), ' ');
        assert_eq!(section.indentation_count, 3);
        assert_eq!(section.body, vec!["e", "b"]);
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn section_body_doesnt_include_comments() {
        let document = "sectionA\n   #this is a comment\n   e # this is a comment\n   b";
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 1);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), ' ');
        assert_eq!(section.indentation_count, 3);
        assert_eq!(section.body, vec!["e ", "b"]);
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn comments_between_sections_ignored() {
        let document = "#comment a b c\nsectionA\n\ta\n\n# another comment\n\nsectionB\n\tb\n";
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 2);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), '\t');
        assert_eq!(section.indentation_count, 1);
        assert_eq!(section.body, vec!["a"]);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionB");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), '\t');
        assert_eq!(section.indentation_count, 1);
        assert_eq!(section.body, vec!["b"]);
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn can_parse_multiple_sections() {
        let document = 
            "sectionA\n   e\n   b\n\nsectionB parameter\n  something here\n  123\n\nsectionC\n\ta\n\tc\n\tb\n\n\n\n";
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 3);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), ' ');
        assert_eq!(section.indentation_count, 3);
        assert_eq!(section.body, vec!["e", "b"]);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionB");
        assert_eq!(section.parameters, Some("parameter"));
        assert_eq!(section.indentation_char.as_character(), ' ');
        assert_eq!(section.indentation_count, 2);
        assert_eq!(section.body, vec!["something here", "123"]);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionC");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), '\t');
        assert_eq!(section.indentation_count, 1);
        assert_eq!(section.body, vec!["a", "c", "b"]);
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn can_parse_err_and_keep_going() {
        let document = 
            "sectionA\n   e\n   b\n\nsectionB\n  something\n onlyonespace\n  123\n\nsectionC\n\ta\n\tc\n\tb\n\n\n\n";
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 3);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), ' ');
        assert_eq!(section.indentation_count, 3);
        assert_eq!(section.body, vec!["e", "b"]);
        let err = sections.remove(0).expect_err("should be an error");
        assert_eq!(err, "body must start with 2 characters of ' '");
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionC");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), '\t');
        assert_eq!(section.indentation_count, 1);
        assert_eq!(section.body, vec!["a", "c", "b"]);
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn can_parse_multiple_sections_with_empty_bodies() {
        let document = 
            "sectionA\n\nsectionB parameter(1 2 3)\n\nsectionC";
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 3);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_count, 0);
        assert!(section.body.is_empty());
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionB");
        assert_eq!(section.parameters, Some("parameter(1 2 3)"));
        assert_eq!(section.indentation_count, 0);
        assert!(section.body.is_empty());
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionC");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_count, 0);
        assert!(section.body.is_empty());
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn sections_require_empty_line_between() {
        let document = "sectionA\nsectionB\n\n";
        let mut sections = parse_document_to_sections(document);
        let err = sections.remove(0).expect_err("should be a valid document");
        // TODO: this error message should probably say "multiple sections must be separated by an empty line"
        assert_eq!(err, "section body of 'sectionA' has invalid starting character 's' must be a space or a tab");
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn section_types_must_start_with_alphabetic_characters() {
        let document = "1sectionA\n\n";
        let mut sections = parse_document_to_sections(document);
        let err = sections.remove(0).expect_err("should be a valid document");
        // TODO: this error message should say something about "sections must start with alphabetic ascii character"
        assert_eq!(err, "invalid line '1sectionA' must be a section or a comment");
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn section_types_must_be_alphanumeric() {
        let document = "sec❤️tionA\n\n";
        let mut sections = parse_document_to_sections(document);
        let err = sections.remove(0).expect_err("should be a valid document");
        // TODO: this error message should say something about "sections must start with alphabetic ascii character"
        assert_eq!(err, "invalid section type 'sec❤️tionA': must only contain ascii alphanumeric characters");
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn section_params_can_be_non_ascii() {
        let document = "sectionA ❤️\n\n";
        let mut sections = parse_document_to_sections(document);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters.unwrap(), "❤️");
        assert!(section.body.is_empty());
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn body_can_contain_non_ascii() {
        let document = "sectionA\n\t\t❤️ hello\n\t\tsomething#❤️\n";
        let mut sections = parse_document_to_sections(document);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.indentation_char.as_character(), '\t');
        assert_eq!(section.indentation_count, 2);
        assert_eq!(section.parameters, None);
        assert_eq!(section.body, vec!["❤️ hello", "something"]);
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn sections_body_cant_mix_match_indent_character() {
        let document = "sectionA\n\t\t a";
        let mut sections = parse_document_to_sections(document);
        let err = sections.remove(0).expect_err("should be a valid document");
        assert_eq!(err, "section indentation must be same character. found 2 occurrences of '\\t' followed by ' '");
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn sections_body_must_use_same_indentation_character_each_line() {
        let document = "sectionA\n\t\ta\n  b\n";
        let mut sections = parse_document_to_sections(document);
        let err = sections.remove(0).expect_err("should be a valid document");
        assert_eq!(err, "body must start with 2 characters of '\\t'");
        assert_eq!(sections.len(), 0);
    }
}
