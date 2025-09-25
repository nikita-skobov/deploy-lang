//! the parse module handles parsing an arbitrary text document into sections
//! sections are generic, containing their data as arbitrary text.
//! the actual language itself defines types of sections, which can be successfully parsed
//! via the type field of a section. for example there are language-defined sections `state` and `template`.
//! if parsing a section and those keywords show up as the section's type, then they are parsed as that specific
//! section type, otherwise the section is discarded.
//! this enables us to extend the language features in the future.
//!
//! a dcl file is composed of sections
//! each section has a required type, followed by optional parameters on the same line.
//! the following line becomes part of the section body as long as they start with at least one
//! indentation character (a space or a tab). all following lines after that also become
//! part of the section body as long as they have the same number of indentation characters.

use std::str::Lines;

pub mod line_count;
pub mod state;
pub mod template;
pub mod resource;

use crate::{parse::line_count::{LineCounterIterator, StrAtLine}, DclFile};


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
    pub body: Vec<StrAtLine<'a>>,
    pub start_line: usize,
    pub end_line: usize,
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

#[derive(Debug)]
pub struct SpannedDiagnostic {
    pub span: Span,
    pub severity: u32,
    pub message: String,
}

impl Default for SpannedDiagnostic {
    fn default() -> Self {
        Self {
            span: Default::default(),
            severity: 3, // default to error
            message: Default::default()
        }
    }
}

impl SpannedDiagnostic {
    /// returns a default diagnostic with error set, and with
    /// the start/end to be assumed on the same line, with the provided ending column,
    /// and the starting column assumed to be 0.
    pub fn new(err: String, line: usize, end_column: usize) -> Self {
        let mut out = Self::default();
        out.message = err;
        out.span.start.line = line;
        out.span.end.line = line;
        out.span.end.column = end_column;
        out
    }
}

impl PartialEq<&str> for SpannedDiagnostic {
    fn eq(&self, other: &&str) -> bool {
        self.message == *other
    }
}

pub fn consume_until_empty<'a>(lines: &mut LineCounterIterator<'a, Lines<'a>>) -> Option<()> {
    loop {
        let line = lines.peek()?;
        if line.s.is_empty() {
            break;
        }
        lines.next();
    }
    None
}

pub fn sections_to_dcl_file_with_logger<'a>(
    sections: Vec<Section<'a>>,
    mut _logger: impl Logger,
) -> Result<DclFile, SpannedDiagnostic> {
    let mut out = DclFile::default();
    for section in sections {
        match section.typ {
            state::SECTION_TYPE => {
                state::parse_state_section(&mut out, &section)?;
            },
            resource::SECTION_TYPE => {
                resource::parse_resource_section(&mut out, &section)?;
            }
            template::SECTION_TYPE => {
                template::parse_template_section(&mut out, &section)?;
            }
            _ => {
                // silently drop unknown section
                // TODO: in the future might want to make this a warn/error depending on user configuration
            }
        }
    }
    Ok(out)
}


/// after calling `parse_document_to_sections` you filter out the errors and pass
/// the successfully parsed sections to this function, and it parses generic Sections into
/// concrete Sections of a DclFile, discarding any unknown sections
pub fn sections_to_dcl_file<'a>(sections: Vec<Section<'a>>) -> Result<DclFile, SpannedDiagnostic> {
    sections_to_dcl_file_with_logger(sections, ())
}

pub trait Logger: Clone {
    fn log(&mut self, log: &str);
}

impl Logger for () {
    fn log(&mut self, _log: &str) {}
}

pub fn parse_document_to_sections_with_logger<'a>(
    document: &'a str,
    _logger: impl Logger,
) -> Vec<Result<Section<'a>, SpannedDiagnostic>> {
    let mut out = vec![];
    let mut lines: LineCounterIterator<'a, Lines<'a>> = LineCounterIterator::new(document.lines());
    while let Some(line) = lines.next() {
        if line.s.is_empty() { continue; }
        // ignore empty whitespace lines:
        if line.s.chars().all(|c: char| c.is_ascii_whitespace()) { continue; }
        if line.s.starts_with(|c: char| c.is_ascii_alphabetic()) {
            // start of section: parse it
            let section = match parse_section_starting_with_line(line, &mut lines) {
                Ok(o) => o,
                Err(e) => {
                    out.push(Err(e));
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
                !left.s.chars().all(|c| c.is_ascii_whitespace())
            }
            None => true,
        };
        if is_invalid {
            let mut diag = SpannedDiagnostic::default();
            diag.span.start.line = line.line;
            diag.span.end.line = line.line;
            diag.span.start.column = 0;
            diag.span.end.column = line.s.len();
            diag.message = format!("invalid line '{}' must be a section or a comment", line);
            out.push(Err(diag));
        }
        // otherwise its a line with a comment. can be ignored
    }
    out
}

pub fn parse_document_to_sections<'a>(document: &'a str) -> Vec<Result<Section<'a>, SpannedDiagnostic>> {
    parse_document_to_sections_with_logger(document, ())
}

pub fn parse_section_starting_with_line<'a>(
    first_line: StrAtLine<'a>,
    next_lines: &mut LineCounterIterator<'a, Lines<'a>>,
) -> Result<Section<'a>, SpannedDiagnostic> {
    // let line_index = next_lines.last_line_index();
    // a section must start with a type
    // a type can be optionally followed by parameters
    let (typ, parameters) = match first_line.split_once(' ') {
        Some((l, r)) => (l, Some(r)),
        None => (first_line, None),
    };
    // a type must be ascii alphanumeric characters
    // and whose first character must be alphabetic:
    if !typ.s.starts_with(|c: char| c.is_ascii_alphabetic()) {
        let err = format!("invalid section type '{}': must start with ascii alphabetic character", typ);
        let diag = SpannedDiagnostic::new(err, typ.line, typ.s.len());
        return Err(diag);
    }
    if !typ.s.chars().all(|c| c.is_ascii_alphanumeric()) {
        let err = format!("invalid section type '{}': must only contain ascii alphanumeric characters", typ);
        let diag = SpannedDiagnostic::new(err, typ.line, typ.s.len());
        return Err(diag);
    }
    // remove from the parameters the text after the comment character, if present
    let parameters = parameters.map(|x| match x.split_once(COMMENT_CHAR) {
        Some((l, _comment)) => l,
        None => x,
    });
    // for debug purposes we can render the parameters as a string. if there are none, its an empty string:
    let parameters_debug = parameters.clone().unwrap_or_default();

    let first_body_line = match next_lines.next() {
        Some(l) => l,
        None => {
            // end of document, likely the section has no body
            // return a fake empty string to make the empty body check simpler:
            StrAtLine::default()
        }
    };
    // get the first indentation character
    let mut first_body_line_chars = first_body_line.s.chars().peekable();
    let first_indentation_character = match first_body_line_chars.next() {
        Some(c) => c,
        None => {
            // empty section body detected. the next line has no characters
            return Ok(Section {
                start_line: typ.line,
                end_line: typ.line,
                typ: typ.s,
                parameters: parameters.map(|p| p.s),
                indentation_char: IndentaionCharacter::Space,
                indentation_count: 0,
                body: Vec::new(),
            })
        }
    };
    let indentation_char = match first_indentation_character {
        ' ' => IndentaionCharacter::Space,
        '\t' => IndentaionCharacter::Tab,
        c => {
            let err = format!(
                "section body of '{}{}' has invalid starting character '{}' must be a space or a tab",
                typ, parameters_debug,
                c
            );
            let diag = SpannedDiagnostic::new(err, first_body_line.line, 1);
            return Err(diag)
        }
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
                let err = format!(
                    "section indentation must be same character. found {} occurrences of {} followed by '{}'",
                    indentation_count,
                    indentation_char.as_debug(),
                    c_debug,
                );
                let diag = SpannedDiagnostic::new(err, first_body_line.line, indentation_count);
                return Err(diag);
            }
            break;
        }
        indentation_count += 1;
        first_body_line_chars.next();
    }
    // now that we know the indentation character, and the number of indentations
    // we can collect the body:
    let body = parse_section_body(indentation_char, indentation_count, first_body_line, next_lines)?;
    // the fact we're here suggests body was not empty, but to be safe we deafult to end_line: 0
    let end_line = body.last().map(|l| l.line).unwrap_or(0);
    Ok(Section {
        typ: typ.s,
        parameters: parameters.map(|p| p.s),
        indentation_char,
        indentation_count,
        body,
        start_line: typ.line,
        end_line,
    })
}

pub fn add_body_line<'a>(
    body: &mut Vec<StrAtLine<'a>>,
    line: StrAtLine<'a>,
    line_index: usize,
    indentation_count: usize,
    indentation_char: IndentaionCharacter,
    invalid_utf8_err: &str,
) -> Result<(), SpannedDiagnostic> {
    let indentation_character = indentation_char.as_character();
    let (indentation, remaining) = line.split_at_checked(indentation_count).ok_or(invalid_utf8_err)
        .map_err(|e| SpannedDiagnostic::new(e.to_string(), line_index, line.s.chars().count()))?;
    if !indentation.s.chars().all(|c| c == indentation_character) {
        let err = format!("body must start with {} characters of {}", indentation_count, indentation_char.as_debug());
        let diag = SpannedDiagnostic::new(err, line_index, indentation_count);
        return Err(diag);
    }
    let body_line = match remaining.split_once(COMMENT_CHAR) {
        Some((l, _comment)) => l,
        None => remaining,
    };
    if !body_line.s.is_empty() {
        body.push(body_line);
    }
    Ok(())
}

pub fn parse_section_body<'a>(
    indentation_char: IndentaionCharacter,
    indentation_count: usize,
    first_body_line: StrAtLine<'a>,
    next_lines: &mut LineCounterIterator<'a, Lines<'a>>,
) -> Result<Vec<StrAtLine<'a>>, SpannedDiagnostic> {
    let mut body = vec![];
    let invalid_utf8_err = format!("invalid utf8 sequence from {}..", indentation_count);
    // get first line into body since it was already determined to be part of the body:
    let line = first_body_line.line;
    add_body_line(&mut body, first_body_line, line, indentation_count, indentation_char, &invalid_utf8_err)?;

    // put all lines into body until encounter an empty line
    // a section MUST be followed by an empty line
    // therefore its ok to consume it here: if we detect its empty
    // we break out, and the caller knows to start the next section
    while let Some(next_line) = next_lines.next() {
        if next_line.s.is_empty() {
            break;
        }
        let line = next_line.line;
        add_body_line(&mut body, next_line, line, indentation_count, indentation_char, &invalid_utf8_err)?;
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
    fn can_parse_multiple_indents() {
        let document = "sectionA\n   create\n     b\n     c\n       d\n\n";
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 1);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters, None);
        assert_eq!(section.indentation_char.as_character(), ' ');
        assert_eq!(section.indentation_count, 3);
        assert_eq!(section.body, vec!["create", "  b", "  c", "    d"]);
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
    fn section_parsing_allows_whitespace_in_empty_line() {
        let document = r#"
this is a section

# the following line has a lot of extra spaces:
                          
# the following line has a lot of extra tabs:
									
"#;
        let sections = parse_document_to_sections(document);
        for res in sections {
            assert!(res.is_ok(), "it errored: {:?}", res);
        }
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
    fn body_line_positions_tracked_around_comments() {
        let document = r#"
line1
  body starts on line2
  # line3 is a comment, skipped
  line4 has content # and then a comment that is dropped
  # multi line comment
  # is skipped
  # ...
  line8
"#;
        let mut sections = parse_document_to_sections(document);
        assert_eq!(sections.len(), 1);
        let mut section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "line1");
        assert_eq!(section.parameters, None);
        let line = section.body.remove(0);
        assert_eq!(line, "body starts on line2");
        assert_eq!(line.line, 2);
        assert_eq!(line.col, 2);
        let line = section.body.remove(0);
        assert_eq!(line, "line4 has content ");
        assert_eq!(line.line, 4);
        assert_eq!(line.col, 2);
        let line = section.body.remove(0);
        assert_eq!(line, "line8");
        assert_eq!(line.line, 8);
        assert_eq!(line.col, 2);
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
        let document = "sec💙tionA\n\n";
        let mut sections = parse_document_to_sections(document);
        let err = sections.remove(0).expect_err("should be a valid document");
        // TODO: this error message should say something about "sections must start with alphabetic ascii character"
        assert_eq!(err, "invalid section type 'sec💙tionA': must only contain ascii alphanumeric characters");
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn section_params_can_be_non_ascii() {
        let document = "sectionA 💙\n\n";
        let mut sections = parse_document_to_sections(document);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.parameters.unwrap(), "💙");
        assert!(section.body.is_empty());
        assert_eq!(sections.len(), 0);
    }

    #[test]
    fn body_can_contain_non_ascii() {
        let document = "sectionA\n\t\t💙 hello\n\t\tsomething#💙\n";
        let mut sections = parse_document_to_sections(document);
        let section = sections.remove(0).expect("should be a valid document");
        assert_eq!(section.typ, "sectionA");
        assert_eq!(section.indentation_char.as_character(), '\t');
        assert_eq!(section.indentation_count, 2);
        assert_eq!(section.parameters, None);
        assert_eq!(section.body, vec!["💙 hello", "something"]);
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

    #[test]
    fn section_body_err_correct_position() {
        // this should error on the "dsa" line because its indented with a tab
        // whereas the line before it is indented with a space
        let document = r#"
hello
 w
	dsa"#;
        let mut sections = parse_document_to_sections(document);
        let err = sections.remove(0).expect_err("should be an error");
        assert_eq!(err.span.start.line, 3);
        assert_eq!(err.span.end.line, 3);
        assert_eq!(err.span.start.column, 0);
        assert_eq!(err.span.end.column, 1);
    }

    #[test]
    fn section_parses_start_and_end_line_of_section() {
        let document = r#"# this is line0
this is line 1
    line2
    # line3
    line 4
    line 5

this is line 7


this is line 10

another 12
  section
  ends on 14"#;

        let mut sections = parse_document_to_sections(document);
        let section = sections.remove(0).expect("it should be a valid section");
        assert_eq!(section.start_line, 1);
        assert_eq!(section.end_line, 5);

        let section = sections.remove(0).expect("it should be a valid section");
        assert_eq!(section.start_line, 7);
        assert_eq!(section.end_line, 7);

        let section = sections.remove(0).expect("it should be a valid section");
        assert_eq!(section.start_line, 10);
        assert_eq!(section.end_line, 10);

        let section = sections.remove(0).expect("it should be a valid section");
        assert_eq!(section.start_line, 12);
        assert_eq!(section.end_line, 14);
    }
}
