use std::{collections::HashMap, str::{Chars, Lines}};

use str_at_line::{LineCounterIterator, StrAtLine, StringAtLine};

#[derive(Debug, Clone)]
pub enum Value {
    Null { pos: Position, val: () },
    Bool { pos: Position, val: bool },
    Number { pos: Position, val: Number },
    String { pos: Position, val: StringAtLine },
    Array { pos: Position, val: Vec<Value> },
    Object { pos: Position, val: HashMap<StringAtLine, Value> },
}

#[derive(Debug, Clone)]
pub enum Number {
    Float(f64),
    Int(i64),
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CharPosition {
    pub pos: Position,
    pub c: char,
}

pub struct CharPositionIterator<'a> {
    pub start_line: usize,
    pub start_col: usize,
    pub chars: Chars<'a>,
    pub num_next_calls: usize,
}

impl<'a> CharPositionIterator<'a> {
    pub fn new(s: StrAtLine<'a>) -> Self {
        Self {
            start_line: s.line,
            start_col: s.col,
            chars: s.s.chars(),
            num_next_calls: 0,
        }
    }
}

impl<'a> Iterator for CharPositionIterator<'a> {
    type Item = CharPosition;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        let out = CharPosition {
            c,
            pos: Position {
                line: self.start_line,
                column: self.start_col + self.num_next_calls,
            }
        };
        self.num_next_calls += 1;
        Some(out)
    }
}

pub struct CharIterator<'a, I: Iterator<Item = StrAtLine<'a>>> {
    pub iter: I,
    pub current_line: Option<CharPositionIterator<'a>>,
}

impl<'a, I: Iterator<Item = StrAtLine<'a>>> CharIterator<'a, I> {
    pub fn new(iter: I) -> Self {
        Self { iter, current_line: None }
    }
}

pub fn char_iterator_from_str<'a>(s: &'a str) -> CharIterator<'a, LineCounterIterator<'a, Lines<'a>>> {
    let iter = LineCounterIterator::new(s.lines());
    CharIterator::new(iter)
}

impl<'a, I: Iterator<Item = StrAtLine<'a>>> Iterator for CharIterator<'a, I> {
    type Item = CharPosition;

    fn next(&mut self) -> Option<Self::Item> {
        // get current line if it hasnt been read from the backing iterator yet:
        let line_chars = if let Some(line) = &mut self.current_line {
            line
        } else {
            let current_line = self.iter.next()?;
            self.current_line.get_or_insert(CharPositionIterator::new(current_line))
        };
        let next_char = line_chars.next();
        if next_char.is_some() {
            return next_char;
        }
        // if we reached the end of the character line, try to go to the next line
        // and return its first character:
        let current_line = self.iter.next()?;
        let mut iter = CharPositionIterator::new(current_line);
        let out = iter.next();
        self.current_line = Some(iter);
        out
    }
}

pub type PeekableCharIterator<'a> = std::iter::Peekable<CharIterator<'a, LineCounterIterator<'a, Lines<'a>>>>;

pub fn parse_json_value_from_iter(char_iter: &mut PeekableCharIterator) -> Result<Value, String> {
    loop {
        let peeked_char = char_iter.peek().ok_or("ran out of characters")?;
        let current_char = if peeked_char.c.is_ascii_whitespace() {
            // ignore whitespace. advance iterator and try next char:
            let _ = char_iter.next();
            continue;
        } else {
            let current_char = peeked_char.clone();
            let _ = char_iter.next();
            current_char
        };
        match current_char.c {
            // parse object
            '{' => todo!(),
            // parse array
            '[' => {
                let pos = current_char.pos;
                let mut vals = vec![];
                // keep parsing values until a ']' is found
                'outter: loop {
                    // check for empty arrays:
                    if let Some(c) = char_iter.peek() {
                        if c.c == ']' {
                            let _ = char_iter.next();
                            break;
                        }
                    }
                    let val = parse_json_value_from_iter(char_iter)?;
                    vals.push(val);
                    // consume whitespace up to a ','
                    loop {
                        if let Some(c) = char_iter.peek() {
                            if c.c.is_ascii_whitespace() {
                                char_iter.next();
                                continue;
                            }
                            if c.c == ',' {
                                // consume the comma, then break out to parse the next
                                // json value
                                char_iter.next();
                                break;
                            }
                            if c.c == ']' {
                                // break out of the outer loop. we're done parsing values
                                break 'outter;
                            }
                        } else { break; }
                    }
                }
                return Ok(Value::Array { pos, val: vals })
            },
            // parse true
            't' => {
                let pos = current_char.pos;
                let expected = ['r', 'u', 'e'];
                for exp_c in expected {
                    let c = char_iter.next().unwrap_or_default();
                    if c.c != exp_c {
                        return Err(format!("expected '{}' when parsing boolean value 'true'. instead found '{}'", exp_c, c.c));
                    }
                }
                return Ok(Value::Bool { pos, val: true })
            },
            // parse false
            'f' => {
                let pos = current_char.pos;
                let expected = ['a', 'l', 's', 'e'];
                for exp_c in expected {        
                    let c = char_iter.next().unwrap_or_default();
                    if c.c != exp_c {
                        return Err(format!("expected '{}' when parsing boolean value 'false'. instead found '{}'", exp_c, c.c));
                    }
                }
                return Ok(Value::Bool { pos, val: false })
            }
            // parse null
            'n' => todo!(),
            // parse string
            '"' => todo!(),
            // parse number
            '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => todo!(),
            invalid_c => {
                return Err(format!("invalid character '{}'", invalid_c))
            },
        }
    }
}

pub fn parse_json_value<'a>(s: &'a str) -> Result<Value, String> {
    let mut char_iter: PeekableCharIterator = char_iterator_from_str(s).peekable();
    parse_json_value_from_iter(&mut char_iter)
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use std::path::PathBuf;
    use super::*;

    #[test]
    fn can_parse_json_arrays() {
        let document = "[]";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Array { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert!(val.is_empty());
        });

        let document = "[false]";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Array { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.len(), 1);
            assert_matches!(val.remove(0), Value::Bool { pos, val } => {
                assert_eq!(pos.line, 0);
                assert_eq!(pos.column, 1);
                assert_eq!(val, false);
            });
        });

        let document = "[true]";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Array { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.len(), 1);
            assert_matches!(val.remove(0), Value::Bool { pos, val } => {
                assert_eq!(pos.line, 0);
                assert_eq!(pos.column, 1);
                assert_eq!(val, true);
            });
        });

        let document = " [\n true, \n  false\n]";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Array { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 1);
            assert_eq!(val.len(), 2);
            assert_matches!(val.remove(0), Value::Bool { pos, val } => {
                assert_eq!(pos.line, 1);
                assert_eq!(pos.column, 1);
                assert_eq!(val, true);
            });
            assert_matches!(val.remove(0), Value::Bool { pos, val } => {
                assert_eq!(pos.line, 2);
                assert_eq!(pos.column, 2);
                assert_eq!(val, false);
            });
        });

        let document = "[[ []]]";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Array { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.len(), 1);
            assert_matches!(val.remove(0), Value::Array { pos, mut val } => {
                assert_eq!(pos.line, 0);
                assert_eq!(pos.column, 1);
                assert_eq!(val.len(), 1);
                assert_matches!(val.remove(0), Value::Array { pos, val } => {
                    assert_eq!(pos.line, 0);
                    assert_eq!(pos.column, 3);
                    assert!(val.is_empty());
                });
            });
        });
    }

    #[test]
    fn char_iteration_works() {
        let document = r#"this is line0
and this is line1"#;
        let mut iter = char_iterator_from_str(document);
        let expected = [
            ('t', 0, 0),
            ('h', 0, 1),
            ('i', 0, 2),
            ('s', 0, 3),
            (' ', 0, 4),
            ('i', 0, 5),
            ('s', 0, 6),
            (' ', 0, 7),
            ('l', 0, 8),
            ('i', 0, 9),
            ('n', 0, 10),
            ('e', 0, 11),
            ('0', 0, 12),
            ('a', 1, 0),
            ('n', 1, 1),
            ('d', 1, 2),
            (' ', 1, 3),
            ('t', 1, 4),
            ('h', 1, 5),
            ('i', 1, 6),
            ('s', 1, 7),
            (' ', 1, 8),
            ('i', 1, 9),
            ('s', 1, 10),
            (' ', 1, 11),
            ('l', 1, 12),
            ('i', 1, 13),
            ('n', 1, 14),
            ('e', 1, 15),
            ('1', 1, 16),
        ];
        for (exp_c, exp_line, exp_col) in expected {
            let char_pos = iter.next()
                .expect(&format!("expected to get character '{}' at line{},col{}", exp_c, exp_line, exp_col));
            assert_eq!(char_pos.c, exp_c);
            assert_eq!(char_pos.pos.column, exp_col);
            assert_eq!(char_pos.pos.line, exp_line);
        }
    }

    /// runs through all the test file cases in JSONTestSuite
    /// ignoring any of the files that start with i_ and
    /// loads them as a string (ignoring any invalid utf8 strings)
    /// and then calls the provided function with the json test case
    /// and the function returns Ok() if parsing succeeds, or an error otherwise.
    /// this function then panics if the expected result doesnt match the parsing result
    fn test_all_json_cases(f: fn(String) -> Result<(), String>) {
        let mut root_workspace_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        // out 1 level and into the crates directory:
        root_workspace_path.pop();
        // out 1 level into the root of the workspace
        root_workspace_path.pop();
        // out of the root workspace:
        root_workspace_path.pop();
        // into the repository with the test cases:
        root_workspace_path.push("JSONTestSuite");
        // directory with the test cases:
        root_workspace_path.push("test_parsing");
        let rd = std::fs::read_dir(&root_workspace_path).expect("failed to read test case directory");
        for de in rd {
            let dir_entry = de.expect("failed to read dir entry");
            let file_name = dir_entry.file_name().to_string_lossy().to_string();
            let first_char = file_name.chars().nth(0).expect("it should have 1 char");
            if first_char != 'y' && first_char != 'n' {
                // ignore files that start with i_
                // they are ambiguous, i dont care about that for now
                continue;
            }
            if !file_name.ends_with(".json") {
                continue;
            }
            let contents = std::fs::read(dir_entry.path()).expect("failed to read test case contents");
            let contents_utf8 = match String::from_utf8(contents) {
                Ok(o) => o,
                Err(_) => {
                    println!("SKIP {} not valid utf8", file_name);
                    continue;
                }
            };
            let json_value = f(contents_utf8);
            match json_value {
                Ok(_) => {
                    if first_char == 'y' {
                        println!("OK {}", file_name);
                    } else {
                        panic!("ER {} expected failure but it parsed", file_name);
                    }
                }
                Err(e) => {
                    if first_char == 'n' {
                        println!("OK {}", file_name);
                    } else {
                        panic!("ER {} expected parse but it failed. parse error: {}", file_name, e);
                    }
                }
            }
        }
    }

    fn parse_json_serde(json_str: String) -> Result<(), String> {
        let _ = serde_json::from_str::<serde_json::Value>(&json_str).map_err(|e| e.to_string())?;
        Ok(())
    }

    #[test]
    #[ignore = "this test assumes you have the minefield test case repository cloned already and is adjacent to the root workspace"]
    fn run_all_minefield_tests_serde() {
        test_all_json_cases(parse_json_serde);
    }
}
