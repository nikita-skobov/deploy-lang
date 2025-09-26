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

impl Number {
    pub fn as_float(&self) -> f64 {
        match self {
            Number::Float(f) => *f,
            Number::Int(i) => *i as f64
        }
    }
    pub fn as_int(&self) -> i64 {
        match self {
            Number::Float(f) => *f as i64,
            Number::Int(i) => *i,
        }
    }
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

pub fn parse_json_number(
    char_iter: &mut PeekableCharIterator,
    first_char: CharPosition
) -> Result<Number, String> {
    let mut has_decimal = false;
    let mut sequence = String::from(first_char.c);
    loop {
        let next_char = match char_iter.peek() {
            Some(c) => {
                // if its numeric, or decimal we can accept it:
                if c.c.is_ascii_digit() || c.c == '.' {
                    let out = *c;
                    let _ = char_iter.next();
                    out
                } else {
                    // reached a non-number. dont take it off the iterator
                    // instead return the sequence we've parsed so far
                    break;
                }
            }
            None => {
                break;
            }
        };
        if next_char.c == '.' {
            has_decimal = true;
        }
        sequence.push(next_char.c);
    }
    if has_decimal {
        let f = sequence.parse::<f64>().map_err(|e| format!("failed to parse json numeric value as number: {:?}", e))?;
        return Ok(Number::Float(f))
    }
    let i = sequence.parse::<i64>().map_err(|e| format!("failed to parse json numeric value as number: {:?}", e))?;
    Ok(Number::Int(i))
}

pub fn parse_json_string(
    char_iter: &mut PeekableCharIterator,
    first_quote: CharPosition,
) -> Result<StringAtLine, String> {
    let line = first_quote.pos.line;
    let col = first_quote.pos.column;
    let mut last_char = first_quote.c;
    let mut out_s = String::new();
    loop {
        let next_char = char_iter.next().ok_or("ran out of characters parsing json string")?;
        if last_char == '\\' {
            // pop the last character and insert the correct
            // unescaped character:
            out_s.pop();
            let char_to_add = match next_char.c {
                't' => '\t',
                'b' => 	'\x08',
                'f' => '\x0c',
                '"' => '"',
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                c => return Err(format!("invalid escape character '\\{}'", c)),
            };
            out_s.push(char_to_add);
            last_char = char_to_add;
            continue;
        }
        if next_char.c == '"' {
            break;
        }
        out_s.push(next_char.c);
        last_char = next_char.c;
    }
    Ok(StringAtLine {
        s: out_s,
        line,
        col
    })
}

pub fn parse_json_object(
    char_iter: &mut PeekableCharIterator,
    _open_brace: CharPosition,
) -> Result<HashMap<StringAtLine, Value>, String> {
    let mut out = HashMap::new();
    loop {
        let peeked_char = char_iter.peek().ok_or("ran out of characters parsing json object")?;
        if peeked_char.c.is_ascii_whitespace() {
            let _ = char_iter.next();
            continue;
        }
        if peeked_char.c == '}' {
            let _ = char_iter.next();
            break;
        }
        // it must be assumed to be a quote char:
        let quote_char = *peeked_char;
        let _ = char_iter.next();
        if quote_char.c != '"' {
            return Err(format!("json object key must be a string, instead found '{}'", quote_char.c));
        }
        let key = parse_json_string(char_iter, quote_char)?;
        // ignore whitespace until we get to a colon character
        let colon_char = loop {
            let next_char = char_iter.next().ok_or("ran out of characters parsing json object")?;
            if next_char.c.is_ascii_whitespace() {
                let _ = char_iter.next();
                continue;
            }
            break next_char
        };
        if colon_char.c != ':' {
            return Err(format!("json object key must be followed by a colon, instead found '{}'", colon_char.c));
        }
        // now we can parse until we get a complete value:
        let value = parse_json_value_from_iter(char_iter)?;
        out.insert(key, value);
        // ignore whitespace and capture the comma before going to the next iteration:
        loop {
            let peeked_char = char_iter.peek().ok_or("ran out of characters parsing json object")?;
            if peeked_char.c.is_ascii_whitespace() {
                let _ = char_iter.next();
                continue;
            }
            if peeked_char.c == ',' {
                let _ = char_iter.next();
                break;
            }
            // anything else, we break without capturing it
            // so that the next iteration of the outer loop
            // can error, or if its a } can exit
            break;
        }
    }
    Ok(out)
}

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
            '{' => {
                let pos = current_char.pos;
                return parse_json_object(char_iter, current_char)
                    .map(|val| Value::Object { pos, val })
            }
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
            'n' => {
                let pos = current_char.pos;
                let expected = ['u', 'l', 'l'];
                for exp_c in expected {
                    let c = char_iter.next().unwrap_or_default();
                    if c.c != exp_c {
                        return Err(format!("expected '{}' when parsing null value. instead found '{}'", exp_c, c.c));
                    }
                }
                return Ok(Value::Null { pos, val: () })
            },
            // parse string
            '"' => {
                let pos = current_char.pos;
                return parse_json_string(char_iter, current_char)
                    .map(|val| Value::String { pos, val })
            }
            // parse number
            '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let pos = current_char.pos;
                return parse_json_number(char_iter, current_char)
                    .map(|val| Value::Number { pos, val })
            }
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
    fn can_parse_objects() {
        let document = "{}";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert!(val.is_empty());
        });

        let document = r#"{"key":"val"}"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.len(), 1);
            for (key, val) in val.drain() {
                assert_eq!(key.s, "key");
                assert_matches!(val, Value::String { pos, val } => {
                    assert_eq!(val.s, "val");
                    assert_eq!(pos.line, 0);
                    assert_eq!(pos.column, 7);
                });
            }
        });

        let document = r#"{"key":"val","zz":{}}"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.len(), 2);
            let mut vals: Vec<(StringAtLine, Value)> = val.drain().collect();
            vals.sort_by(|a, b| a.0.s.cmp(&b.0.s));
            let next = vals.remove(0);
            assert_eq!(next.0.s, "key");
            assert_matches!(next.1, Value::String { val, .. } => {
                assert_eq!(val.s, "val");
            });
            let next = vals.remove(0);
            assert_eq!(next.0.s, "zz");
            assert_eq!(next.0.col, 13);
            assert_matches!(next.1, Value::Object { pos, val } => {
                assert!(val.is_empty());
                assert_eq!(pos.column, 18);
            });
        });

        let document = r#"
        {
          "key":"val",
          "zz":{}
        }"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, mut val } => {
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 8);
            assert_eq!(val.len(), 2);
            let mut vals: Vec<(StringAtLine, Value)> = val.drain().collect();
            vals.sort_by(|a, b| a.0.s.cmp(&b.0.s));
            let next = vals.remove(0);
            assert_eq!(next.0.s, "key");
            assert_matches!(next.1, Value::String { val, .. } => {
                assert_eq!(val.s, "val");
            });
            let next = vals.remove(0);
            assert_eq!(next.0.s, "zz");
            assert_eq!(next.0.col, 10);
            assert_matches!(next.1, Value::Object { pos, val } => {
                assert!(val.is_empty());
                assert_eq!(pos.column, 15);
            });
        });

        let document = r#"{"key":false}"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.len(), 1);
            for (key, val) in val.drain() {
                assert_eq!(key.s, "key");
                assert_matches!(val, Value::Bool { pos, val } => {
                    assert_eq!(val, false);
                    assert_eq!(pos.line, 0);
                    assert_eq!(pos.column, 7);
                });
            }
        });
    }

    #[test]
    fn can_parse_numbers() {
        let document = "0";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Number { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.as_int(), 0);
        });

        let document = "-10";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Number { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.as_int(), -10);
        });

        let document = "-1.2";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Number { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.as_float(), -1.2);
        });
    }

    #[test]
    fn can_parse_null() {
        let document = "null";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Null { pos, .. } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
        });

        let document = "\n   null";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Null { pos, .. } => {
            assert_eq!(pos.line, 1);
            assert_eq!(pos.column, 3);
        });
    }

    #[test]
    fn can_parse_strings() {
        let document = r#""""#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::String { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert_eq!(val.s, "");
        });

        let document = r#" "hello""#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::String { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 1);
            assert_eq!(val.s, "hello");
        });

        let document = r#" "t\"in\tner quote\"""#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::String { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 1);
            assert_eq!(val.s, "t\"in\tner quote\"");
        });
    }

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
