use std::{collections::HashMap, ops::Index, str::{Chars, Lines}};

use str_at_line::{LineCounterIterator, StrAtLine, StringAtLine};

/// we cant represent a json_with_positions::Value as a serde_json::Value because of the JsonPath,
/// so we treat JsonPaths as an object with just this one key, and the value is a string
/// containing the json path query
const PATH_QUERY_KEY: &str = "__DCL_PATH_QUERY_PRIVATE_FIELD_DO_NOT_USE__";


#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null { pos: Position, val: () },
    Bool { pos: Position, val: bool },
    Number { pos: Position, val: Number },
    String { pos: Position, val: StringAtLine },
    Array { pos: Position, val: Vec<Value> },
    Object { pos: Position, val: HashMap<StringAtLine, Value> },
    /// not json, so in the future perhaps this crate should hide this behind
    /// a feature flag, but for my purposes, i want to support { "field": $.json.path }
    JsonPath { pos: Position, val: StringAtLine },
}

impl Value {
    /// converts a value into a serde json value
    pub fn to_serde_json_value(self) -> serde_json::Value {
        convert_to_serde_value_recursively(self)
    }
    pub fn to_serde_json_value_with_replace_func(
        &self,
        replacement_func: &mut impl FnMut(&str) -> Result<serde_json::Value, String>
    ) -> Result<serde_json::Value, String> {
        convert_to_serde_value_recursively_with_replacement_func(self, replacement_func)
    }
    /// converts a value to a serde json value only if self has no JsonPaths.
    /// otherwise returns None
    pub fn to_serde_json_value_pure(&self) -> Option<serde_json::Value> {
        let converted = convert_to_serde_value_recursively(self.clone());
        if has_a_path_query_key(&converted) {
            return None
        }
        Some(converted)
    }
    pub fn get_all_json_paths(&self) -> Vec<StringAtLine> {
        match self {
            Value::Null { .. } => vec![],
            Value::Bool { .. } => vec![],
            Value::Number { .. } => vec![],
            Value::String { .. } => vec![],
            Value::Array { val, .. } => {
                let mut out = vec![];
                for v in val {
                    out.extend(v.get_all_json_paths());
                }
                out
            }
            Value::Object { val, .. } => {
                let mut out = vec![];
                for (_k, v) in val {
                    out.extend(v.get_all_json_paths());
                }
                out
            }
            Value::JsonPath { val, .. } => vec![val.clone()],
        }
    }
}

impl<I: ValueIndexable> Index<I> for Value {
    type Output = Value;

    fn index(&self, index: I) -> &Self::Output {
        static NULL: &Value = &Value::Null { pos: Position { line: 0, column: 0 }, val: () };
        index.index(self).unwrap_or(NULL)
    }
}

pub trait ValueIndexable {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value>;
}

impl ValueIndexable for usize {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value> {
        match v {
            Value::Array { val, .. } => {
                val.get(*self)
            }
            _ => None,
        }
    }
}

impl ValueIndexable for str {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value> {
        match v {
            Value::Object { val, .. } => {
                val.get(self)
            }
            _ => None,
        }
    }
}
impl ValueIndexable for &str {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value> {
        match v {
            Value::Object { val, .. } => {
                val.get(*self)
            }
            _ => None,
        }
    }
}

pub fn has_a_path_query_key(json_obj: &serde_json::Value) -> bool {
    match json_obj {
        serde_json::Value::Null => false,
        serde_json::Value::Bool(_) => false,
        serde_json::Value::Number(_) => false,
        serde_json::Value::String(_) => false,
        serde_json::Value::Array(values) => {
            values.iter().any(|x| has_a_path_query_key(x))
        }
        serde_json::Value::Object(map) => {
            for (key, val) in map.iter() {
                if key == PATH_QUERY_KEY {
                    return true;
                }
                if has_a_path_query_key(val) {
                    return true;
                }
            }
            false
        }
    }
}

/// converts a value into a serde json value recursively, converting any
/// json path queries to an object representation via the key `PATH_QUERY_KEY`
pub fn convert_to_serde_value_recursively(dynamic_json_val: Value) -> serde_json::Value {
    match dynamic_json_val {
        Value::Null { .. } => serde_json::Value::Null,
        Value::Bool { val, .. } => serde_json::Value::Bool(val),
        Value::Number { val, .. } => {
            let num = match val {
                Number::Float(f) => serde_json::Number::from_f64(f),
                Number::Int(i) => serde_json::Number::from_i128(i as _),
            }.unwrap_or(serde_json::Number::from(0));
            serde_json::Value::Number(num)
        },
        Value::String { val, .. } => serde_json::Value::String(val.s),
        Value::Array { val, .. } => {
            let mut out = Vec::with_capacity(val.len());
            for val in val {
                out.push(convert_to_serde_value_recursively(val));
            }
            serde_json::Value::Array(out)
        }
        Value::Object { val, .. } => {
            let mut out = serde_json::Map::with_capacity(val.len());
            for (key, val) in val {
                out.insert(key.s, convert_to_serde_value_recursively(val));
            }
            serde_json::Value::Object(out)
        }
        Value::JsonPath { val, .. } => {
            let mut out = serde_json::Map::new();
            out.insert(PATH_QUERY_KEY.to_string(), serde_json::Value::String(val.s));
            serde_json::Value::Object(out)
        }
    }
}

/// converts a value into a serde json value recursively, converting any
/// json path by calling the replacement function with the json path as a string.
/// returns an error if the replacement function errors
pub fn convert_to_serde_value_recursively_with_replacement_func(
    dynamic_json_val: &Value,
    replacement_func: &mut impl FnMut(&str) -> Result<serde_json::Value, String>,
) -> Result<serde_json::Value, String> {
    match dynamic_json_val {
        Value::Null { .. } => Ok(serde_json::Value::Null),
        Value::Bool { val, .. } => Ok(serde_json::Value::Bool(*val)),
        Value::Number { val, .. } => {
            let num = match val {
                Number::Float(f) => serde_json::Number::from_f64(*f),
                Number::Int(i) => serde_json::Number::from_i128(*i as _),
            }.unwrap_or(serde_json::Number::from(0));
            Ok(serde_json::Value::Number(num))
        },
        Value::String { val, .. } => Ok(serde_json::Value::String(val.s.clone())),
        Value::Array { val, .. } => {
            let mut out = Vec::with_capacity(val.len());
            for val in val {
                out.push(convert_to_serde_value_recursively_with_replacement_func(val, replacement_func)?);
            }
            Ok(serde_json::Value::Array(out))
        }
        Value::Object { val, .. } => {
            let mut out = serde_json::Map::with_capacity(val.len());
            for (key, val) in val {
                out.insert(key.s.clone(), convert_to_serde_value_recursively_with_replacement_func(val, replacement_func)?);
            }
            Ok(serde_json::Value::Object(out))
        }
        Value::JsonPath { val, .. } => {
            let replaced_val = replacement_func(&val.s)?;
            Ok(replaced_val)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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


pub type PeekableCharIterator<'a, I> = std::iter::Peekable<CharIterator<'a, I>>;

pub fn parse_json_number<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
    first_char: CharPosition
) -> Result<Number, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
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

pub fn parse_until_closing_char<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
    closing_char: char
) -> Result<String, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let mut out = String::new();
    loop {
        let next = char_iter.next().ok_or("ran out of characters parsing json path")?;
        out.push(next.c);
        if next.c == closing_char {
            break;
        }
    }
    Ok(out)
}

pub fn parse_until_non_digit<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>
) -> Result<String, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let mut out = String::new();
    loop {
        match char_iter.peek() {
            Some(c) => {
                if c.c.is_ascii_digit() {
                    out.push(c.c);
                    let _ = char_iter.next();
                } else {
                    break;
                }
            }
            None => break,
        }
    }
    Ok(out)
}

pub fn parse_bracketed_segment<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>
) -> Result<String, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let mut out = String::new();
    let err = "ran out of characters parsing bracketed json path selector";
    let next_char = char_iter.next().ok_or(err)?;
    if next_char.c == '*' {
        out.push(next_char.c);
    } else if next_char.c == '\'' {
        out.push(next_char.c);
        out.push_str(&parse_until_closing_char(char_iter, '\'')?);
    } else if next_char.c == '"' {
        out.push(next_char.c);
        out.push_str(&parse_until_closing_char(char_iter, '"')?);
    } else if next_char.c.is_ascii_digit() || next_char.c == '-' {
        out.push(next_char.c);
        // parse as an index: keep parsing until we get a non digit
        out.push_str(&parse_until_non_digit(char_iter)?);
    }
    let closing_bracket = char_iter.next().ok_or(err)?;
    if closing_bracket.c != ']' {
        return Err(format!("expected closing bracket ']' instead found '{}'", closing_bracket.c));
    }
    out.push(closing_bracket.c);
    Ok(out)
}

// i was too lazy to read the rfc closely
// but i think its basically only certain characters are allowed
// in shorthand selectors, so im going to say "only allow alphanumeric characters and _ and -"
pub fn parse_member_shorthand<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
    first_char: CharPosition,
) -> Result<String, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let mut out = String::new();
    if !first_char.c.is_alphanumeric() {
        return Err(format!("failed to parse json path shorthand: non alphanumeric char '{}'", first_char.c));
    }
    out.push(first_char.c);
    loop {
        match char_iter.peek() {
            Some(c) => {
                if c.c.is_alphanumeric() || c.c == '_' || c.c == '-' {
                    out.push(c.c);
                    let _ = char_iter.next();
                } else {
                    break;
                }
            }
            None => break,
        }
    }
    Ok(out)
}

pub fn parse_json_path_segment<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
) -> Result<Option<String>, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let mut segment = String::new();
    // this isnt strictly rfc9535 compliant but i dont care
    // https://www.rfc-editor.org/rfc/rfc9535.pdf
    // after the root dollar sign, we can have either a child segment or descendant segment
    // if its not a dot or a [ then we can exit since its not part of the path segment:
    let next_char = if let Some(peeked) = char_iter.peek() {
        if peeked.c != '.' && peeked.c != '[' {
            return Ok(None);
        }
        // clone and consume from iterator
        let nc = peeked.clone();
        let _ = char_iter.next();
        nc
    } else {
        return Ok(None);
    };

    if next_char.c == '.' {
        segment.push(next_char.c);
        // check if the following char is a dot as well
        // if it is, then we're parsing a descendant segment
        if let Some(c) = char_iter.peek() {
            if c.c == '.' {
                let _ = char_iter.next();
                segment.push('.');
                // now we know to parse the next either as
                // bracketed_segment, wildcard_selector, or member_shorthand
                let next_char = char_iter.next().ok_or("ran out of characters parsing json path")?;
                if next_char.c == '[' {
                    // parse as bracketed_segment
                    segment.push('[');
                    segment.push_str(&parse_bracketed_segment(char_iter)?);
                } else if next_char.c == '*' {
                    // its a wildcard selector, just grab it and done:
                    segment.push('*');
                } else {
                    // parse as member shorthand
                    segment.push_str(&parse_member_shorthand(char_iter, next_char)?);
                }
            } else {
                // its a child segment, so next must be either
                // wildcard selector or member_shorthand
                let next_char = char_iter.next().ok_or("ran out of characters parsing json path")?;
                if next_char.c == '*' {
                    segment.push('*');
                } else {
                    // parse as member shorthand
                    segment.push_str(&parse_member_shorthand(char_iter, next_char)?);
                }
            }
        }
    } else if next_char.c == '[' {
        // parse as bracketed segment
        segment.push('[');
        segment.push_str(&parse_bracketed_segment(char_iter)?);
    } else {
        return Err(format!("invalid json path character '{}' expected $. or $[", next_char.c));
    }
    Ok(Some(segment))
}

pub fn parse_json_path<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
    first_dollar: CharPosition
) -> Result<StringAtLine, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let mut out = StringAtLine {
        line: first_dollar.pos.line,
        col: first_dollar.pos.column,
        s: String::from('$'),
    };
    while let Some(next_segment) = parse_json_path_segment(char_iter)? {
        out.s.push_str(&next_segment);
    }
    Ok(out)
}

pub fn parse_json_string<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
    first_quote: CharPosition,
) -> Result<StringAtLine, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let line = first_quote.pos.line;
    let col = first_quote.pos.column;
    let mut last_char_is_slash = false;
    let mut out_s = String::new();
    loop {
        let next_char = char_iter.next().ok_or("ran out of characters parsing json string")?;
        if last_char_is_slash {
            // pop the last character and insert the correct
            // unescaped character:
            out_s.pop();
            let char_to_add = match next_char.c {
                't' => '\t',
                '/' => '/',
                'b' => 	'\x08',
                'f' => '\x0c',
                '"' => '"',
                '\\' => '\\',
                'n' => '\n',
                'r' => '\r',
                'u' => {
                    // need to look up next 4 chars as hex digits
                    let hex_digit1 = char_iter.next().ok_or("expected hex digit after \\u")?;
                    let hex_digit2 = char_iter.next().ok_or("expected hex digit after \\u")?;
                    let hex_digit3 = char_iter.next().ok_or("expected hex digit after \\u")?;
                    let hex_digit4 = char_iter.next().ok_or("expected hex digit after \\u")?;
                    let mut s = String::with_capacity(4);
                    s.push(hex_digit1.c);
                    s.push(hex_digit2.c);
                    s.push(hex_digit3.c);
                    s.push(hex_digit4.c);
                    let char_u32 = u32::from_str_radix(&s, 16).map_err(|_| format!("failed to parse \\u{} as escaped character", s))?;
                    char::from_u32(char_u32).ok_or("failed to parse escaped \\u character")?
                },
                c => return Err(format!("invalid escape character '\\{}'", c)),
            };
            out_s.push(char_to_add);
            last_char_is_slash = false;
            continue;
        }
        if next_char.c == '"' {
            break;
        }
        out_s.push(next_char.c);
        last_char_is_slash = next_char.c == '\\';
    }
    Ok(StringAtLine {
        s: out_s,
        line,
        col
    })
}

pub enum ParseStep {
    ParseValue,
    ParseObjectKey,
    ParseArrayNext,
}

#[derive(Debug)]
pub enum ValueState {
    FullyParsedValue(Value),
    PartiallyParsedObject { pos: Position, existing: HashMap<StringAtLine, Value>, current_key: StringAtLine },
    PartiallyParsedArray { pos: Position, existing: Vec<Value> },
}

/// reads characters off the char_iter until one of the characters is found,
/// at which point its returned, and is taken off the iterator.
/// errors if runs out of characters prior to finding one_of_chars
/// or if a character other than one_of_chars is found
pub fn collect_whitespace_until<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
    one_of_chars: &[char]
) -> Result<CharPosition, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    loop {
        let next_char = char_iter.next().ok_or("ran out of characters parsing json value")?;
        if next_char.c.is_ascii_whitespace() {
            continue;
        }
        if one_of_chars.iter().any(|c| *c == next_char.c) {
            return Ok(next_char)
        }
        return Err(format!("unexpected character '{}' expecting one of {:?}", next_char.c, one_of_chars));
    }
}

pub fn parse_sequence<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>,
    expected_sequence: &[char],
    ret_val: Value
) -> Result<Value, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    for exp_c in expected_sequence {
        let c = char_iter.next().unwrap_or_default();
        if c.c != *exp_c {
            return Err(format!("expected '{}' instead found '{}'", exp_c, c.c));
        }
    }
    return Ok(ret_val)
}

pub fn parse_json_value_from_iter_no_recursion<'a, I>(
    char_iter: &mut PeekableCharIterator<'a, I>
) -> Result<Value, String>
    where I: Iterator<Item = StrAtLine<'a>>
{
    let mut parse_stack: Vec<ParseStep> = vec![ParseStep::ParseValue];
    let mut value_stack: Vec<ValueState> = vec![];
    let mut current_object: (Position, HashMap<StringAtLine, Value>) = (Position::default(), HashMap::new());
    let mut current_array: (Position, Vec<Value>) = Default::default();
    let valid_value_chars = [
        '{', '[', '"', 't', 'f', 'n', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        '$',
    ];
    loop {
        if let Some(next_parse_step) = parse_stack.pop() {
            match next_parse_step {
                ParseStep::ParseValue => {
                    // collect whitespace up to the next character that determines the current value
                    let value_start_char = collect_whitespace_until(char_iter, &valid_value_chars)?;
                    match value_start_char.c {
                        '{' => {
                            let obj_next_char = collect_whitespace_until(char_iter, &['}', '"'])?;
                            match obj_next_char.c {
                                '}' => {
                                    // empty object, can push it to the stack now
                                    value_stack.push(ValueState::FullyParsedValue(Value::Object {
                                        pos: value_start_char.pos,
                                        val: HashMap::new(),
                                    }));
                                }
                                _ => {
                                    // must be a quote, parse it, then the colon
                                    // then add it as partially parsed and loop to parse the value after the colon
                                    let key = parse_json_string(char_iter, obj_next_char)?;
                                    collect_whitespace_until(char_iter, &[':'])?;
                                    value_stack.push(ValueState::PartiallyParsedObject {
                                        pos: value_start_char.pos,
                                        existing: HashMap::new(),
                                        current_key: key
                                    });
                                    parse_stack.push(ParseStep::ParseValue);
                                }
                            }
                        },
                        '[' => {
                            // collect whitespace to check if its an empty array:
                            let peeked_char = loop {
                                let peeked_char = char_iter.peek().ok_or("ran out of characters parsing json array")?;
                                if peeked_char.c.is_ascii_whitespace() {
                                    let _ = char_iter.next();
                                    continue;
                                }
                                break peeked_char
                            };
                            if peeked_char.c == ']' {
                                let _ = char_iter.next();
                                // can close the array:
                                value_stack.push(ValueState::FullyParsedValue(Value::Array { pos: value_start_char.pos, val: vec![] }));
                                current_array = Default::default();
                            } else {
                                // parse the next value
                                value_stack.push(ValueState::PartiallyParsedArray { pos: value_start_char.pos, existing: vec![] });
                                parse_stack.push(ParseStep::ParseValue);
                            }
                        },
                        '$' => {
                            let s = parse_json_path(char_iter, value_start_char)?;
                            value_stack.push(ValueState::FullyParsedValue(Value::JsonPath { pos: value_start_char.pos, val: s }));
                        },
                        '"' => {
                            let s = parse_json_string(char_iter, value_start_char)?;
                            value_stack.push(ValueState::FullyParsedValue(Value::String { pos: value_start_char.pos, val: s }));
                        },
                        't' => {
                            let val = parse_sequence(char_iter, &['r', 'u', 'e'], Value::Bool {
                                pos: value_start_char.pos,
                                val: true
                            })?;
                            value_stack.push(ValueState::FullyParsedValue(val));
                        },
                        'f' => {
                            let val = parse_sequence(char_iter, &['a', 'l', 's', 'e'], Value::Bool {
                                pos: value_start_char.pos,
                                val: false
                            })?;
                            value_stack.push(ValueState::FullyParsedValue(val));
                        },
                        'n' => {
                            let val = parse_sequence(char_iter, &['u', 'l', 'l'], Value::Null {
                                pos: value_start_char.pos,
                                val: (),
                            })?;
                            value_stack.push(ValueState::FullyParsedValue(val));
                        },
                        // the rest are all parse as number:
                        _ => {
                            let pos = value_start_char.pos;
                            let val = parse_json_number(char_iter, value_start_char)?;
                            value_stack.push(ValueState::FullyParsedValue(Value::Number { pos, val }));
                        }
                    }
                },
                ParseStep::ParseArrayNext => {
                    // current_array is the array we're parsing. try to get its next value if there is one
                    // or close the array if a ] is found before a ,
                    let c = collect_whitespace_until(char_iter, &[']', ','])?;
                    match c.c {
                        ']' => {
                            // array is done, add it to the stack
                            value_stack.push(ValueState::FullyParsedValue(Value::Array { pos: current_array.0, val: current_array.1 }));
                            current_array = Default::default();
                        }
                        _ => {
                            // its a comma. so we know to parse the next as a value:
                            value_stack.push(ValueState::PartiallyParsedArray {
                                pos: current_array.0,
                                existing: current_array.1,
                            });
                            current_array = Default::default();
                            parse_stack.push(ParseStep::ParseValue);
                        }
                    }
                }
                ParseStep::ParseObjectKey => {
                    // current_object is the object we're parsing, try to get its next key if there is one, or
                    // close the object if a } is found first:
                    let c = collect_whitespace_until(char_iter, &['}', ','])?;
                    match c.c {
                        '}' => {
                            // object is done, add it to the stack
                            value_stack.push(ValueState::FullyParsedValue(Value::Object { pos: current_object.0, val: current_object.1 }));
                            current_object = Default::default();
                        }
                        _ => {
                            // otherwise it was a comma, so now we know to parse the next key of the
                            // object, but first: get the first quote char
                            let quote_char = collect_whitespace_until(char_iter, &['"'])?;
                            let key = parse_json_string(char_iter, quote_char)?;
                            collect_whitespace_until(char_iter, &[':'])?;
                            value_stack.push(ValueState::PartiallyParsedObject {
                                pos: current_object.0,
                                existing: current_object.1,
                                current_key: key
                            });
                            current_object = Default::default();
                            parse_stack.push(ParseStep::ParseValue);
                        }
                    }
                }
            }
            continue;
        }
        // no more steps from the stack, check the value stack and condense into one final value:
        let last_value = value_stack.pop().ok_or("ran out of json values on stack state")?;
        if let Some(prior_value) = value_stack.pop() {
            match prior_value {
                ValueState::FullyParsedValue(value) => {
                    return Err(format!("expected to merge json value {:?} but prior stack item is fully parsed", value));
                },
                ValueState::PartiallyParsedObject { pos, mut existing, current_key } => {
                    if let ValueState::FullyParsedValue(val) = last_value {
                        existing.insert(current_key, val);
                        current_object = (pos, existing);
                        parse_stack.push(ParseStep::ParseObjectKey);
                    } else {
                        return Err(format!("expected to merge json value {:?} but its not fully parsed", last_value));
                    }
                }
                ValueState::PartiallyParsedArray { pos, mut existing } => {
                    if let ValueState::FullyParsedValue(val) = last_value {
                        existing.push(val);
                        current_array = (pos, existing);
                        parse_stack.push(ParseStep::ParseArrayNext);
                    } else {
                        return Err(format!("expected to merge json value {:?} but its not fully parsed", last_value));
                    }
                }
            }
        } else {
            // theres only one item on the value stack, check if its the final value that can be returned:
            if let ValueState::FullyParsedValue(val) = last_value {
                return Ok(val)
            } else {
                return Err(format!("final value on value stack is partial"));
            }
        }
    }
}

pub fn parse_json_value<'a>(s: &'a str) -> Result<Value, String> {
    let mut char_iter: PeekableCharIterator<'a, _> = char_iterator_from_str(s).peekable();
    parse_json_value_from_iter_no_recursion(&mut char_iter)
}

#[cfg(test)]
mod test {
    use assert_matches::assert_matches;
    use std::path::PathBuf;
    use super::*;

    #[test]
    fn can_parse_json_path_query_solo() {
        let value = parse_json_value("$.input.zipfile").unwrap();
        assert_matches!(value, Value::JsonPath { val, .. } => {
            assert_eq!(val.s, "$.input.zipfile");
        });
    }

    #[test]
    fn can_parse_arrays_of_json_path_queries() {
        // simple case first:
        let value = parse_json_value("[$.a, $.input.b]").unwrap();
        assert_matches!(value, Value::Array { val, .. } => {
            assert_eq!(val.len(), 2);
            assert_matches!(&val[0], Value::JsonPath { val, .. } => {
                assert_eq!(val.s, "$.a");
            });
            assert_matches!(&val[1], Value::JsonPath { val, .. } => {
                assert_eq!(val.s, "$.input.b");
            });
        });
        let value = parse_json_value("[$, $.something, $['abc'].x[\"e\"]]").unwrap();
        assert_matches!(value, Value::Array { val, .. } => {
            assert_eq!(val.len(), 3);
            assert_matches!(&val[0], Value::JsonPath { val, .. } => {
                assert_eq!(val.s, "$");
            });
            assert_matches!(&val[1], Value::JsonPath { val, .. } => {
                assert_eq!(val.s, "$.something");
            });
            assert_matches!(&val[2], Value::JsonPath { val, .. } => {
                assert_eq!(val.s, "$['abc'].x[\"e\"]");
            });
        });
    }

    #[test]
    fn can_parse_json_path_queries() {
        let cases = [
            "$.something",
            "$..thing",
            "$[0]",
            "$[100]",
            "$[-1]",
            "$[*]",
            "$.o[*]",
            "$.o['j j']['k.k']",
            r#"$["'"]["@"]"#,
            "$..book[2].publisher",
            "$..*",
            "$.store..price",
        ];
        for case in cases {
            let document = format!(r#"
        {{
            "aquery": {case},
            "other": 2
        }}"#);
            let value = parse_json_value(&document).unwrap();
            assert_matches!(value, Value::Object { mut val, .. } => {
                let mut vals: Vec<(StringAtLine, Value)> = val.drain().collect();
                vals.sort_by(|a, b| a.0.s.cmp(&b.0.s));
                let next = vals.remove(0);
                assert_eq!(next.0.s, "aquery");
                assert_matches!(next.1, Value::JsonPath { val, pos } => {
                    assert_eq!(val.s, case);
                    assert_eq!(pos.line, 2);
                    assert_eq!(pos.column, 22);
                });
            });
        }
    }

    #[test]
    fn can_parse_all_allowed_escapes() {
        let document = r#"["\"\\\/\b\f\n\r\t"]"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Array { .. } => {});
    }

    #[test]
    fn can_parse_double_escape_a() {
        let document = r#"["\\a"]"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Array { mut val, .. } => {
            assert_matches!(val.remove(0), Value::String { val, .. } => {
                assert_eq!(val.s, "\\a");
            })
        });
    }

    #[test]
    fn can_parse_objects_nested() {
        let document = "{}";
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            assert!(val.is_empty());
        });

        let document = r#"{"a":{}}"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 0);
            let mut vals: Vec<(StringAtLine, Value)> = val.drain().collect();
            vals.sort_by(|a, b| a.0.s.cmp(&b.0.s));
            let next = vals.remove(0);
            assert_eq!(next.0.s, "a");
            assert_matches!(next.1, Value::Object { val, .. } => {
                assert!(val.is_empty());
            });
            assert!(val.is_empty());
        });

        let document = r#" {"a":{"b": { "c"  : {"d":{}}, "cprime": "..."  }}}"#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::Object { pos, mut val } => {
            assert_eq!(pos.line, 0);
            assert_eq!(pos.column, 1);
            let mut vals: Vec<(StringAtLine, Value)> = val.drain().collect();
            assert_matches!(vals.remove(0).1, Value::Object { mut val, .. } => {
                let mut vals: Vec<(StringAtLine, Value)> = val.drain().collect();
                assert_matches!(vals.remove(0).1, Value::Object { mut val, .. } => {
                    let mut vals: Vec<(StringAtLine, Value)> = val.drain().collect();
                    vals.sort_by(|a, b| a.0.s.cmp(&b.0.s));
                    let next_val = vals.remove(0);
                    assert_eq!(next_val.0.s, "c");
                    assert_eq!(next_val.0.col, 14);
                    let next_val = vals.remove(0);
                    assert_eq!(next_val.0.s, "cprime");
                    assert_eq!(next_val.0.col, 31);
                });
            });
        });
    }
    
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
    fn garbage_number() {
        let doc = "[-1x]";
        let err = parse_json_value(doc).expect_err("it should err");
        assert!(err.starts_with("unexpected character 'x'"), "it was {}", err);
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
    fn can_parse_escaped_control_char() {
        let document = r#""\u0012""#;
        let value = parse_json_value(document).unwrap();
        assert_matches!(value, Value::String { val, .. } => {
            assert_eq!(val.s, "\u{12}");
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
        let mut errors = vec![];
        let mut success_count = 0;
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
            println!("starting {}", file_name);
            let now = std::time::Instant::now();
            let json_value = f(contents_utf8);
            match json_value {
                Ok(_) => {
                    if first_char == 'y' {
                        success_count += 1;
                        println!("OK {} in {}ms", file_name, now.elapsed().as_millis());
                    } else {
                        println!("Err {} in {}ms", file_name, now.elapsed().as_millis());
                        errors.push(format!("{} should have failed, but parsing succeeded", file_name));
                    }
                }
                Err(e) => {
                    if first_char == 'n' {
                        success_count += 1;
                        println!("OK {} in {}ms", file_name, now.elapsed().as_millis());
                    } else {
                        println!("Err {} in {}ms", file_name, now.elapsed().as_millis());
                        errors.push(format!("{} should have succeeded, but parsing failed: {}", file_name, e));
                    }
                }
            }
        }
        if !errors.is_empty() {
            let report = format!("{}/{} test cases passed", success_count, success_count + errors.len());
            panic!("Found {} errors:\n{:#?}\n{}", errors.len(), errors, report);
        }
    }

    fn parse_json_serde(json_str: String) -> Result<(), String> {
        let _ = serde_json::from_str::<serde_json::Value>(&json_str).map_err(|e| e.to_string())?;
        Ok(())
    }

    fn parse_json_with_positions(json_str: String) -> Result<(), String> {
        parse_json_value(&json_str).map(|_| ())
    }

    #[test]
    #[ignore = "this test assumes you have the minefield test case repository cloned already and is adjacent to the root workspace"]
    fn run_all_minefield_tests_serde() {
        test_all_json_cases(parse_json_serde);
    }

    #[test]
    #[ignore = "this test assumes you have the minefield test case repository cloned already and is adjacent to the root workspace"]
    fn run_all_minefield_tests_json_with_positions() {
        test_all_json_cases(parse_json_with_positions);
    }
}
