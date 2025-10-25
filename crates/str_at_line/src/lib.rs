use std::{borrow::Borrow, fmt::Display, hash::Hash, str::CharIndices};

pub trait SpannedStr {
    fn get_str<'a>(&'a self) -> &'a str;
    fn get_line(&self) -> usize;
    fn get_col(&self) -> usize;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct StrAtLine<'a> {
    pub s: &'a str,
    pub line: usize,
    pub col: usize,
}

/// owned version of StrAtLine
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct StringAtLine {
    pub s: String,
    pub line: usize,
    pub col: usize,
}

impl<'aa> SpannedStr for StrAtLine<'aa> {
    fn get_str<'a>(&'a self) -> &'a str {
        self.s
    }

    fn get_line(&self) -> usize {
        self.line
    }

    fn get_col(&self) -> usize {
        self.col
    }
}

impl SpannedStr for StringAtLine {
    fn get_str<'a>(&'a self) -> &'a str {
        self.s.as_str()
    }

    fn get_line(&self) -> usize {
        self.line
    }

    fn get_col(&self) -> usize {
        self.col
    }
}

impl<'aa> SpannedStr for &StrAtLine<'aa> {
    fn get_str<'a>(&'a self) -> &'a str {
        self.s
    }

    fn get_line(&self) -> usize {
        self.line
    }

    fn get_col(&self) -> usize {
        self.col
    }
}

impl SpannedStr for &StringAtLine {
    fn get_str<'a>(&'a self) -> &'a str {
        self.s.as_str()
    }

    fn get_line(&self) -> usize {
        self.line
    }

    fn get_col(&self) -> usize {
        self.col
    }
}

impl StringAtLine {
    pub fn pop(&mut self) -> Option<char> {
        let out = self.s.pop();
        out
    }
    pub fn as_str<'a>(&'a self) -> &'a str {
        self.s.as_str()
    }
    pub fn as_borrowed<'a>(&'a self) -> StrAtLine<'a> {
        StrAtLine { s: &self.s, line: self.line, col: self.col }
    }
}

impl From<&str> for StringAtLine {
    fn from(value: &str) -> Self {
        Self { s: value.to_string(), line: 0, col: 0 }
    }
}

impl Hash for StringAtLine {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.s.hash(state);
    }
}

impl Borrow<str> for StringAtLine {
    fn borrow(&self) -> &str {
        &self.s.as_str()
    }
}
impl Borrow<String> for StringAtLine {
    fn borrow(&self) -> &String {
        &self.s
    }
}


impl<'a> Display for StrAtLine<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.s)
    }
}

impl Display for StringAtLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.s)
    }
}

pub trait Splittable {
    fn split_once<'a>(&self, s: &StrAtLine<'a>) -> Option<(StrAtLine<'a>, StrAtLine<'a>)>;
}

impl Splittable for &str {
    fn split_once<'a>(&self, s: &StrAtLine<'a>) -> Option<(StrAtLine<'a>, StrAtLine<'a>)> {
        let (left, right) = s.s.split_once(self)?;
        // ensure columns measured in code point counts, not bytes:
        let left_len = left.chars().count();
        let self_len = self.chars().count();
        // splitting a string thats already on one line means
        // both left and right are on the same line:
        let line = s.line;
        let left = StrAtLine {
            s: left,
            line,
            // left starts at the same column as the source StrAtLine
            col: s.col,
        };
        let right = StrAtLine {
            s: right,
            line,
            // right starts after the length of left + the length of the splitting string
            col: s.col + left_len + self_len,
        };
        Some((left, right))
    }
}

impl Splittable for char {
    fn split_once<'a>(&self, s: &StrAtLine<'a>) -> Option<(StrAtLine<'a>, StrAtLine<'a>)> {
        // inefficient but meh
        let char_str = self.to_string();
        Splittable::split_once(&char_str.as_str(), s)
    }
}

pub struct AsciiWhitespaceIter<'a> {
    pub chars: CharIndices<'a>,
    pub source: &'a str,
    /// the line index where source is within its document
    pub line: usize,
    /// the column where source starts, not always 0 necessarily!
    pub col: usize,
    pub start_byte_index: Option<usize>,
    /// how many times .next() was called on the chars iterator from the source str.
    /// this represents the column position of each substring that is returned
    pub chars_advanced: usize,
}

impl<'a> Iterator for AsciiWhitespaceIter<'a> {
    type Item = StrAtLine<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let (start, end) = loop {
            let (char_index, next_char) = match self.chars.next() {
                Some((i, c)) => (i, c),
                None => match self.start_byte_index {
                    Some(i) => break (i, usize::MAX),
                    None => return None,
                }
            };
            self.chars_advanced += 1;
            if next_char.is_ascii_whitespace() {
                let start = match self.start_byte_index {
                    Some(i) => i,
                    // prevent returning empty strings:
                    // havent encountered a non-whitespace char yet
                    None => continue,
                };
                break (start, char_index);
            } else {
                self.start_byte_index.get_or_insert(char_index);
            }
        };
        // reset so next .next() call will check for start of non-whitespace char
        self.start_byte_index = None;
        let (s, sub1) = unsafe {
            if end == usize::MAX {
                // if we reached the end of the char iterator
                // we dont need to subtract 1: there was no whitespace
                // character at the end
                (self.source.get_unchecked(start..), 0)
            } else {
                (self.source.get_unchecked(start..end), 1)
            }
        };
        // column is:
        // start of the source str (self.col)
        // + number of chars we advanced until we hit a whitespace
        // - sub1 for the whitespace character (if any) that caused us to break out of the loop
        // - the number of chars that we collected in s
        let col = self.col + self.chars_advanced - sub1 - s.chars().count();
        return Some(StrAtLine { s, line: self.line, col })
    }
}

impl<'a> StrAtLine<'a> {
    pub fn to_owned(&self) -> StringAtLine {
        StringAtLine { s: self.s.to_string(), line: self.line, col: self.col }
    }
    pub fn trim(&self) -> StrAtLine<'a> {
        let mut start_index = 0;
        let mut num_chars_removed_from_start = 0;
        let mut did_break = false;
        for (index, char) in self.s.char_indices() {
            start_index = index;
            if !char.is_ascii_whitespace() {
                did_break = true;
                break;
            }
            num_chars_removed_from_start += 1;
        }
        // if we did not break out, then that means the string is entirely ascii whitespace
        // in which case we can return it trimmed to an empty string:
        if !did_break {
            return StrAtLine {
                s: "",
                line: self.line,
                col: self.col
            }
        }
        let mut end_index = None;
        for (index, char) in self.s.char_indices().rev() {
            if !char.is_ascii_whitespace() {
                break;
            }
            end_index = Some(index);
        }
        let end_index = match end_index {
            None if start_index == 0 => return StrAtLine {
                s: self.s,
                line: self.line,
                col: self.col,
            },
            None => usize::MAX,
            Some(i) => i,
        };
        let s = unsafe {
            if end_index == usize::MAX {
                self.s.get_unchecked(start_index..)
            } else {
                self.s.get_unchecked(start_index..end_index)
            }
        };
        StrAtLine {
            s,
            line: self.line,
            col: self.col + num_chars_removed_from_start,
        }
    }
    pub fn split_once<S: Splittable>(&self, s: S) -> Option<(StrAtLine<'a>, StrAtLine<'a>)> {
        s.split_once(self)
    }
    pub fn split_ascii_whitespace(&self) -> impl Iterator<Item = StrAtLine<'a>> {
        AsciiWhitespaceIter {
            chars: self.s.char_indices(),
            source: self.s,
            line: self.line,
            col: self.col,
            start_byte_index: None,
            chars_advanced: 0
        }
    }
    pub fn split_at_checked(&self, index: usize) -> Option<(StrAtLine<'a>, StrAtLine<'a>)> {
        let (left, right) = self.s.split_at_checked(index)?;
        // ensure columns measured in code point counts, not bytes:
        let left_len = left.chars().count();
        // splitting a string thats already on one line means
        // both left and right are on the same line:
        let line = self.line;
        let left = StrAtLine {
            s: left,
            line,
            // left starts at the same column as the source StrAtLine
            col: self.col,
        };
        let right = StrAtLine {
            s: right,
            line,
            // right starts after the length of left
            col: self.col + left_len,
        };
        Some((left, right))
    }
}

impl<'a> PartialEq<&'a str> for StrAtLine<'a> {
    fn eq(&self, other: &&'a str) -> bool {
        self.s == *other
    }
}
impl<'a> PartialEq<&'a str> for &StrAtLine<'a> {
    fn eq(&self, other: &&'a str) -> bool {
        self.s == *other
    }
}

impl PartialEq<&str> for StringAtLine {
    fn eq(&self, other: &&str) -> bool {
        self.s == *other
    }
}
impl PartialEq<&str> for &StringAtLine {
    fn eq(&self, other: &&str) -> bool {
        self.s == *other
    }
}

pub struct LineCounterIterator<'a, I: Iterator<Item = &'a str>> {
    iter: I,
    line: usize,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<StrAtLine<'a>>>,
}


impl<'a, I: Iterator<Item = &'a str>> LineCounterIterator<'a, I> {
    pub fn new(iter: I) -> LineCounterIterator<'a, I> {
        LineCounterIterator { iter, line: 0, peeked: None }
    }
    pub fn peek(&mut self) -> Option<&StrAtLine<'a>> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| {
            let s: &'a str = iter.next()?;
            let line = self.line;
            self.line += 1;
            Some(StrAtLine{ s, line, col: 0 })
        }).as_ref()
    }
}

impl<'a, I: Iterator<Item = &'a str>> Iterator for LineCounterIterator<'a, I> {
    type Item = StrAtLine<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let res = match self.peeked.take() {
            Some(v) => v,
            None => {
                let s: &'a str = self.iter.next()?;
                let line = self.line;
                self.line += 1;
                Some(StrAtLine{ s, line, col: 0 })
            }
        };
        res
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn can_index_hashmap_string_at_line_with_str() {
        let mut map: HashMap<StringAtLine, String> = HashMap::new();
        map.insert(StringAtLine { s: "abc".to_string(), line: 1, col: 2 }, "dsa".to_string());
        assert_eq!(map["abc"], "dsa");
        let s = format!("abc");
        assert_eq!(map[&s], "dsa");
    }

    #[test]
    fn can_get_line_of_substr() {
        let document = r#"line0
this is line1
line2
line3"#;
        let mut iter = LineCounterIterator::new(document.lines());
        let peeked = iter.peek().unwrap();
        assert_eq!(peeked, "line0");
        assert_eq!(peeked.line, 0);
        assert_eq!(peeked.col, 0);
        let peeked = iter.peek().unwrap();
        assert_eq!(peeked, "line0");
        assert_eq!(peeked.line, 0);
        let next = iter.next().unwrap();
        assert_eq!(next.line, 0);
        assert_eq!(next, "line0");
        let peeked = iter.peek().unwrap();
        assert_eq!(peeked, "this is line1");
        assert_eq!(peeked.line, 1);
        let peeked = iter.peek().unwrap();
        assert_eq!(peeked, "this is line1");
        assert_eq!(peeked.line, 1);
        let next = iter.next().unwrap();
        assert_eq!(next.line, 1);
        assert_eq!(next, "this is line1");
        let next = iter.next().unwrap();
        assert_eq!(next.line, 2);
        assert_eq!(next, "line2");
        let next = iter.peek().unwrap();
        assert_eq!(next.line, 3);
        assert_eq!(next, "line3");
        let next = iter.next().unwrap();
        assert_eq!(next.line, 3);
        assert_eq!(next, "line3");
        assert!(iter.peek().is_none());
        assert!(iter.next().is_none());
        assert!(iter.peek().is_none());
    }

    #[test]
    fn can_split_once_str_at_line() {
        let document = "this💙 is line 0";
        let mut iter = LineCounterIterator::new(document.lines());
        let str_at_line = iter.next().unwrap();
        let (left, right) = str_at_line.split_once(" ").unwrap();
        assert_eq!(left.s, "this💙");
        assert_eq!(left.line, 0);
        assert_eq!(left.col, 0);
        assert_eq!(right.s, "is line 0");
        // columns measured by code points, not bytes:
        // if it was bytes this would've been 9
        assert_eq!(right.col, 6);
        assert_eq!(right.line, 0);
    }

    #[test]
    fn can_split_whitespace_str_at_line() {
        let source = " Mary   had\ta little  \n\t lamb";
        let str_at_line = StrAtLine {
            s: source,
            line: 2,
            col: 0,
        };
        let mut iter = str_at_line.split_ascii_whitespace();
        let st = iter.next().unwrap();
        assert_eq!(st.s, "Mary");
        assert_eq!(st.line, 2);
        assert_eq!(st.col, 1);
        let st = iter.next().unwrap();
        assert_eq!(st.s, "had");
        assert_eq!(st.line, 2);
        assert_eq!(st.col, 8);
        let st = iter.next().unwrap();
        assert_eq!(st.s, "a");
        assert_eq!(st.line, 2);
        assert_eq!(st.col, 12);
        let st = iter.next().unwrap();
        assert_eq!(st.s, "little");
        assert_eq!(st.line, 2);
        assert_eq!(st.col, 14);
        let st = iter.next().unwrap();
        assert_eq!(st.s, "lamb");
        assert_eq!(st.line, 2);
        assert_eq!(st.col, 25);
        assert!(iter.next().is_none());
    }

    #[test]
    fn can_split_whitespace_str_at_line_space_at_end() {
        let source = " Mary ";
        let str_at_line = StrAtLine {
            s: source,
            line: 0,
            col: 0,
        };
        let mut iter = str_at_line.split_ascii_whitespace();
        let st = iter.next().unwrap();
        assert_eq!(st.s, "Mary");
        assert_eq!(st.col, 1);
        assert!(iter.next().is_none());
    }

    #[test]
    fn can_split_whitespace_str_unicode_counts() {
        // this is the example from rust get_unchecked
        // we wish to check we're counting the columns correctly
        // when there are variable length characters
        let source = "🗻 h∈llo 🌏🌏";
        let str_at_line = StrAtLine {
            s: source,
            line: 0,
            col: 0,
        };
        let mut iter = str_at_line.split_ascii_whitespace();
        let st = iter.next().unwrap();
        assert_eq!(st.s, "🗻");
        assert_eq!(st.col, 0);
        let st = iter.next().unwrap();
        assert_eq!(st.s, "h∈llo");
        assert_eq!(st.col, 2);
        let st = iter.next().unwrap();
        assert_eq!(st.s, "🌏🌏");
        assert_eq!(st.col, 8);
        assert!(iter.next().is_none());
    }

    #[test]
    fn trim_whitespace_string_ok() {
        let s = StrAtLine {
            s: "   ",
            line: 1,
            col: 2,
        };
        let s = s.trim();
        assert_eq!(s.s, "");
        assert_eq!(s.line, 1);
        assert_eq!(s.col, 2);
    }

    #[test]
    fn trim_whitespace_tab_mixed_string_ok() {
        let s = StrAtLine {
            s: "	 	  ",
            line: 1,
            col: 2,
        };
        let s = s.trim();
        assert_eq!(s.s, "");
        assert_eq!(s.line, 1);
        assert_eq!(s.col, 2);
    }

    #[test]
    fn trim_works_left() {
        let s = StrAtLine {
            s: "   hello",
            line: 1,
            col: 1,
        };
        let s = s.trim();
        assert_eq!(s, "hello");
        assert_eq!(s.line, 1);
        // trimming should have advanced the column because we removed 3
        // spaces, so now the string starts at col 4
        assert_eq!(s.col, 4);
    }

    #[test]
    fn trim_works_right() {
        let s = StrAtLine {
            s: "hello   ",
            line: 1,
            col: 1,
        };
        let s = s.trim();
        assert_eq!(s, "hello");
        assert_eq!(s.line, 1);
        // trimming should have not advanced the column because the
        // trim happened on the right side
        assert_eq!(s.col, 1);
    }

    #[test]
    fn trim_works_unicode() {
        let s = StrAtLine {
            s: " 🗻 ",
            line: 1,
            col: 1,
        };
        let s = s.trim();
        assert_eq!(s, "🗻");
        assert_eq!(s.line, 1);
        assert_eq!(s.col, 2);
    }

    #[test]
    fn pop_works() {
        let mut s = StringAtLine {
            s: "abcd".to_string(),
            line: 1,
            col: 4,
        };
        s.pop();
        assert_eq!(s.as_str(), "abc");
        assert_eq!(s.line, 1);
        assert_eq!(s.col, 4);
    }
}
