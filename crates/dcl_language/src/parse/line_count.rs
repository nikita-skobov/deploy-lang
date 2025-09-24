use std::{fmt::Display, str::CharIndices};

#[derive(Debug, Default, Clone)]
pub struct StrAtLine<'a> {
    pub s: &'a str,
    pub line: usize,
    pub col: usize,
}

impl<'a> Display for StrAtLine<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.s)
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

pub struct PeekableLineCount<I: Iterator> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<I::Item>>,
    counter: usize,
}

impl<I: Iterator> PeekableLineCount<I> {
    pub fn new(iter: I) -> PeekableLineCount<I> {
        Self::new_with_start_line(iter, 0)
    }
    pub fn new_with_start_line(iter: I, start_at: usize) -> PeekableLineCount<I> {
        PeekableLineCount { iter, peeked: None, counter: start_at }
    }
}

impl<I: Iterator> Iterator for PeekableLineCount<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        let res = match self.peeked.take() {
            Some(v) => v,
            None => self.iter.next(),
        };
        if res.is_some() {
            self.counter += 1;
        }
        res
    }
}

impl<I: Iterator> PeekableLineCount<I> {
    pub fn peek(&mut self) -> Option<&I::Item> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_ref()
    }
    pub fn line_index(&self) -> usize {
        self.counter
    }
    /// returns the index of the last line, the index of the line
    /// that was just returned from the prior .next() call.
    /// if .next() has not been called once, this returns 0
    pub fn last_line_index(&self) -> usize {
        if self.counter > 0 {
            self.counter - 1
        } else {
            0
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_count_lines_advanced() {
        let document = r#"line0
line1
line2
line3"#;
        let lines = document.lines();
        let mut peekable_lines = PeekableLineCount::new(lines);
        assert_eq!(peekable_lines.line_index(), 0);
        assert_eq!(peekable_lines.last_line_index(), 0);
        assert_eq!(peekable_lines.peek(), Some("line0").as_ref());
        // no lines have been advanced yet:
        assert_eq!(peekable_lines.line_index(), 0);
        assert_eq!(peekable_lines.last_line_index(), 0);
        assert_eq!(peekable_lines.peek(), Some("line0").as_ref());
        assert_eq!(peekable_lines.line_index(), 0);
        assert_eq!(peekable_lines.next(), Some("line0"));
        assert_eq!(peekable_lines.line_index(), 1);
        assert_eq!(peekable_lines.last_line_index(), 0);

        assert_eq!(peekable_lines.peek(), Some("line1").as_ref());
        assert_eq!(peekable_lines.line_index(), 1);
        assert_eq!(peekable_lines.last_line_index(), 0);
        assert_eq!(peekable_lines.next(), Some("line1"));
        assert_eq!(peekable_lines.line_index(), 2);
        assert_eq!(peekable_lines.last_line_index(), 1);
        assert_eq!(peekable_lines.next(), Some("line2"));
        assert_eq!(peekable_lines.next(), Some("line3"));
        assert_eq!(peekable_lines.line_index(), 4);
        assert_eq!(peekable_lines.last_line_index(), 3);
        // nothing more to peek/advance, should not go past 4:
        assert_eq!(peekable_lines.peek(), None);
        assert_eq!(peekable_lines.line_index(), 4);
        assert_eq!(peekable_lines.next(), None);
        assert_eq!(peekable_lines.next(), None);
        assert_eq!(peekable_lines.line_index(), 4);
        assert_eq!(peekable_lines.line_index(), 4);
        assert_eq!(peekable_lines.last_line_index(), 3);
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
}
