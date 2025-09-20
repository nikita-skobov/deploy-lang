pub struct PeekableLineCount<I: Iterator> {
    iter: I,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<I::Item>>,
    counter: usize,
}

impl<I: Iterator> PeekableLineCount<I> {
    pub fn new(iter: I) -> PeekableLineCount<I> {
        PeekableLineCount { iter, peeked: None, counter: 0 }
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
}