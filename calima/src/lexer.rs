use std::str::Chars;
use crate::token::{Token, Location};
use crate::lexer::ErrorKind::TabIndent;
use crate::token::Token::StringLiteral;

pub struct Lexer<'input> {
    chars: Chars<'input>,
    input: &'input str,
    line: usize,
    pos: usize,
    col: usize,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ErrorKind {
    TabIndent
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Error {
    location: Location,
    kind: ErrorKind
}

type LexerResult<'input> = Result<(Location, Token<'input>, Location), Error>;

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            input,
            chars: input.chars(),
            line: 1,
            pos: 0,
            col: 1
        }
    }

    fn current_pos(&self) -> Location {
        Location {
            line: self.line,
            col: self.col,
            pos: self.pos
        }
    }

    fn incr_pos(&mut self) {
        self.col += 1;
        self.pos += 1;
    }

    fn incr_line(&mut self) {
        self.col = 1;
        self.line += 1;
        self.pos += 1;
    }

    fn comment(&mut self) {
        loop {
            match self.chars.next() {
                None => break,
                Some('\n') => {
                    self.incr_line();
                    break;
                }
                Some(_) => self.incr_pos()
            }
        }
    }

    fn string_literal(&mut self) -> Option<LexerResult<'input>> {
        let start = self.current_pos();
        let start_idx = self.pos;
        let end_idx = loop {
            match self.chars.next() {
                None => break(self.pos),
                Some('"') => break(self.pos),
                Some(_) => ()
            }
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.current_pos();
        Some(Ok((start, StringLiteral(lit), end)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexerResult<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.chars.next() {
                None => break None,
                Some(' ') => self.incr_pos(),
                Some('\n') => self.incr_line(),
                Some('\r') => self.incr_pos(),
                Some('\t') => break Some(Err(Error { location: self.current_pos(), kind: TabIndent })),
                Some('#') => self.comment(),
                Some('"') => break self.string_literal(),
                Some(_) => ()
            }
        }
    }
}