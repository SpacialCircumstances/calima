use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Token<'input> {
    CurlyBraceOpen,
    CurlyBraceClose,
    ParenOpen,
    ParenClose,
    SquareBracketOpen,
    SquareBracketClose,
    Period,
    Comma,
    NameIdentifier(&'input str),
    TypeIdentifier(&'input str),
    OperatorIdentifier(&'input str),
    StringLiteral(&'input str),
    NumberLiteral(&'input str),
    BooleanLiteral(bool),
    Do,
    Let,
    Rec,
    Underscore,
    In,
    Fun,
    If,
    Then,
    Else,
    Case,
    Of,
    End,
    Type,
    Region,
    Equal,
    Pipe,
    Import,
    Colon,
    Arrow
}

impl<'input> Display for Token<'input> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct Location {
    pub line: usize,
    pub col: usize,
    pub pos: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Line: {}, Col: {}", self.line, self.col)
    }
}