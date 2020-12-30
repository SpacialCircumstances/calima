use std::fmt::{Display, Formatter};
use std::ops::Range;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum NumberFormat {
    Float,
    Integer,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Token<'input> {
    CurlyBraceOpen,
    CurlyBraceClose,
    ParenOpen,
    ParenClose,
    SquareBracketOpen,
    SquareBracketClose,
    Comma,
    NameIdentifier(&'input str),
    TypeIdentifier(&'input str),
    OperatorIdentifier(&'input str),
    StringLiteral(&'input str),
    NumberLiteral((&'input str, NumberFormat)),
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
    Arrow,
    Backtick,
    At,
    Apostrophe,
    Infix,
    Prefix,
}

impl<'input> Display for Token<'input> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Token::CurlyBraceOpen => write!(f, "{{"),
            Token::CurlyBraceClose => write!(f, "}}"),
            Token::ParenOpen => write!(f, "("),
            Token::ParenClose => write!(f, ")"),
            Token::SquareBracketOpen => write!(f, "["),
            Token::SquareBracketClose => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::NameIdentifier(id) => write!(f, "{}", id),
            Token::TypeIdentifier(id) => write!(f, "{}", id),
            Token::OperatorIdentifier(id) => write!(f, "{}", id),
            Token::StringLiteral(lit) => write!(f, "{}", lit),
            Token::NumberLiteral((lit, _)) => write!(f, "{}", lit),
            Token::BooleanLiteral(bool) => write!(f, "{}", bool),
            Token::Do => write!(f, "do"),
            Token::Let => write!(f, "let"),
            Token::Rec => write!(f, "rec"),
            Token::Underscore => write!(f, "_"),
            Token::In => write!(f, "in"),
            Token::Fun => write!(f, "fun"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Case => write!(f, "case"),
            Token::Of => write!(f, "of"),
            Token::End => write!(f, "end"),
            Token::Type => write!(f, "type"),
            Token::Region => write!(f, "region"),
            Token::Equal => write!(f, "="),
            Token::Pipe => write!(f, "|"),
            Token::Import => write!(f, "import"),
            Token::Colon => write!(f, ":"),
            Token::Arrow => write!(f, "->"),
            Token::Backtick => write!(f, "`"),
            Token::At => write!(f, "@"),
            Token::Apostrophe => write!(f, "'"),
            Token::Infix => write!(f, "infix"),
            Token::Prefix => write!(f, "prefix"),
        }
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

#[derive(Debug, Eq, PartialEq, Clone, Copy, Default)]
pub struct Span {
    pub left: Location,
    pub right: Location,
}

pub fn span(left: Location, right: Location) -> Span {
    Span { left, right }
}

impl Span {
    pub fn to_range(&self) -> Range<usize> {
        self.left.pos..self.right.pos + 1
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} - {}", self.left, self.right)
    }
}
