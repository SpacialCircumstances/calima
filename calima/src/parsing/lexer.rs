use crate::parsing::token::Token::*;
use crate::parsing::token::{Location, NumberFormat, Span, Token};
use crate::StringInterner;
use quetta::Text;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'input> {
    interner: &'input StringInterner,
    chars: Peekable<Chars<'input>>,
    input: &'input str,
    last_pos: Location,
    current_pos: Location,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ErrorKind {
    InvalidNumber,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::InvalidNumber => write!(f, "Invalid number format"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Error {
    pub location: Span,
    pub kind: ErrorKind,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexer error {} in position: {}",
            self.kind, self.location
        )
    }
}

type LexerResult = Result<(Location, Token, Location), Error>;

fn is_separator(c: char, is_op: bool) -> bool {
    match c {
        ',' => true,
        '#' => true,
        '"' => true,
        '(' => true,
        ')' => true,
        '[' => true,
        ']' => true,
        '{' => true,
        '}' => true,
        ':' => true,
        '`' => true,
        '.' => true,
        c if c.is_whitespace() => true,
        _ => is_op ^ c.is_ascii_punctuation(),
    }
}

fn single_char_token(c: char) -> Option<Token> {
    match c {
        ':' => Some(Colon),
        ',' => Some(Comma),
        '(' => Some(ParenOpen),
        ')' => Some(ParenClose),
        '{' => Some(CurlyBraceOpen),
        '}' => Some(CurlyBraceClose),
        '[' => Some(SquareBracketOpen),
        ']' => Some(SquareBracketClose),
        '`' => Some(Backtick),
        '\'' => Some(Apostrophe),
        '.' => Some(Period),
        _ => None,
    }
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str, interner: &'input StringInterner) -> Self {
        let starting_pos = Location {
            pos: 0,
            line: 1,
            col: 1,
        };
        Lexer {
            input,
            chars: input.chars().peekable(),
            last_pos: starting_pos,
            current_pos: starting_pos,
            interner,
        }
    }

    fn token_start_pos(&self) -> Location {
        self.last_pos
    }

    fn incr_pos(&mut self) {
        self.last_pos = self.current_pos;
        self.current_pos.col += 1;
        self.current_pos.pos += 1;
    }

    fn incr_line(&mut self) {
        self.last_pos = self.current_pos;
        self.current_pos.col = 1;
        self.current_pos.line += 1;
        self.current_pos.pos += 1;
    }

    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            None => None,
            Some('\n') => {
                self.incr_line();
                Some('\n')
            }
            Some(x) => {
                self.incr_pos();
                Some(x)
            }
        }
    }

    fn comment(&mut self) {
        loop {
            match self.advance() {
                None => break,
                Some('\n') => {
                    break;
                }
                Some(_) => (),
            }
        }
    }

    fn handle_identifier(&mut self, ident: &str) -> Token {
        match ident {
            "do" => Do,
            "let" => Let,
            "rec" => Rec,
            "in" => In,
            "fun" => Fun,
            "if" => If,
            "then" => Then,
            "else" => Else,
            "case" => Case,
            "of" => Of,
            "end" => End,
            "type" => Type,
            "import" => Import,
            "opening" => Opening,
            "->" => Arrow,
            "=" => Equal,
            "|" => Pipe,
            "_" => Underscore,
            "true" => BooleanLiteral(true),
            "false" => BooleanLiteral(false),
            "infix" => Infix,
            "prefix" => Prefix,
            "public" => Public,
            x => {
                let first = x
                    .chars()
                    .next()
                    .expect(format!("Fatal Error: Unrecognized identifier '{}'", ident).as_ref());
                let intern_ident = self.interner.intern(Text::new(ident));
                match first {
                    c if c.is_alphabetic() && c.is_uppercase() => TypeIdentifier(intern_ident),
                    c if c.is_alphabetic() => NameIdentifier(intern_ident),
                    _ => OperatorIdentifier(intern_ident),
                }
            }
        }
    }

    fn string_literal(&mut self) -> Option<LexerResult> {
        //TODO: More escape stuff
        let start = self.token_start_pos();
        let start_idx = self.current_pos.pos;
        let mut escaped = false;
        let end_idx = loop {
            match self.advance() {
                None => break (self.current_pos.pos),
                Some('\\') => escaped = true,
                Some('"') => {
                    if !escaped {
                        break (self.current_pos.pos - 1);
                    } else {
                        escaped = false
                    }
                }
                Some(_) => (),
            }
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.token_start_pos();
        Some(Ok((
            start,
            StringLiteral(self.interner.intern(Text::new(lit))),
            end,
        )))
    }

    fn number_literal(&mut self) -> Option<LexerResult> {
        //TODO: Improve literals
        let start = self.token_start_pos();
        let start_idx = self.current_pos.pos - 1;
        let end_idx = loop {
            match self.chars.peek() {
                None => break (self.current_pos.pos),
                Some('.') => (),
                Some(x) if x.is_numeric() => (),
                _ => break (self.current_pos.pos),
            }
            self.advance();
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.token_start_pos();
        let format = match lit.contains('.') {
            true => NumberFormat::Float,
            false => NumberFormat::Integer,
        };
        Some(Ok((
            start,
            NumberLiteral((self.interner.intern(Text::new(lit)), format)),
            end,
        )))
    }

    fn identifier(&mut self) -> Option<LexerResult> {
        let start = self.token_start_pos();
        let start_idx = self.current_pos.pos - 1;
        let is_operator = &self.input[start_idx..]
            .chars()
            .next()
            .expect("Identifier of length zero?")
            .is_ascii_punctuation();
        let end_idx = loop {
            match self.chars.peek() {
                None => break (self.current_pos.pos),
                Some(&c) if is_separator(c, *is_operator) => break (self.current_pos.pos),
                Some(_) => (),
            }
            self.advance();
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.token_start_pos();
        Some(Ok((start, self.handle_identifier(lit), end)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let last_pos = self.token_start_pos();
            match self.advance() {
                None => break None,
                Some(' ') => (),
                Some('\n') => (),
                Some('\r') => (),
                Some('\t') => (),
                Some('#') => self.comment(),
                Some('"') => break self.string_literal(),
                Some('-') => {
                    if let Some(t) = self.chars.peek() {
                        if t.is_numeric() {
                            break self.number_literal();
                        } else {
                            break self.identifier();
                        }
                    }
                }
                Some(x) => {
                    if x.is_numeric() {
                        break self.number_literal();
                    }
                    if let Some(t) = single_char_token(x) {
                        break Some(Ok((last_pos, t, self.token_start_pos())));
                    }
                    break self.identifier();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::TypeAnnotation::Name;
    use crate::parsing::lexer::Lexer;
    use crate::parsing::token::Token::*;
    use crate::parsing::token::{NumberFormat, Token};
    use crate::StringInterner;

    fn lex_equal(code: &str, tokens: Vec<Token>, interner: &StringInterner) {
        let lexer = Lexer::new(code, interner);
        let res: Vec<Token> = lexer.map(|tk| tk.unwrap()).map(|(_, t, _)| t).collect();
        assert_eq!(res, tokens);
    }

    #[test]
    fn lex1() {
        let interner = StringInterner::new();
        let code = "ab cd, : ->";
        let tokens = vec![
            NameIdentifier(interner.intern_str("ab")),
            NameIdentifier(interner.intern_str("cd")),
            Comma,
            Colon,
            Arrow,
        ];
        lex_equal(code, tokens, &interner)
    }

    #[test]
    fn lex2() {
        let interner = StringInterner::new();
        let code = "\"test\" test \"asdf\\\"\"";
        let tokens = vec![
            StringLiteral(interner.intern_str("test")),
            NameIdentifier(interner.intern_str("test")),
            StringLiteral(interner.intern_str("asdf\\\"")),
        ];
        lex_equal(code, tokens, &interner)
    }

    #[test]
    fn lex3() {
        let interner = StringInterner::new();
        let code = "12.3 344.45, 9900 -3";
        let tokens = vec![
            NumberLiteral((interner.intern_str("12.3"), NumberFormat::Float)),
            NumberLiteral((interner.intern_str("344.45"), NumberFormat::Float)),
            Comma,
            NumberLiteral((interner.intern_str("9900"), NumberFormat::Integer)),
            NumberLiteral((interner.intern_str("-3"), NumberFormat::Integer)),
        ];
        lex_equal(code, tokens, &interner)
    }

    #[test]
    fn lex4() {
        let interner = StringInterner::new();
        let code = "case x of | a:Int -> 2";
        let tokens = vec![
            Case,
            NameIdentifier(interner.intern_str("x")),
            Of,
            Pipe,
            NameIdentifier(interner.intern_str("a")),
            Colon,
            TypeIdentifier(interner.intern_str("Int")),
            Arrow,
            NumberLiteral((interner.intern_str("2"), NumberFormat::Integer)),
        ];
        lex_equal(code, tokens, &interner);
    }

    #[test]
    fn lex5() {
        let interner = StringInterner::new();
        let code = "{ a = \"test\", b = 12.4, c = d (a b) }";
        let tokens = vec![
            CurlyBraceOpen,
            NameIdentifier(interner.intern_str("a")),
            Equal,
            StringLiteral(interner.intern_str("test")),
            Comma,
            NameIdentifier(interner.intern_str("b")),
            Equal,
            NumberLiteral((interner.intern_str("12.4"), NumberFormat::Float)),
            Comma,
            NameIdentifier(interner.intern_str("c")),
            Equal,
            NameIdentifier(interner.intern_str("d")),
            ParenOpen,
            NameIdentifier(interner.intern_str("a")),
            NameIdentifier(interner.intern_str("b")),
            ParenClose,
            CurlyBraceClose,
        ];
        lex_equal(code, tokens, &interner)
    }

    #[test]
    fn lex6() {
        let interner = StringInterner::new();
        let code = "if (x == asdf) then fun a -> a else 12";
        let tokens = vec![
            If,
            ParenOpen,
            NameIdentifier(interner.intern_str("x")),
            OperatorIdentifier(interner.intern_str("==")),
            NameIdentifier(interner.intern_str("asdf")),
            ParenClose,
            Then,
            Fun,
            NameIdentifier(interner.intern_str("a")),
            Arrow,
            NameIdentifier(interner.intern_str("a")),
            Else,
            NumberLiteral((interner.intern_str("12"), NumberFormat::Integer)),
        ];
        lex_equal(code, tokens, &interner);
    }

    #[test]
    fn lex7() {
        let interner = StringInterner::new();
        let code = "
test #asdf d.
      , (
      #)
#";
        let tokens = vec![
            NameIdentifier(interner.intern_str("test")),
            Comma,
            ParenOpen,
        ];
        lex_equal(code, tokens, &interner);
    }

    #[test]
    fn lex8() {
        let interner = StringInterner::new();
        let code = "map (fun i -> i + 2) x";
        let tokens = vec![
            NameIdentifier(interner.intern_str("map")),
            ParenOpen,
            Fun,
            NameIdentifier(interner.intern_str("i")),
            Arrow,
            NameIdentifier(interner.intern_str("i")),
            OperatorIdentifier(interner.intern_str("+")),
            NumberLiteral((interner.intern_str("2"), NumberFormat::Integer)),
            ParenClose,
            NameIdentifier(interner.intern_str("x")),
        ];
        lex_equal(code, tokens, &interner);
    }

    #[test]
    fn lex9() {
        let interner = StringInterner::new();
        let code = "println (\"Hello\" ++ \"World!\")";
        let tokens = vec![
            NameIdentifier(interner.intern_str("println")),
            ParenOpen,
            StringLiteral(interner.intern_str("Hello")),
            OperatorIdentifier(interner.intern_str("++")),
            StringLiteral(interner.intern_str("World!")),
            ParenClose,
        ];
        lex_equal(code, tokens, &interner);
    }

    #[test]
    fn lex10() {
        let interner = StringInterner::new();
        let code = "a.T.b reg ++ test";
        let tokens = vec![
            NameIdentifier(interner.intern_str("a")),
            Period,
            TypeIdentifier(interner.intern_str("T")),
            Period,
            NameIdentifier(interner.intern_str("b")),
            NameIdentifier(interner.intern_str("reg")),
            OperatorIdentifier(interner.intern_str("++")),
            NameIdentifier(interner.intern_str("test")),
        ];
        lex_equal(code, tokens, &interner);
    }

    #[test]
    fn lex11() {
        let interner = StringInterner::new();
        let code = "!a+b";
        let tokens = vec![
            OperatorIdentifier(interner.intern_str("!")),
            NameIdentifier(interner.intern_str("a")),
            OperatorIdentifier(interner.intern_str("+")),
            NameIdentifier(interner.intern_str("b")),
        ];
        lex_equal(code, tokens, &interner);
    }
}
