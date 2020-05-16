use std::str::Chars;
use crate::token::{Token, Location, NumberFormat};
use crate::token::Token::*;
use std::iter::Peekable;
use std::fmt::{Display, Formatter};

pub struct Lexer<'input> {
    chars: Peekable<Chars<'input>>,
    input: &'input str,
    last_pos: Location,
    current_pos: Location
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ErrorKind {
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Error {
    location: Location,
    kind: ErrorKind
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Lexer error {:#?} in position: {}", self.kind, self.location)
    }
}

type LexerResult<'input> = Result<(Location, Token<'input>, Location), Error>;

fn is_separator(c: char) -> bool {
    match c {
        '.' => true,
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
        c if c.is_whitespace() => true,
        _ => false
    }
}

fn single_char_token<'input>(c: char) -> Option<Token<'input>> {
    match c {
        ':' => Some(Colon),
        '.' => Some(Period),
        ',' => Some(Comma),
        '(' => Some(ParenOpen),
        ')' => Some(ParenClose),
        '{' => Some(CurlyBraceOpen),
        '}' => Some(CurlyBraceClose),
        '[' => Some(SquareBracketOpen),
        ']' => Some(SquareBracketClose),
        _ => None
    }
}

fn handle_identifier(ident: &str) -> Token {
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
        "region" => Region,
        "import" => Import,
        "->" => Arrow,
        "=" => Equal,
        "|" => Pipe,
        "_" => Underscore,
        "true" => BooleanLiteral(true),
        "false" => BooleanLiteral(false),
        "class" => Class,
        "instance" => Instance,
        "and" => And,
        x => {
            let first = x.chars().next().expect(format!("Fatal Error: Unrecognized identifier '{}'", ident).as_ref());
            match first {
                '@' => RegionIdentifier(&ident[1..]),
                '\'' => GenericRegionIdentifier(&ident[1..]),
                c if c.is_alphabetic() && c.is_uppercase() => TypeIdentifier(ident),
                c if c.is_alphabetic() => NameIdentifier(ident),
                _ => OperatorIdentifier(ident)
            }
        }
    }
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let starting_pos = Location {
            pos: 0,
            line: 1,
            col: 1
        };
        Lexer {
            input,
            chars: input.chars().peekable(),
            last_pos: starting_pos,
            current_pos: starting_pos
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
            },
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
                    self.incr_line();
                    break;
                }
                Some(_) => self.incr_pos()
            }
        }
    }

    fn string_literal(&mut self) -> Option<LexerResult<'input>> {
        let start = self.token_start_pos();
        let start_idx = self.current_pos.pos;
        let mut escaped = false;
        let end_idx = loop {
            match self.advance() {
                None => break(self.current_pos.pos),
                Some('\\') => escaped = true,
                Some('"') => {
                    if !escaped {
                        break(self.current_pos.pos - 1)
                    } else {
                        escaped = false
                    }
                },
                Some(_) => ()
            }
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.token_start_pos();
        Some(Ok((start, StringLiteral(lit), end)))
    }

    fn number_literal(&mut self) -> Option<LexerResult<'input>> {
        //TODO: Improve literals
        let start = self.token_start_pos();
        let start_idx = self.current_pos.pos -  1;
        let end_idx = loop {
            match self.chars.peek() {
                None => break(self.current_pos.pos),
                Some('.') => (),
                Some(x) if x.is_numeric() => (),
                _ => break(self.current_pos.pos)
            }
            self.advance();
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.token_start_pos();
        let format = match lit.contains(".") {
            true => NumberFormat::Float,
            false => NumberFormat::Integer
        };
        Some(Ok((start, NumberLiteral((lit, format)), end)))
    }

    fn identifier(&mut self) -> Option<LexerResult<'input>> {
        let start = self.token_start_pos();
        let start_idx = self.current_pos.pos - 1;
        let end_idx = loop {
            match self.chars.peek() {
                None => break(self.current_pos.pos),
                Some(&c) if is_separator(c) => break(self.current_pos.pos),
                Some(_) => ()
            }
            self.advance();
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.token_start_pos();
        Some(Ok((start, handle_identifier(lit), end)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexerResult<'input>;

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
                            break self.number_literal()
                        } else {
                            break self.identifier()
                        }
                    }
                }
                Some(x) => {
                    if x.is_numeric() { break self.number_literal() }
                    if let Some(t) = single_char_token(x) {
                        break Some(Ok((last_pos, t, self.token_start_pos())))
                    }
                    break self.identifier()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{Token, NumberFormat};
    use crate::token::Token::*;
    use crate::lexer::Lexer;

    fn lex_equal(code: &str, tokens: Vec<Token>) {
        let lexer = Lexer::new(code);
        let res: Vec<Token> = lexer.map(|tk| tk.unwrap()).map(|(_, t, _)| t).collect();
        assert_eq!(res, tokens);
    }

    #[test]
    fn lex1() {
        let code = "ab cd, .: ->";
        let tokens = vec![ NameIdentifier("ab"), NameIdentifier("cd"), Comma, Period, Colon, Arrow ];
        lex_equal(code, tokens)
    }

    #[test]
    fn lex2() {
        let code = "\"test\" test \"asdf\\\"\"";
        let tokens = vec! [StringLiteral("test"), NameIdentifier("test"), StringLiteral("asdf\\\"") ];
        lex_equal(code, tokens)
    }

    #[test]
    fn lex3() {
        let code = "12.3 344.45, 9900 -3";
        let tokens = vec![ NumberLiteral(("12.3", NumberFormat::Float)), NumberLiteral(("344.45", NumberFormat::Float)), Comma, NumberLiteral(("9900", NumberFormat::Integer)), NumberLiteral(("-3", NumberFormat::Integer)) ];
        lex_equal(code, tokens)
    }

    #[test]
    fn lex4() {
        let code = "case x of | a:Int -> 2";
        let tokens = vec![ Case, NameIdentifier("x"), Of, Pipe, NameIdentifier("a"), Colon, TypeIdentifier("Int"), Arrow, NumberLiteral(("2", NumberFormat::Integer)) ];
        lex_equal(code, tokens);
    }

    #[test]
    fn lex5() {
        let code = "{ a = \"test\", b = 12.4, c = d (a b) }";
        let tokens = vec![ CurlyBraceOpen, NameIdentifier("a"), Equal, StringLiteral("test"), Comma, NameIdentifier("b"), Equal, NumberLiteral(("12.4", NumberFormat::Float)), Comma, NameIdentifier("c"), Equal, NameIdentifier("d"), ParenOpen, NameIdentifier("a"), NameIdentifier("b"), ParenClose, CurlyBraceClose ];
        lex_equal(code, tokens)
    }

    #[test]
    fn lex6() {
        let code = "if (x == asdf) then fun a -> a else 12";
        let tokens = vec![ If, ParenOpen, NameIdentifier("x"), OperatorIdentifier("=="), NameIdentifier("asdf"), ParenClose, Then, Fun, NameIdentifier("a"), Arrow, NameIdentifier("a"), Else, NumberLiteral(("12", NumberFormat::Integer)) ];
        lex_equal(code, tokens);
    }

    #[test]
    fn lex7() {
        let code = "
test #asdf d.
      , (
      #)
#";
        let tokens = vec![ NameIdentifier("test"), Comma, ParenOpen ];
        lex_equal(code, tokens);
    }

    #[test]
    fn lex8() {
        let code = "map (fun i -> i + 2) x";
        let tokens = vec![ NameIdentifier("map"), ParenOpen, Fun, NameIdentifier("i"), Arrow, NameIdentifier("i"), OperatorIdentifier("+"), NumberLiteral(("2", NumberFormat::Integer)), ParenClose, NameIdentifier("x") ];
        lex_equal(code, tokens);
    }

    #[test]
    fn lex9() {
        let code = "println (\"Hello\" ++ \"World!\")";
        let tokens = vec! [ NameIdentifier("println"), ParenOpen, StringLiteral("Hello"), OperatorIdentifier("++"), StringLiteral("World!"), ParenClose ];
        lex_equal(code, tokens);
    }

    #[test]
    fn lex10() {
        let code = "a.T.b @reg ++ test";
        let tokens = vec! [ NameIdentifier("a"), Period, TypeIdentifier("T"), Period, NameIdentifier("b"), RegionIdentifier("reg"), OperatorIdentifier("++"), NameIdentifier("test") ];
        lex_equal(code, tokens);
    }
}