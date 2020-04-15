use std::str::Chars;
use crate::token::{Token, Location};
use crate::lexer::ErrorKind::TabIndent;
use crate::token::Token::*;
use std::iter::Peekable;

pub struct Lexer<'input> {
    chars: Peekable<Chars<'input>>,
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

fn try_to_keyword(ident: &str) -> Token {
    match ident {
        "do" => Do,
        "let" => Let,
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
        _ => Identifier(ident)
    }
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            input,
            chars: input.chars().peekable(),
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
        let start = self.current_pos();
        let start_idx = self.pos;
        let end_idx = loop {
            match self.advance() {
                None => break(self.pos),
                Some('"') => break(self.pos - 1),
                Some(_) => ()
            }
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.current_pos();
        Some(Ok((start, StringLiteral(lit), end)))
    }

    fn number_literal(&mut self) -> Option<LexerResult<'input>> {
        //TODO: Improve literals
        let start = self.current_pos();
        let start_idx = self.pos -  1;
        let end_idx = loop {
            match self.advance() {
                None => break(self.pos),
                Some('.') => (),
                Some(x) if x.is_numeric() => (),
                _ => break(self.pos - 1)
            }
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.current_pos();
        Some(Ok((start, NumberLiteral(lit), end)))
    }

    fn identifier(&mut self) -> Option<LexerResult<'input>> {
        let start = self.current_pos();
        let start_idx = self.pos - 1;
        let end_idx = loop {
            match self.chars.peek() {
                None => break(self.pos),
                Some(&c) if is_separator(c) => break(self.pos),
                Some(_) => ()
            }
            self.advance();
        };
        let lit = &self.input[start_idx..end_idx];
        let end = self.current_pos();
        Some(Ok((start, try_to_keyword(lit), end)))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexerResult<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let last_pos = self.current_pos();
            match self.advance() {
                None => break None,
                Some(' ') => (),
                Some('\n') => (),
                Some('\r') => (),
                Some('\t') => break Some(Err(Error { location: self.current_pos(), kind: TabIndent })),
                Some('#') => self.comment(),
                Some('"') => break self.string_literal(),
                Some(x) => {
                    if x.is_numeric() { break self.number_literal() }
                    if let Some(t) = single_char_token(x) {
                        break Some(Ok((last_pos, t, self.current_pos())))
                    }
                    break self.identifier()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;
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
        let tokens = vec![ Identifier("ab"), Identifier("cd"), Comma, Period, Colon, Arrow ];
        lex_equal(code, tokens)
    }

    #[test]
    fn lex2() {
        let code = "\"test\" test";
        let tokens = vec! [StringLiteral("test"), Identifier("test") ];
        lex_equal(code, tokens)
    }

    #[test]
    fn lex3() {
        let code = "12.3 344.45 9900";
        let tokens = vec![ NumberLiteral("12.3"), NumberLiteral("344.45"), NumberLiteral("9900") ];
        lex_equal(code, tokens)
    }

    #[test]
    fn lex4() {
        let code = "case x of | a:Int -> 2";
        let tokens = vec![ Case, Identifier("x"), Of, Pipe, Identifier("a"), Colon, Identifier("Int"), Arrow, NumberLiteral("2") ];
        lex_equal(code, tokens);
    }
}