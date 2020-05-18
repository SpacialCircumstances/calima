use crate::ast::*;
use crate::lexer::*;
use crate::token::{Span, Location, Token};

use lalrpop_util::ParseError;
use std::fmt::{Display, Formatter};
use crate::string_interner::StringInterner;

lalrpop_mod!(pub calima_parser);

#[derive(Debug)]
pub struct ParserError(String);

impl ParserError {
    fn new(err: ParseError<Location, Token, Error>) -> Self {
        ParserError(err.to_string())
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for ParserError {

}

pub fn parse<'source, 'input>(code: &'source str, interner: &'input StringInterner) -> Result<TopLevelBlock<'input, Span>, ParserError> {
    let lexer = Lexer::new(code, interner);
    let parser = calima_parser::TopLevelBlockParser::new();
    let ast = parser.parse(&|left_loc, right_loc| Span { left: left_loc, right: right_loc }, lexer).map_err(|e| ParserError::new(e))?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use std::fs::read_dir;
    use std::io::Write;
    use crate::parser::parse;
    use crate::ast::{Block, Literal, TopLevelBlock};
    use crate::ast::Expr::*;
    use crate::token::{Location, Span};
    use goldenfile::Mint;
    use crate::string_interner::StringInterner;

    #[test]
    fn test_hello_world() {
        let interner = StringInterner::new();
        let code = "println \"Hello World!\"";
        let parsed = parse(code, &interner);
        let ast = parsed.expect("Parser error");
        let loc1 = Span {
            left: Location { pos: 0, col: 1, line: 1 },
            right: Location { pos: 6, col: 7, line: 1 }
        };
        let loc2 = Span {
            left: Location { pos: 8, col: 9, line: 1 },
            right: Location { pos: 21, col: 22, line: 1 }
        };
        let loc3 = Span {
            left: loc1.left,
            right: loc2.right
        };
        let expected = TopLevelBlock {
            top_levels: Vec::new(),
            block: Block {
                statements: Vec::new(),
                result: Box::new(FunctionCall(Box::new(Variable(vec!["println"], loc1)), vec![
                    Literal(Literal::String("Hello World!"), loc2)
                ], loc3))
            }
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_by_comparing_to_parsed() {
        let mut mint = Mint::new("tests/parsed/");
        let interner = StringInterner::new();

        for entry in read_dir("../examples/basic/").unwrap() {
            match entry {
                Ok(entry) => {
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        let filename = entry_path.file_name().unwrap();
                        let mut parsed_file = mint.new_goldenfile(filename).unwrap();
                        let file_content = std::fs::read_to_string(&entry_path).unwrap();
                        let parsed = parse(&file_content, &interner).expect(format!("Error parsing {}", filename.to_string_lossy()).as_ref());
                        write!(parsed_file, "{}", parsed).unwrap();
                    }
                },
                Err(_) => ()
            }
        }
    }
}