use crate::ast::*;
use crate::lexer::*;
use crate::token::{Span, Location, Token};
use lalrpop_util::ParseError;
use crate::string_interner::StringInterner;

lalrpop_mod!(pub calima_parser);

pub fn parse<'source, 'input>(code: &'source str, interner: &'input StringInterner) -> Result<TopLevelBlock<'input, Span>, ParseError<Location, Token<'input>, Error>> {
    let lexer = Lexer::new(code, interner);
    let parser = calima_parser::TopLevelBlockParser::new();
    let ast = parser.parse(&|left_loc, right_loc| Span { left: left_loc, right: right_loc }, lexer)?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use std::fs::read_dir;
    use std::io::Write;
    use crate::parser::parse;
    use crate::ast::{Block, TopLevelBlock};
    use crate::ast_common::Literal;
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
                result: Box::new(FunctionCall(Box::new(Variable("println", loc1)), vec![
                    Literal(Literal::String("Hello World!"), loc2)
                ], loc3))
            }
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_by_comparing_to_parsed() {
        let mut mint_code = Mint::new("tests/parsed/");
        let interner = StringInterner::new();

        for entry in read_dir("../examples/basic/").unwrap() {
            match entry {
                Ok(entry) => {
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        let filename = entry_path.file_name().unwrap();
                        let mut parsed_file = mint_code.new_goldenfile(filename).unwrap();
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