use crate::ast::*;
use crate::lexer::*;
use crate::token::Location;

lalrpop_mod!(pub calima_parser);

pub fn parse<'a>(code: &'a str) -> Result<TopLevelBlock<'a, Location>, String> {
    let lexer = Lexer::new(code);
    let parser = calima_parser::TopLevelBlockParser::new();
    let block: TopLevelBlock<'a, Location> = parser.parse(code, &|loc| loc, lexer).map_err(|e| format!("{}", e))?;
    Ok(block)
}

#[cfg(test)]
mod tests {
    use std::fs::read_dir;
    use std::io::Write;
    use crate::parser::parse;
    use crate::ast::{Block, Literal};
    use crate::ast::Expr::*;
    use crate::token::Location;
    use goldenfile::Mint;

    #[test]
    fn test_hello_world() {
        let code = "println \"Hello World!\"";
        let parsed = parse(code);
        let ast = parsed.expect("Parser error");
        let expected = Block {
            statements: Vec::new(),
            result: Box::new(FunctionCall(Box::new(Variable(vec![ "println" ], Location { pos: 0, col: 1, line: 1 })), vec![
                Literal(Literal::String("Hello World!"), Location { pos: 8, col: 9, line: 1 })
            ], Location { pos: 0, col: 1, line: 1 }))
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_by_comparing_to_parsed() {
        let mut mint = Mint::new("tests/parsed/");

        for entry in read_dir("../examples/basic/").unwrap() {
            match entry {
                Ok(entry) => {
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        let filename = entry_path.file_name().unwrap();
                        let mut parsed_file = mint.new_goldenfile(filename).unwrap();
                        let file_content = std::fs::read_to_string(&entry_path).unwrap();
                        let parsed = parse(&file_content).expect(format!("Error parsing {}", filename.to_string_lossy()).as_ref());
                        write!(parsed_file, "{}", parsed).unwrap();
                    }
                },
                Err(_) => ()
            }
        }
    }
}