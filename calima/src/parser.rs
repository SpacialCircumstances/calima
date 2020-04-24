use crate::ast::*;
use crate::lexer::*;
use crate::token::Location;

lalrpop_mod!(pub calima_parser);

pub fn parse<'a>(code: &'a str) -> Result<Block<'a, Location>, String> {
    let lexer = Lexer::new(code);
    let parser = calima_parser::BlockParser::new();
    let block = parser.parse(code, &|loc| loc, lexer).map_err(|e| format!("{}", e))?;
    Ok(block)
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;
    use crate::ast::{Block, Literal};
    use crate::ast::Expr::*;
    use crate::token::Location;

    #[test]
    fn test_hello_world() {
        let code = include_str!("test_snippets/hello_world.ca");
        let parsed = parse(code);
        assert!(parsed.is_ok());
        let ast = parsed.unwrap();
        let expected = Block {
            statements: Vec::new(),
            result: Box::new(FunctionCall(Box::new(Variable(vec![ "println" ], Location { pos: 0, col: 1, line: 1 })), vec![
                Literal(Literal::String("Hello World!"), Location { pos: 8, col: 9, line: 1 })
            ], Location { pos: 0, col: 1, line: 1 }))
        };
        assert_eq!(ast, expected);
    }
}