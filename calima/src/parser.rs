use crate::ast::*;
use crate::lexer::*;
use crate::token::Location;

lalrpop_mod!(pub calima_parser);

pub fn parse<'a>(code: &'a str) -> Result<Block<'a, Location>, String> {
    let lexer = Lexer::new(code);
    let parser = calima_parser::BlockParser::new();
    let block = parser.parse(code, lexer).map_err(|e| format!("{}", e))?;
    Ok(block)
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;

    #[test]
    fn test_hello_world() {
        let code = include_str!("test_snippets/hello_world.ca");
        let parsed = parse(code);
        assert!(parsed.is_ok());
        println!("{:#?}", parsed.unwrap());
    }
}