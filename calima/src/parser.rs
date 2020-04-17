use crate::ast::*;
use crate::lexer::*;

lalrpop_mod!(pub calima_parser);

pub fn parse<'a>(code: &'a str) -> Result<Block<'a, ()>, String> {
    let lexer = Lexer::new(code);
    let parser = calima_parser::ProgramParser::new();
    let statements = parser.parse(code, lexer);
    Err(String::from("Error"))
}