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
}