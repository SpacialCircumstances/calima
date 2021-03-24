use crate::ast::*;
use crate::ast_common::Name;
use crate::parsing::lexer::{Error, Lexer};
use crate::parsing::token::{Location, Span, Token};
use crate::symbol_names::{SymbolName, SymbolNameInterner};
use lalrpop_util::ParseError;

lalrpop_mod!(
    #[allow(clippy::all)]
    calima_parser,
    "/parsing/calima_parser.rs"
);

fn to_data(left_loc: Location, right_loc: Location) -> Span {
    Span {
        left: left_loc,
        right: right_loc,
    }
}

pub fn parse_type<'a>(
    text: &'a str,
    interner: &SymbolNameInterner,
) -> Result<TypeAnnotation<Name<Span>, SymbolName, Span>, ParseError<Location, Token<'a>, Error>> {
    let lexer = Lexer::new(text);
    let parser = calima_parser::TypeAnnotation0Parser::new();
    parser.parse(&to_data, interner, lexer)
}

pub fn parse<'a>(
    code: &'a str,
    interner: &SymbolNameInterner,
) -> Result<TopLevelBlock<Name<Span>, SymbolName, Span>, ParseError<Location, Token<'a>, Error>> {
    let lexer = Lexer::new(code);
    let parser = calima_parser::TopLevelBlockParser::new();
    parser.parse(
        &|left_loc, right_loc| Span {
            left: left_loc,
            right: right_loc,
        },
        interner,
        lexer,
    )
}

#[cfg(test)]
mod tests {
    use crate::ast::Expr::*;
    use crate::ast::{Block, Let, TopLevelBlock, TopLevelStatement};
    use crate::ast_common::{BindPattern, Literal};
    use crate::parsing::parser::parse;
    use crate::parsing::token::{Location, Span};
    use crate::symbol_names::SymbolNameInterner;
    use crate::typed_ast::Unit;
    use goldenfile::Mint;
    use std::fs::read_dir;
    use std::io::Write;

    #[test]
    fn test_by_comparing_to_parsed() {
        let mut mint_code = Mint::new("tests/parsed/");

        for entry in read_dir("../examples/basic/").unwrap() {
            match entry {
                Ok(entry) => {
                    let interner = SymbolNameInterner::new();
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        let filename = entry_path.file_name().unwrap();
                        let mut parsed_file = mint_code.new_goldenfile(filename).unwrap();
                        let file_content = std::fs::read_to_string(&entry_path).unwrap();
                        let parsed = parse(&file_content, &interner).expect(
                            format!("Error parsing {}", filename.to_string_lossy()).as_ref(),
                        );
                        write!(parsed_file, "{}", parsed).unwrap();
                    }
                }
                Err(_) => (),
            }
        }
    }
}
