use crate::ast::*;
use crate::parsing::lexer::{Error, Lexer};
use crate::parsing::string_interner::StringInterner;
use crate::parsing::token::{Location, Span, Token};
use lalrpop_util::ParseError;

lalrpop_mod!(
    #[allow(clippy::all)]
    calima_parser,
    "/parsing/calima_parser.rs"
);

pub fn parse_type<'source, 'input>(
    text: &'source str,
    interner: &'input StringInterner,
) -> Result<TypeAnnotation<'input, Span>, ParseError<Location, Token<'input>, Error>> {
    let lexer = Lexer::new(text, interner);
    let parser = calima_parser::TypeAnnotation0Parser::new();
    parser.parse(
        &|left_loc, right_loc| Span {
            left: left_loc,
            right: right_loc,
        },
        lexer,
    )
}

pub fn parse<'source, 'input>(
    code: &'source str,
    interner: &'input StringInterner,
) -> Result<TopLevelBlock<'input, Span>, ParseError<Location, Token<'input>, Error>> {
    let lexer = Lexer::new(code, interner);
    let parser = calima_parser::TopLevelBlockParser::new();
    let ast = parser.parse(
        &|left_loc, right_loc| Span {
            left: left_loc,
            right: right_loc,
        },
        lexer,
    )?;
    Ok(ast)
}

#[cfg(test)]
mod tests {
    use crate::ast::Expr::*;
    use crate::ast::{Block, Let, TopLevelBlock, TopLevelStatement};
    use crate::ast_common::{BindPattern, Literal};
    use crate::parsing::parser::parse;
    use crate::parsing::string_interner::StringInterner;
    use crate::parsing::token::{Location, Span};
    use crate::typed_ast::Unit;
    use goldenfile::Mint;
    use std::fs::read_dir;
    use std::io::Write;

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
