use crate::ast::*;
use crate::parsing::lexer::{Error, Lexer};
use crate::parsing::token::{Location, Span, Token};
use crate::symbol_names::{IText, StringInterner};
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

pub fn parse_type(
    text: &str,
    interner: &StringInterner,
) -> Result<TypeAnnotation<Name<Span>, IText, Span>, ParseError<Location, Token, Error>> {
    let lexer = Lexer::new(text, interner);
    let parser = calima_parser::TypeAnnotation0Parser::new();
    parser.parse(&to_data, lexer)
}

pub fn parse(
    code: &str,
    interner: &StringInterner,
) -> Result<TopLevelBlock<Name<Span>, IText, Span>, ParseError<Location, Token, Error>> {
    let lexer = Lexer::new(code, interner);
    let parser = calima_parser::TopLevelBlockParser::new();
    parser.parse(
        &|left_loc, right_loc| Span {
            left: left_loc,
            right: right_loc,
        },
        lexer,
    )
}

#[cfg(test)]
mod tests {
    use crate::ast::Expr::*;
    use crate::ast::{BindPattern, Block, Let, Literal, TopLevelBlock, TopLevelStatement};
    use crate::parsing::parser::parse;
    use crate::parsing::token::{Location, Span};
    use crate::symbol_names::StringInterner;
    use goldenfile::Mint;
    use std::fs::read_dir;
    use std::io::Write;

    #[test]
    fn test_by_comparing_to_parsed() {
        let mut mint_code = Mint::new("tests/parsed_programs/");

        for entry in read_dir("tests/test_programs/").unwrap() {
            match entry {
                Ok(entry) => {
                    let interner = StringInterner::new();
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
