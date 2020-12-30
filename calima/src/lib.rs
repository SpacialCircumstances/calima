#[macro_use]
extern crate lalrpop_util;

mod ast;
mod ast_common;
mod common;
mod compiler;
mod errors;
mod formatting;
mod lexer;
mod parser;
mod prelude;
mod string_interner;
mod token;
mod typechecker;
mod typed_ast;
mod types;
mod util;

use crate::errors::ErrorContext;
use string_interner::StringInterner;

#[derive(Debug)]
pub struct CompilerArguments<'a, S: AsRef<str>> {
    entrypoint: &'a str,
    search_paths: &'a Vec<S>,
}

impl<'a, S: AsRef<str>> CompilerArguments<'a, S> {
    pub fn new(entrypoint: &'a str, search_paths: &'a Vec<S>) -> Self {
        CompilerArguments {
            entrypoint,
            search_paths,
        }
    }
}

pub fn compile<S: AsRef<str>>(args: CompilerArguments<S>) -> Result<(), ()> {
    let strs = StringInterner::new();
    let mut errors = ErrorContext::new();
    let module_context = compiler::parse_all_modules(&strs, &mut errors, args)?;
    let typed_context = typechecker::typecheck(&mut errors, module_context)?;
    Ok(())
}
