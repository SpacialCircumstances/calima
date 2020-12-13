#[macro_use] extern crate lalrpop_util;

mod token;
mod lexer;
mod ast_common;
mod ast;
mod parser;
mod compiler;
mod util;
mod string_interner;
mod errors;
mod common;
mod types;
mod prelude;
mod typed_ast;
mod typechecker;

use string_interner::StringInterner;
use crate::errors::ErrorContext;

#[derive(Debug)]
pub struct CompilerArguments<'a, S: AsRef<str>> {
    entrypoint: &'a str,
    search_paths: &'a Vec<S>
}

impl<'a, S: AsRef<str>> CompilerArguments<'a, S> {
    pub fn new(entrypoint: &'a str, search_paths: &'a Vec<S>) -> Self {
        CompilerArguments {
            entrypoint,
            search_paths
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