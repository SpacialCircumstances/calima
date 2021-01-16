#[macro_use]
extern crate lalrpop_util;

use crate::errors::ErrorContext;
use crate::parsing::parse_all_modules;
use crate::parsing::string_interner::StringInterner;

mod ast;
mod ast_common;
mod common;
mod errors;
mod formatting;
mod modules;
mod parsing;
mod typechecker;
mod typed_ast;
mod types;

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
    let module_context = parse_all_modules(&strs, &mut errors, args)?;
    let typed_context = typechecker::typecheck(&mut errors, module_context)?;
    Ok(())
}
