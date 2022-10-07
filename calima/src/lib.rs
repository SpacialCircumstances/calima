#[macro_use]
extern crate lalrpop_util;

use symbol_names::StringInterner;

use crate::errors::ErrorContext;
use crate::parsing::parse_all_modules;

mod ast;
mod ast_common;
mod common;
mod errors;
mod formatting;
mod modules;
mod parsing;
pub mod symbol_names;
mod typechecker;
mod typed_ast;
mod types;

#[derive(Debug)]
pub struct CompilerArguments<'a, S: AsRef<str>> {
    entrypoint: &'a str,
    search_paths: &'a Vec<S>,
    project_root_name: Option<&'a str>,
}

impl<'a, S: AsRef<str>> CompilerArguments<'a, S> {
    pub fn new(
        entrypoint: &'a str,
        search_paths: &'a Vec<S>,
        project_root_name: Option<&'a str>,
    ) -> Self {
        CompilerArguments {
            entrypoint,
            search_paths,
            project_root_name,
        }
    }
}

pub fn compile<S: AsRef<str>>(args: CompilerArguments<S>) -> Result<(), ()> {
    let mut errors = ErrorContext::new();
    let interner = StringInterner::new();
    let module_context = parse_all_modules(&mut errors, &interner, args)?;
    let typed_context = typechecker::typecheck(&mut errors, module_context, &interner)?;
    Ok(())
}
