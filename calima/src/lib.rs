#[macro_use]
extern crate lalrpop_util;

use quetta::Text;
use std::path::{Path, PathBuf};
use symbol_names::StringInterner;

use crate::errors::ErrorContext;
use crate::parsing::parse_all_modules;

mod ast;
mod common;
mod errors;
mod formatting;
mod ir;
mod modules;
mod parsing;
pub mod symbol_names;
mod typechecker;
mod types;

#[derive(Debug)]
pub struct CompilerArguments {
    entrypoint: PathBuf,
    search_paths: Vec<PathBuf>,
    output_name: PathBuf,
}

impl CompilerArguments {
    pub fn new(entrypoint: &str, search_paths: &Vec<String>, output_name: &Option<String>) -> Self {
        todo!()
    }
}

pub fn compile(args: CompilerArguments) -> Result<(), ()> {
    let mut errors = ErrorContext::new();
    let interner = StringInterner::new();
    let module_context = parse_all_modules(&mut errors, &interner, args)?;
    let typed_context = typechecker::typecheck(&mut errors, module_context, &interner)?;
    Ok(())
}
