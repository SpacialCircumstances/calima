#[macro_use] extern crate lalrpop_util;

use std::error::Error;

mod token;
mod lexer;
mod ast;
mod parser;
mod compiler;
mod util;
mod string_interner;
mod analyze;

use compiler::CompilerContext;

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

pub fn compile<S: AsRef<str>>(args: CompilerArguments<S>) -> Result<(), Box<dyn Error>> {
    let mut context = CompilerContext::from_args(args)?;
    context.parse_all_modules()
}