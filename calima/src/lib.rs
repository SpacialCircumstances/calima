#[macro_use] extern crate lalrpop_util;

use std::fs::File;
use std::io::Read;
use crate::ast::TopLevelBlock;
use crate::token::Span;
use std::error::Error;

mod token;
mod lexer;
mod ast;
mod parser;

#[derive(Debug)]
pub struct CompilerArguments<'a> {
    entrypoint: &'a str,
    search_paths: Vec<&'a str>
}

impl<'a> CompilerArguments<'a> {
    pub fn new(entrypoint: &'a str, search_paths: Vec<&'a str>) -> Self {
        CompilerArguments {
            entrypoint,
            search_paths
        }
    }
}

pub fn compile(args: CompilerArguments) -> Result<(), Box<dyn Error>> {
    let code = std::fs::read_to_string(args.entrypoint)?;
    let ast = parser::parse(&code)?;
    println!("{}", ast);
    Ok(())
}