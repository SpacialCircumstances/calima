#[macro_use] extern crate lalrpop_util;

use std::fs::File;
use std::io::Read;

mod token;
mod lexer;
mod ast;
mod parser;

pub fn compile(file_name: &str) -> Result<(), String> {
    let mut file = File::open(file_name).expect(format!("Error opening file {}", file_name).as_ref());
    let mut file_content = String::new();
    file.read_to_string(&mut file_content).expect("Error reading from input file");
    let ast = parser::parse(&file_content)?;
    println!("{}", ast);
    Ok(())
}