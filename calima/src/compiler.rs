use std::path::PathBuf;
use std::fmt::{Display, Formatter};
use std::error::Error;
use std::fs::read_to_string;
use crate::{parser, CompilerArguments};
use crate::token::Span;
use crate::ast::TopLevelBlock;
use std::collections::HashMap;

//TODO: Use an enum and handle all errors with this
#[derive(Debug)]
pub struct CompilerError(String);

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for CompilerError {

}

pub struct Module<'input> {
    ast: TopLevelBlock<'input, Span>,
    name: &'input str,
    depth: u32,
    deps: Vec<&'input str>
}

pub struct CompilerContext<'input> {
    module_queue: Vec<PathBuf>,
    search_dirs: Vec<PathBuf>,
    modules: HashMap<&'input str, Module<'input>>
}

impl<'input> CompilerContext<'input> {
    pub fn from_args<S: AsRef<str>>(args: CompilerArguments<S>) -> Result<Self, Box<dyn Error>> {
        let entrypoint_path = PathBuf::from(args.entrypoint);
        if !entrypoint_path.is_file() {
            return Err(Box::new(CompilerError(format!("Entrypoint file {} not found", args.entrypoint))))
        }
        //Get directory of entrypoint
        let mut entrypoint_dir = entrypoint_path.clone();
        entrypoint_dir.pop();

        let module_queue = vec![ entrypoint_path ];

        let mut search_dirs = Vec::new();
        search_dirs.push(entrypoint_dir);
        for dir in args.search_paths {
            let path = PathBuf::from(dir.as_ref());
            if path.is_dir() {
                search_dirs.push(PathBuf::from(dir.as_ref()))
            } else {
                return Err(Box::new(CompilerError(format!("Search directory {} not found or not a directory", dir.as_ref()))))
            }
        }
        Ok(CompilerContext {
            module_queue,
            search_dirs,
            modules: HashMap::new()
        })
    }

    pub fn parse_all_modules(&mut self) -> Result<(), Box<dyn Error>> {
        let code = read_to_string(self.module_queue.first().unwrap())?;
        let ast = parser::parse(&code)?;
        println!("{}", ast);
        Ok(())
    }
}