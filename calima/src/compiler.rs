use std::path::PathBuf;
use std::fmt::{Display, Formatter};
use std::error::Error;
use std::fs::read_to_string;
use crate::{parser, CompilerArguments};
use crate::token::Span;
use crate::ast::TopLevelBlock;
use crate::util::*;
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
    name: Vec<String>,
    depth: u32,
    deps: Vec<&'input str>
}

pub struct ModuleDescriptor {
    identifier: Vec<String>,
    depth: u32
}

pub struct CompilerContext<'input> {
    module_queue: Vec<ModuleDescriptor>,
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
        let entrypoint_module_name = entrypoint_path.file_name().unwrap().to_str().unwrap().to_string();
        entrypoint_dir.pop();

        let module_queue = vec![ ModuleDescriptor { identifier: vec![ entrypoint_module_name ], depth: 0 } ];

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

    fn resolve_module<S: AsRef<str>>(&self, module_name: &[S]) -> Option<PathBuf> {
        self.search_dirs.iter().find_map(|dir| {
            let mut path = dir.clone();
            for el in module_name {
                path.push(el.as_ref())
            }
            path.set_extension("ca");
            match path.exists() {
                true => Some(path),
                false => None
            }
        })
    }

    pub fn parse_all_modules(&mut self) -> Result<(), Box<dyn Error>> {
        while let Some(next) = self.module_queue.pop() {
            let path = self.resolve_module(&next.identifier)
                .ok_or_else(|| CompilerError(format!("Error resolving module {}: Not found", format_iter(next.identifier.iter(), "."))))?;
            let code = read_to_string(path)?;
            let ast = parser::parse(&code)?;
            let module = Module {
                ast,
                name: next.identifier,
                depth: next.depth,
                deps: vec![]
            };
        }
        Ok(())
    }
}