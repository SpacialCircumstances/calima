use std::path::PathBuf;
use std::fmt::{Display, Formatter};
use std::error::Error;
use std::fs::read_to_string;
use crate::{parser, CompilerArguments};
use crate::token::Span;
use crate::ast::TopLevelBlock;
use crate::util::*;
use std::collections::HashMap;
use std::cmp::min;
use crate::string_interner::StringInterner;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleIdentifier {
    full_name: String
}

impl ModuleIdentifier {
    pub fn from_name(name: &[&str]) -> Self {
        ModuleIdentifier {
            full_name: name.join(".")
        }
    }

    pub fn from_filename(name: String) -> Self {
        ModuleIdentifier {
            full_name: name
        }
    }

    pub fn components(&self) -> impl Iterator<Item=&str> {
        self.full_name.split(".")
    }
}

impl Display for ModuleIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.full_name)
    }
}

pub struct Module<'input> {
    ast: TopLevelBlock<'input, Span>,
    name: ModuleIdentifier,
    path: PathBuf,
    depth: u32,
    deps: Vec<ModuleIdentifier>
}

pub struct ModuleDescriptor {
    identifier: ModuleIdentifier,
    depth: u32
}

pub struct CompilerContext<'input> {
    module_queue: Vec<ModuleDescriptor>,
    search_dirs: Vec<PathBuf>,
    modules: HashMap<ModuleIdentifier, Module<'input>>,
    string_interner: StringInterner
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

        let module_queue = vec![ ModuleDescriptor { identifier: ModuleIdentifier::from_filename(entrypoint_module_name), depth: 0 } ];

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
            modules: HashMap::new(),
            string_interner: StringInterner::new()
        })
    }

    fn resolve_module(&self, module_ident: &ModuleIdentifier) -> Option<PathBuf> {
        self.search_dirs.iter().find_map(|dir| {
            let mut path = dir.clone();
            for el in module_ident.components() {
                path.push(el)
            }
            path.set_extension("ca");
            match path.exists() {
                true => Some(path),
                false => None
            }
        })
    }

    pub fn parse_all_modules(&'input mut self) -> Result<(), Box<dyn Error>> {
        while let Some(next) = self.module_queue.pop() {
            match self.modules.get_mut(&next.identifier) {
                Some(module) => {
                    module.depth = min(module.depth, next.depth);
                },
                None => {
                    let path = self.resolve_module(&next.identifier)
                        .ok_or_else(|| CompilerError(format!("Error resolving module {}: Not found", next.identifier)))?;
                    let code = read_to_string(&path)?;
                    let ast = parser::parse(&code, &self.string_interner)?;
                    let module = Module {
                        path,
                        ast,
                        name: next.identifier.clone(),
                        depth: next.depth,
                        deps: vec![]
                    };
                    self.modules.insert(next.identifier, module);
                }
            }
        }
        Ok(())
    }
}