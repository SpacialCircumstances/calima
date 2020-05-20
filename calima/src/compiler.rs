use std::path::PathBuf;
use std::fmt::{Display, Formatter};
use std::error::Error;
use std::fs::read_to_string;
use crate::{parser, CompilerArguments};
use crate::token::Span;
use crate::ast::TopLevelBlock;
use std::collections::HashMap;
use std::cmp::min;
use crate::string_interner::StringInterner;
use crate::analyze::find_imported_modules;

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
    path: PathBuf,
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

        let module_queue = vec![ ModuleDescriptor {
            identifier: ModuleIdentifier::from_filename(entrypoint_module_name),
            depth: 0,
            path: entrypoint_path
        } ];

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

    fn try_resolve_module(&self, module_ident: &ModuleIdentifier) -> Option<PathBuf> {
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

    fn parse_module(desc: ModuleDescriptor, interner: &StringInterner) -> Result<Module, Box<dyn Error>> {
        let code = read_to_string(&desc.path)?;
        let ast = parser::parse(&code, interner)?;
        let deps = find_imported_modules(&ast);

        Ok(Module {
            path: desc.path,
            ast,
            name: desc.identifier.clone(),
            depth: desc.depth,
            deps
        })
    }

    pub fn parse_all_modules(&'input mut self) -> Result<(), Box<dyn Error>> {
        while let Some(next) = self.module_queue.pop() {
            let module = Self::parse_module(next, &self.string_interner)?;
            for dep in &module.deps {
                match self.modules.get_mut(dep) {
                    Some(dep_mod) => {
                        dep_mod.depth = min(dep_mod.depth, module.depth + 1)
                    },
                    None => {
                        match self.try_resolve_module(dep) {
                            Some(found_path) => {
                                let desc = ModuleDescriptor {
                                    identifier: dep.clone(),
                                    depth: module.depth + 1,
                                    path: found_path
                                };
                                self.module_queue.push(desc);
                            },
                            None => {
                                return Err(Box::new(CompilerError(format!("Error resolving dependencies of {}: Module {} not found", &module.name, dep))));
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}