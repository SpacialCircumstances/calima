use std::path::{PathBuf, Path};
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
use std::iter::once;
use crate::errors::{CompilerError, ErrorContext};
use crate::errors::CompilerError::*;

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

    pub fn path_relative_to(&self, path: &Path) -> PathBuf {
        let mut path = PathBuf::from(path);
        for el in self.components() {
            path.push(el)
        }
        path.set_extension("ca");
        path
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
    string_interner: StringInterner,
    error_context: ErrorContext<'input>
}

impl<'input> CompilerContext<'input> {
    pub fn from_args<S: AsRef<str>>(args: CompilerArguments<S>) -> Result<Self, CompilerError> {
        let entrypoint_path = PathBuf::from(args.entrypoint);
        if !entrypoint_path.is_file() {
            return Err(GeneralError(None, format!("Entrypoint file {} not found", args.entrypoint)))
        }
        //Get directory of entrypoint
        let entrypoint_module_name = entrypoint_path.file_name().unwrap().to_str().unwrap().to_string();

        let module_queue = vec![ ModuleDescriptor {
            identifier: ModuleIdentifier::from_filename(entrypoint_module_name),
            depth: 0,
            path: entrypoint_path
        } ];

        let (search_dirs, errors): (Vec<PathBuf>, Vec<PathBuf>) = args.search_paths
            .iter()
            .map(|dir| PathBuf::from(dir.as_ref()))
            .partition(|dir| dir.is_dir());

        if !errors.is_empty() {
            Err(GeneralError(None, format!("{} is not a valid search directory", errors.first().unwrap().to_string_lossy())))
        } else {
            Ok(CompilerContext {
                module_queue,
                search_dirs,
                modules: HashMap::new(),
                string_interner: StringInterner::new(),
                error_context: ErrorContext::new()
            })
        }
    }

    fn try_resolve_module(&self, from: &Module, module_ident: &ModuleIdentifier) -> Option<PathBuf> {
        once(&from.path)
            .chain(self.search_dirs.iter())
            .find_map(|dir| {
                let mut path = module_ident.path_relative_to(dir);
                module_ident.path_relative_to(&mut path);
                match path.is_file() {
                    true => Some(path),
                    false => None
                }
            })
    }

    fn parse_module(desc: ModuleDescriptor, interner: &StringInterner) -> Result<Module, CompilerError> {
        let code = read_to_string(&desc.path).map_err(|e| GeneralError(Some(Box::new(e)), format!("Error opening file {}", &desc.path.display())))?;
        let ast = parser::parse(&code, interner).map_err(|pe| ParserError(pe, desc.identifier.clone(), desc.path.clone()))?;
        let deps = find_imported_modules(&ast);

        Ok(Module {
            path: desc.path,
            ast,
            name: desc.identifier.clone(),
            depth: desc.depth,
            deps
        })
    }

    pub fn parse_all_modules(&'input mut self) -> Result<(), ()> {
        while let Some(next) = self.module_queue.pop() {
            //TODO: Store error in error context
            let module = Self::parse_module(next, &self.string_interner).unwrap();
            for dep in &module.deps {
                match self.modules.get_mut(dep) {
                    Some(dep_mod) => {
                        dep_mod.depth = min(dep_mod.depth, module.depth + 1)
                    },
                    None => {
                        match self.try_resolve_module(&module, dep) {
                            Some(found_path) => {
                                let desc = ModuleDescriptor {
                                    identifier: dep.clone(),
                                    depth: module.depth + 1,
                                    path: found_path
                                };
                                self.module_queue.push(desc);
                            },
                            None => {
                                //TODO: Handle error in error context
                                //return Err(Box::new(G(format!("Error resolving dependencies of {}: Module {} not found", &module.name, dep))));
                            }
                        }
                    }
                }
            }
            self.modules.insert(module.name.clone(), module);
        }
        Ok(())
    }
}