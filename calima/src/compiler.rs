use std::path::{PathBuf};
use std::fs::read_to_string;
use crate::{parser, CompilerArguments};
use std::collections::HashMap;
use std::cmp::min;
use crate::string_interner::StringInterner;
use crate::analyze::find_imported_modules;
use std::iter::once;
use crate::errors::{CompilerError, ErrorContext};
use crate::errors::CompilerError::*;
use crate::common::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ModuleDescriptor {
    identifier: ModuleIdentifier,
    path: PathBuf,
    depth: u32,
}

pub struct CompilerContext<'input> {
    module_queue: Vec<ModuleDescriptor>,
    search_dirs: Vec<PathBuf>,
    modules: HashMap<ModuleIdentifier, Module<'input>>,
    string_interner: StringInterner,
    error_context: ErrorContext<'input>,
}

impl<'input> CompilerContext<'input> {
    pub fn from_args<S: AsRef<str>>(args: CompilerArguments<S>) -> Result<Self, ()> {
        let mut error_context = ErrorContext::new();

        let entrypoint_path = PathBuf::from(args.entrypoint);
        if !entrypoint_path.is_file() {
            error_context.add_error(GeneralError(None, format!("Entrypoint file {} not found", args.entrypoint)));
        }
        //Get directory of entrypoint
        let entrypoint_module_name = match entrypoint_path.file_name(){
            None => {
                error_context.add_error(GeneralError(None, format!("Invalid entrypoint file {}", entrypoint_path.display())));
                "Error".to_string()
            },
            Some(filename) => filename.to_string_lossy().to_string()
        };

        let module_queue = vec![ModuleDescriptor {
            identifier: ModuleIdentifier::from_filename(entrypoint_module_name),
            depth: 0,
            path: entrypoint_path,
        }];

        let (search_dirs, errors): (Vec<PathBuf>, Vec<PathBuf>) = args.search_paths
            .iter()
            .map(|dir| PathBuf::from(dir.as_ref()))
            .partition(|dir| dir.is_dir());

        if !errors.is_empty() {
            for err in errors {
                error_context.add_error(GeneralError(None, format!("{} is not a valid search directory", err.display())));
            }
        }
        error_context.handle_errors().map(|()| CompilerContext {
            module_queue,
            search_dirs,
            modules: HashMap::new(),
            string_interner: StringInterner::new(),
            error_context,
        })
    }

    fn try_resolve_module(&self, from: &Module, module_ident: &ModuleIdentifier) -> Result<PathBuf, Vec<PathBuf>> {
        let mut module_dir = from.path.to_path_buf();
        module_dir.pop();
        once(&module_dir)
            .chain(self.search_dirs.iter())
            .map(|dir| {
                let mut path = module_ident.path_relative_to(dir);
                module_ident.path_relative_to(&mut path);
                match path.is_file() {
                    true => Ok(path),
                    false => {
                        let mut search_dirs = self.search_dirs.clone();
                        search_dirs.insert(0, module_dir.clone());
                        Err(search_dirs)
                    }
                }
            }).collect()
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
            deps,
        })
    }

    pub fn parse_all_modules(&'input mut self) -> Result<(), ()> {
        while let Some(next) = self.module_queue.pop() {
            match Self::parse_module(next, &self.string_interner) {
                Err(e) => self.error_context.add_error(e),
                Ok(module) => {
                    for (dep, location) in &module.deps {
                        match self.modules.get_mut(dep) {
                            Some(dep_mod) => {
                                dep_mod.depth = min(dep_mod.depth, module.depth + 1)
                            }
                            None => {
                                match self.try_resolve_module(&module, dep) {
                                    Ok(found_path) => {
                                        let desc = ModuleDescriptor {
                                            identifier: dep.clone(),
                                            depth: module.depth + 1,
                                            path: found_path,
                                        };
                                        self.module_queue.push(desc);
                                    }
                                    Err(search_dirs) => {
                                        let err = ImportError {
                                            importing_mod: module.name.clone(),
                                            importing_mod_path: module.path.clone(),
                                            imported: dep.clone(),
                                            search_dirs,
                                            location: *location
                                        };
                                        self.error_context.add_error(err);
                                    }
                                }
                            }
                        }
                    }
                    self.modules.insert(module.name.clone(), module);
                }
            }
        }
        self.error_context.handle_errors()
    }
}