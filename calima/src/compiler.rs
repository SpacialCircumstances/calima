use std::path::{PathBuf};
use std::fs::read_to_string;
use crate::{parser, CompilerArguments};
use std::collections::HashMap;
use std::cmp::min;
use crate::string_interner::StringInterner;
use std::iter::once;
use crate::errors::{CompilerError, ErrorContext};
use crate::errors::CompilerError::*;
use crate::common::*;
use crate::token::Span;
use crate::ast::{find_imported_modules, TopLevelBlock};

pub struct UntypedModuleData<'input>(pub TopLevelBlock<'input, Span>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ModuleDescriptor {
    identifier: ModuleIdentifier,
    path: PathBuf,
    depth: u32,
}

pub struct ModuleTreeContext<'input> {
    pub search_dirs: Vec<PathBuf>,
    pub modules: HashMap<ModuleIdentifier, Module<UntypedModuleData<'input>>>,
}

fn try_resolve_module(search_dirs: &[PathBuf], from: &Module<UntypedModuleData>, module_ident: &ModuleIdentifier) -> Result<PathBuf, Vec<PathBuf>> {
    let mut module_dir = from.path.to_path_buf();
    module_dir.pop();
    once(&module_dir)
        .chain(search_dirs.iter())
        .map(|dir| {
            let mut path = module_ident.path_relative_to(dir);
            module_ident.path_relative_to(&mut path);
            match path.is_file() {
                true => Ok(path),
                false => {
                    let mut search_dirs = search_dirs.to_vec();
                    search_dirs.insert(0, module_dir.clone());
                    Err(search_dirs)
                }
            }
        }).collect()
}

fn parse_module(desc: ModuleDescriptor, interner: &StringInterner) -> Result<Module<UntypedModuleData>, CompilerError> {
    let code = read_to_string(&desc.path).map_err(|e| GeneralError(Some(Box::new(e)), format!("Error opening file {}", &desc.path.display())))?;
    let ast = parser::parse(&code, interner).map_err(|pe| ParserError(pe, desc.identifier.clone(), desc.path.clone()))?;
    let deps = find_imported_modules(&ast);

    Ok(Module {
        path: desc.path,
        data: UntypedModuleData(ast),
        name: desc.identifier.clone(),
        depth: desc.depth,
        deps,
    })
}

pub fn parse_all_modules<'input, S: AsRef<str>>(string_interner: &'input StringInterner, error_context: &mut ErrorContext<'input>, args: CompilerArguments<S>) -> Result<ModuleTreeContext<'input>, ()> {
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

    let mut module_queue = vec![ModuleDescriptor {
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

    error_context.handle_errors()?;

    let mut modules: HashMap<ModuleIdentifier, Module<UntypedModuleData>> = HashMap::new();

    while let Some(next) = module_queue.pop() {
        match parse_module(next, string_interner) {
            Err(e) => error_context.add_error(e),
            Ok(module) => {
                for (dep, location) in &module.deps {
                    match modules.get_mut(dep) {
                        Some(dep_mod) => {
                            dep_mod.depth = min(dep_mod.depth, module.depth + 1)
                        }
                        None => {
                            match try_resolve_module(&search_dirs, &module, dep) {
                                Ok(found_path) => {
                                    let desc = ModuleDescriptor {
                                        identifier: dep.clone(),
                                        depth: module.depth + 1,
                                        path: found_path,
                                    };
                                    module_queue.push(desc);
                                }
                                Err(search_dirs) => {
                                    let err = ImportError {
                                        importing_mod: module.name.clone(),
                                        importing_mod_path: module.path.clone(),
                                        imported: dep.clone(),
                                        search_dirs,
                                        location: *location
                                    };
                                    error_context.add_error(err);
                                }
                            }
                        }
                    }
                }
                modules.insert(module.name.clone(), module);
            }
        }
    }

    error_context.handle_errors().map(|()| ModuleTreeContext {
        search_dirs,
        modules
    })
}