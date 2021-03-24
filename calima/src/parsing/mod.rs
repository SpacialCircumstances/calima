use std::collections::HashMap;
use std::fs::read_to_string;
use std::iter::once;
use std::path::PathBuf;
use std::rc::Rc;

use crate::ast::{find_imported_modules, TopLevelBlock};
use crate::common::*;
use crate::errors::CompilerError::*;
use crate::errors::{CompilerError, ErrorContext};
use crate::modules::{UntypedModule, UntypedModuleData, UntypedModuleTree};
use crate::symbol_names::SymbolNameInterner;
use crate::CompilerArguments;

pub mod lexer;
pub mod parser;
pub mod token;

fn try_resolve_module(
    search_dirs: &[PathBuf],
    from_path: &PathBuf,
    name: &ModuleIdentifier,
) -> Result<PathBuf, Vec<PathBuf>> {
    let mut module_dir = from_path.to_path_buf();
    module_dir.pop();
    once(&module_dir)
        .chain(search_dirs.iter())
        .map(|dir| {
            let path = name.path_relative_to(dir);
            match path.is_file() {
                true => Ok(path),
                false => {
                    let mut search_dirs = search_dirs.to_vec();
                    search_dirs.insert(0, module_dir.clone());
                    Err(search_dirs)
                }
            }
        })
        .collect()
}

pub fn parse_all_modules<'input, S: AsRef<str>>(
    error_context: &mut ErrorContext<'input>,
    interner: &SymbolNameInterner,
    args: CompilerArguments<S>,
) -> Result<UntypedModuleTree, ()> {
    let entrypoint_path = PathBuf::from(args.entrypoint);
    if !entrypoint_path.is_file() {
        error_context.add_error(GeneralError(
            None,
            format!("Entrypoint file {} not found", args.entrypoint),
        ));
    }
    //Get directory of entrypoint
    let entrypoint_module_name = match entrypoint_path.file_name() {
        None => {
            error_context.add_error(GeneralError(
                None,
                format!("Invalid entrypoint file {}", entrypoint_path.display()),
            ));
            "Error".to_string()
        }
        Some(filename) => filename.to_string_lossy().to_string(),
    };

    let project_name = args
        .project_root_name
        .map_or_else(|| entrypoint_module_name.clone(), |s| String::from(s));
    let entrypoint_mod = ModuleIdentifier::from_filename(entrypoint_module_name);

    let (search_dirs, errors): (Vec<PathBuf>, Vec<PathBuf>) = args
        .search_paths
        .iter()
        .map(|dir| PathBuf::from(dir.as_ref()))
        .partition(|dir| dir.is_dir());

    if !errors.is_empty() {
        for err in errors {
            error_context.add_error(GeneralError(
                None,
                format!("{} is not a valid search directory", err.display()),
            ));
        }
    }

    error_context.handle_errors()?;

    let mut module_lookup = HashMap::new();

    match parse(
        &mut module_lookup,
        &search_dirs,
        entrypoint_mod,
        entrypoint_path,
        error_context,
        interner,
    ) {
        Ok(main_module) => error_context.handle_errors().map(|_| UntypedModuleTree {
            search_dirs,
            main_module,
            lookup: module_lookup,
        }),
        Err(e) => {
            error_context.add_error(e);
            error_context.handle_errors().unwrap_err();
            Err(())
        }
    }
}

pub fn parse<'input>(
    tree: &mut HashMap<ModuleIdentifier, UntypedModule>,
    search_dirs: &Vec<PathBuf>,
    name: ModuleIdentifier,
    path: PathBuf,
    err: &mut ErrorContext<'input>,
    interner: &SymbolNameInterner,
) -> Result<UntypedModule, CompilerError<'input>> {
    let code = read_to_string(&path).map_err(|e| {
        GeneralError(
            Some(Box::new(e)),
            format!("Error opening file {}", &path.display()),
        )
    })?;
    unimplemented!();

    let ast_res = parser::parse(&code, &interner);
    err.add_file(&name, &path, code);
    let ast = ast_res.map_err(|pe| ParserError(pe, name.clone()))?;

    let dependencies = find_imported_modules(&ast)
        .iter()
        .filter_map(|dep| match try_resolve_module(search_dirs, &path, &dep.0) {
            Ok(found_path) => {
                match parse(tree, search_dirs, dep.0.clone(), found_path, err, interner) {
                    Ok(m) => Some(m),
                    Err(e) => {
                        err.add_error(e);
                        None
                    }
                }
            }
            Err(search_dirs) => {
                let e = ImportError {
                    importing_mod: name.clone(),
                    imported: dep.0.clone(),
                    search_dirs,
                    location: dep.1,
                };
                err.add_error(e);
                None
            }
        })
        .collect();

    let module_data = UntypedModuleData {
        name: name.clone(),
        ast,
        dependencies,
        path,
    };

    let module = UntypedModule(Rc::new(module_data));
    tree.insert(name, module.clone());
    Ok(module)
}
