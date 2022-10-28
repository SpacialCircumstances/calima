#[macro_use]
extern crate lalrpop_util;

use crate::common::ModuleIdentifier;
use quetta::Text;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use symbol_names::StringInterner;

use crate::errors::{CompilerError, ErrorContext};
use crate::parsing::parse_all_modules;

mod ast;
mod common;
mod errors;
mod formatting;
mod ir;
mod modules;
mod parsing;
pub mod symbol_names;
mod typechecker;
mod types;

pub struct CompilerState {
    module_paths: Vec<PathBuf>,
    project_name: Text,
    output_file: PathBuf,
    entrypoint: PathBuf,
    error_context: ErrorContext,
    interner: StringInterner,
    mod_resolution_table: HashMap<ModuleIdentifier, PathBuf>,
}

impl CompilerState {
    pub fn construct(
        mut error_context: ErrorContext,
        interner: StringInterner,
        entrypoint: &PathBuf,
        search_paths: &Vec<PathBuf>,
        output_file: &Option<PathBuf>,
    ) -> Result<CompilerState, ()> {
        match entrypoint
            .file_stem()
            .map(|e| Text::from(e.to_string_lossy().to_string().as_str()))
            .ok_or(())
        {
            Ok(project_name) => match entrypoint.parent() {
                Some(parent) => {
                    let entrypoint_parent = parent.to_path_buf();
                    let mut search_paths = search_paths.clone();
                    search_paths.insert(0, entrypoint_parent.clone());

                    for path in &search_paths {
                        if !path.is_dir() {
                            error_context.add_error(CompilerError::GeneralError(
                                None,
                                format!(
                                    "Module directory does not exist/is not accessible: {}",
                                    path.display()
                                ),
                            ));

                            return Err(());
                        }
                    }

                    let output_file = match output_file {
                        Some(of) => of.clone(),
                        None => {
                            entrypoint_parent.with_file_name(OsString::from(project_name.as_str()))
                        }
                    };

                    Ok(CompilerState {
                        module_paths: search_paths,
                        entrypoint: entrypoint.clone(),
                        project_name,
                        output_file,
                        error_context,
                        interner,
                        mod_resolution_table: HashMap::new(),
                    })
                }
                None => {
                    error_context.add_error(CompilerError::GeneralError(
                        None,
                        format!(
                            "Cannot resolve parent directory of main file: {}",
                            project_name
                        ),
                    ));
                    Err(())
                }
            },
            Err(_) => {
                error_context.add_error(CompilerError::GeneralError(
                    None,
                    format!("Cannot resolve entrypoint: {}", entrypoint.display()),
                ));
                Err(())
            }
        }
    }

    fn resolve_within_search_dir(
        search_dir: &PathBuf,
        module: &ModuleIdentifier,
    ) -> Option<PathBuf> {
        let mut resolved_from_here = search_dir.clone();

        for x in module.identifier() {
            resolved_from_here.push(Path::new(x.as_str()));
        }

        resolved_from_here.set_extension(".ca");

        match resolved_from_here.exists() {
            true => Some(resolved_from_here),
            false => None,
        }
    }

    pub fn resolve(&mut self, module: ModuleIdentifier) -> Result<PathBuf, ()> {
        match self.mod_resolution_table.entry(module) {
            Entry::Occupied(occ) => Ok(occ.get().clone()),
            Entry::Vacant(entr) => {
                let path = self
                    .module_paths
                    .iter()
                    .find_map(|sd| Self::resolve_within_search_dir(sd, &module))
                    .ok_or(())?;
                entr.insert(path.clone());
                Ok(path)
            }
        }
    }
}

pub fn compile(
    entrypoint: &PathBuf,
    search_paths: &Vec<PathBuf>,
    output_file: &Option<PathBuf>,
) -> Result<(), ()> {
    let mut errors = ErrorContext::new();
    let interner = StringInterner::new();
    let state = CompilerState::construct(errors, interner, entrypoint, search_paths, output_file)?;
    let module_context = parse_all_modules(&mut errors, &interner, state)?;
    let typed_context = typechecker::typecheck(&mut errors, module_context, &interner)?;
    Ok(())
}
