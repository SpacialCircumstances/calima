#[macro_use]
extern crate lalrpop_util;

use quetta::Text;
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
}

impl CompilerState {
    pub fn construct(
        error_context: &mut ErrorContext,
        interner: &StringInterner,
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
                    search_paths.push(entrypoint_parent.clone());

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
}

pub fn compile(
    entrypoint: &PathBuf,
    search_paths: &Vec<PathBuf>,
    output_file: &Option<PathBuf>,
) -> Result<(), ()> {
    let mut errors = ErrorContext::new();
    let interner = StringInterner::new();
    let state = CompilerState::construct(
        &mut errors,
        &interner,
        entrypoint,
        search_paths,
        output_file,
    )?;
    let module_context = parse_all_modules(&mut errors, &interner, state)?;
    let typed_context = typechecker::typecheck(&mut errors, module_context, &interner)?;
    Ok(())
}
