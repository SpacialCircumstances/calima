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
use crate::modules::{TypedModule, UntypedModule};

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
    mod_resolution_table: HashMap<ModuleIdentifier, Result<PathBuf, ()>>,
    mod_parsed_table: HashMap<ModuleIdentifier, Result<UntypedModule, ()>>,
    mod_typed_table: HashMap<ModuleIdentifier, Result<TypedModule, ()>>,
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
                        mod_parsed_table: HashMap::new(),
                        mod_typed_table: HashMap::new(),
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

    fn resolve_cached(
        mrt: &mut HashMap<ModuleIdentifier, Result<PathBuf, ()>>,
        errors: &mut ErrorContext,
        module_paths: &Vec<PathBuf>,
        module: &ModuleIdentifier,
    ) -> Result<PathBuf, ()> {
        match mrt.entry(module.clone()) {
            Entry::Occupied(occ) => occ.get().clone(),
            Entry::Vacant(entr) => {
                match module_paths
                    .iter()
                    .find_map(|sd| Self::resolve_within_search_dir(sd, module))
                {
                    None => {
                        errors.add_error(CompilerError::GeneralError(
                            None,
                            format!("Could not resolve module {}", module), //TODO: Specific error type
                        ));
                        entr.insert(Err(()));
                        Err(())
                    }
                    Some(path) => {
                        entr.insert(Ok(path.clone()));
                        Ok(path)
                    }
                }
            }
        }
    }

    pub fn resolve(&mut self, module: &ModuleIdentifier) -> Result<PathBuf, ()> {
        Self::resolve_cached(
            &mut self.mod_resolution_table,
            &mut self.error_context,
            &self.module_paths,
            module,
        )
    }

    pub fn parse(&mut self, module: &ModuleIdentifier) -> Result<UntypedModule, ()> {
        match self.mod_parsed_table.entry(module.clone()) {
            Entry::Occupied(occ) => occ.get().clone(),
            Entry::Vacant(vac) => {
                let res = if let Ok(path) = Self::resolve_cached(
                    &mut self.mod_resolution_table,
                    &mut self.error_context,
                    &self.module_paths,
                    module,
                ) {
                    match parsing::parse(
                        module.clone(),
                        path,
                        &mut self.error_context,
                        &self.interner,
                    ) {
                        Ok(p) => Ok(p),
                        Err(e) => {
                            self.error_context.add_error(e);
                            Err(())
                        }
                    }
                } else {
                    Err(())
                };
                vac.insert(res.clone());
                res
            }
        }
    }

    pub fn typecheck(&mut self, module: &ModuleIdentifier) -> Result<TypedModule, ()> {
        match self.mod_typed_table.get(module) {
            None => {
                let parsed_module = self.parse(module)?;
                let dependencies = parsed_module
                    .0
                    .dependencies
                    .iter()
                    .map(|dep| self.typecheck(dep))
                    .collect::<Result<Vec<TypedModule>, ()>>()?;
                let res = typechecker::typecheck_module(
                    &parsed_module,
                    dependencies,
                    &mut self.error_context,
                    &self.interner,
                ); //TODO: What about main module check?
                self.mod_typed_table.insert(module.clone(), res.clone());
                res
            }
            Some(res) => res.clone(),
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
    let mut state =
        CompilerState::construct(errors, interner, entrypoint, search_paths, output_file)?;
    let main_mod_id = ModuleIdentifier::from_name(state.project_name.clone());
    let main_mod = state.typecheck(&main_mod_id)?;
    todo!();
    Ok(())
}
