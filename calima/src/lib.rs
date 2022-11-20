#[macro_use]
extern crate lalrpop_util;

use crate::common::{ModuleId, ModuleName};
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
    mod_identifier_table: HashMap<ModuleName, ModuleId>,
    mod_resolution_table: HashMap<ModuleId, Result<PathBuf, ()>>,
    mod_parsed_table: HashMap<ModuleId, Result<UntypedModule, ()>>,
    mod_typed_table: HashMap<ModuleId, Result<TypedModule, ()>>,
    current_mod_id: usize,
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
                        mod_identifier_table: HashMap::new(),
                        mod_resolution_table: HashMap::new(),
                        mod_parsed_table: HashMap::new(),
                        mod_typed_table: HashMap::new(),
                        current_mod_id: 0,
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

    fn resolve_within_search_dir(search_dir: &PathBuf, module: &ModuleName) -> Option<PathBuf> {
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
        mrt: &mut HashMap<ModuleId, Result<PathBuf, ()>>,
        errors: &mut ErrorContext,
        module_paths: &Vec<PathBuf>,
        mod_id: ModuleId,
        module: &ModuleName,
    ) -> Result<PathBuf, ()> {
        match mrt.entry(mod_id) {
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

    pub fn identify(&mut self, module: &ModuleName) -> ModuleId {
        let mod_id = &mut self.current_mod_id;

        *self
            .mod_identifier_table
            .entry(module.clone())
            .or_insert_with(|| {
                let rmid = *mod_id;
                *mod_id += 1;
                ModuleId::new(rmid)
            })
    }

    pub fn resolve(&mut self, module: &ModuleName) -> Result<PathBuf, ()> {
        let mod_id = self.identify(module);
        Self::resolve_cached(
            &mut self.mod_resolution_table,
            &mut self.error_context,
            &self.module_paths,
            mod_id,
            module,
        )
    }

    pub fn parse(&mut self, module: &ModuleName) -> Result<UntypedModule, ()> {
        let mod_id = self.identify(module);

        match self.mod_parsed_table.entry(mod_id) {
            Entry::Occupied(occ) => occ.get().clone(),
            Entry::Vacant(vac) => {
                let res = if let Ok(path) = Self::resolve_cached(
                    &mut self.mod_resolution_table,
                    &mut self.error_context,
                    &self.module_paths,
                    mod_id,
                    module,
                ) {
                    match parsing::parse(
                        module.clone(),
                        mod_id,
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

    pub fn typecheck(&mut self, module: &ModuleName, is_main: bool) -> Result<TypedModule, ()> {
        let mod_id = self.identify(module);

        match self.mod_typed_table.get(&mod_id) {
            None => {
                let parsed_module = self.parse(module)?;
                let dependencies = parsed_module
                    .0
                    .dependencies
                    .iter()
                    .map(|dep| self.typecheck(dep, false))
                    .collect::<Result<Vec<TypedModule>, ()>>()?;
                let res = typechecker::typecheck_module(
                    &parsed_module,
                    dependencies,
                    &mut self.error_context,
                    &self.interner,
                )
                .and_then(|tm| {
                    if is_main {
                        typechecker::verify_main_module(
                            &tm,
                            &mut self.error_context,
                            &self.interner,
                        )
                        .map(|_| tm)
                    } else {
                        Ok(tm)
                    }
                });
                self.mod_typed_table.insert(mod_id, res.clone());
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
    let main_mod_id = ModuleName::from_name(state.project_name.clone());
    let main_mod = state.typecheck(&main_mod_id, true)?;
    todo!();
    Ok(())
}
