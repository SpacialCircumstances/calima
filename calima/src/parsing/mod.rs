use std::collections::HashMap;
use std::fs::read_to_string;
use std::iter::once;
use std::path::PathBuf;
use std::rc::Rc;

use crate::ast::find_imported_modules;
use crate::common::*;
use crate::errors::CompilerError::*;
use crate::errors::{CompilerError, ErrorContext};
use crate::modules::{UntypedModule, UntypedModuleData, UntypedModuleTree};
use crate::symbol_names::StringInterner;
use crate::CompilerState;

pub mod lexer;
pub mod parser;
pub mod token;

pub fn parse<'input>(
    name: ModuleName,
    id: ModuleId,
    path: PathBuf,
    err: &mut ErrorContext,
    interner: &StringInterner,
) -> Result<UntypedModule, CompilerError> {
    let code = read_to_string(&path).map_err(|e| {
        GeneralError(
            Some(Box::new(e)),
            format!("Error opening file {}", &path.display()),
        )
    })?;

    let ast_res = parser::parse(&code, &interner);
    err.add_file(&name, &path, code.clone());
    let ast =
        ast_res.map_err(|pe| ParserError(crate::errors::ParserError::from(pe), name.clone()))?;

    let dependencies = find_imported_modules(&ast);

    let module_data = UntypedModuleData {
        name: name.clone(),
        ast,
        dependencies,
        path,
        id,
    };

    let module = UntypedModule(Rc::new(module_data));
    Ok(module)
}
