use crate::ast::{TopLevelBlock, TopLevelStatement};
use crate::compiler::ModuleIdentifier;

pub fn find_imported_modules<D>(ast: &TopLevelBlock<D>) -> Vec<ModuleIdentifier> {
    ast.top_levels.iter().fold(Vec::new(), |mut imports, statement| {
        match statement {
            TopLevelStatement::Import(module_id, _, _) => {
                imports.push(ModuleIdentifier::from_name(module_id))
            },
            _ => ()
        }
        imports
    })
}