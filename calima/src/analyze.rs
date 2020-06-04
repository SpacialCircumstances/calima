use crate::ast::{TopLevelBlock, TopLevelStatement};
use crate::common::ModuleIdentifier;

pub fn find_imported_modules<D: Copy>(ast: &TopLevelBlock<D>) -> Vec<(ModuleIdentifier, D)> {
    ast.top_levels.iter().fold(Vec::new(), |mut imports, statement| {
        match statement {
            TopLevelStatement::Import(module_id, _, data) => {
                imports.push((ModuleIdentifier::from_name(module_id), *data));
            },
            _ => ()
        }
        imports
    })
}