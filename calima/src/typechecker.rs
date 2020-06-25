use crate::compiler::ModuleTreeContext;
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::HashMap;

pub struct TypeData {
    position: Span
}

pub struct TypedContext<'input> {
    pub modules: HashMap<ModuleIdentifier, Module<'input, TypeData>>,
}

pub fn typecheck<'input>(string_interner: &StringInterner, errors: &mut ErrorContext, module_ctx: ModuleTreeContext<'input>) -> Result<TypedContext<'input>, ()> {
    let mut ctx = TypedContext {
        modules: HashMap::new()
    };
    let mut orderered_modules: Vec<&Module<Span>> = module_ctx.modules.values().collect();
    orderered_modules.sort_by(|m1, m2| m1.depth.cmp(&m2.depth));

    errors.handle_errors().map(|()| ctx)
}