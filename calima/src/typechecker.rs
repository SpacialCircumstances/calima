use crate::compiler::ModuleTreeContext;
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::HashMap;

fn typecheck_module<'input>(unchecked: &Module<'input, Span>, deps: &[Module<'input, TypeData>]) -> Module<'input, TypeData> {
    unimplemented!()
}

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
    let mut ordered_modules: Vec<&Module<Span>> = module_ctx.modules.values().collect();
    ordered_modules.sort_by(|m1, m2| m1.depth.cmp(&m2.depth));

    for module in ordered_modules {
        let deps = module.deps.iter().map(|(d, _)| ctx.modules.get(d).expect("Fatal error: Dependent module not found")).collect();
        ctx.modules.insert(module.name.clone(), typecheck_module(module, deps))
    }

    errors.handle_errors().map(|()| ctx)
}