use crate::compiler::ModuleTreeContext;
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::Module;
use crate::token::Span;

pub struct TypedContext {

}

pub fn typecheck(string_interner: &StringInterner, errors: &mut ErrorContext, module_ctx: ModuleTreeContext) -> Result<TypedContext, ()> {
    let mut orderered_modules: Vec<&Module<Span>> = module_ctx.modules.values().collect();
    orderered_modules.sort_by(|m1, m2| m1.depth.cmp(&m2.depth));

    errors.handle_errors().map(|()| TypedContext {})
}