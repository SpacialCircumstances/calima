use crate::compiler::ModuleTreeContext;
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;

pub struct TypedContext {

}

pub fn typecheck(string_interner: &StringInterner, errors: &mut ErrorContext, module_ctx: ModuleTreeContext) -> Result<TypedContext, ()> {
    errors.handle_errors().map(|()| TypedContext {})
}