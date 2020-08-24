use crate::compiler::ModuleTreeContext;
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::HashMap;
use std::ops::Index;

//TODO: Convert types into general representation
pub struct TypedModule<'a> {
    module: Module<'a, TypeData>,
    context: Context<'a>
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct TypeId(usize);

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Level(u64);

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct GenericId(u64);

pub enum TypeVar {
    Link(TypeId),
    Generic(GenericId),
    Unbound(GenericId, Level)
}

pub enum Type<'a> {
    Constant(&'a str),
    Parameterized(Box<Type<'a>>, Vec<Type<'a>>),
    Arrow(Box<Type<'a>>, Box<Type<'a>>),
    Var(TypeVar)
}

pub struct Context<'a> {
    types: Vec<Type<'a>>,
    id_counter: usize
}

impl<'a> Index<TypeId> for Context<'a> {
    type Output = Type<'a>;

    fn index(&self, index: TypeId) -> &Self::Output {
        return &self.types[index.0];
    }
}

fn typecheck_module<'input>(unchecked: Module<'input, Span>, deps: Vec<&TypedModule<'input>>) -> TypedModule<'input> {
    //TODO: Import dependencies into context
    let mut context = Context {
        types: Vec::new(),
        id_counter: 0
    };
    unimplemented!()
}

pub struct TypeData {
    position: Span,
    typ: TypeId
}

pub struct TypedContext<'input> {
    pub modules: HashMap<ModuleIdentifier, TypedModule<'input>>,
}

pub fn typecheck<'input>(string_interner: &StringInterner, errors: &mut ErrorContext, mut module_ctx: ModuleTreeContext<'input>) -> Result<TypedContext<'input>, ()> {
    let mut ctx = TypedContext {
        modules: HashMap::new()
    };
    let mut ordered_modules: Vec<Module<Span>> = module_ctx.modules.drain().map(|(_, module)| module).collect();
    ordered_modules.sort_by(|m1, m2| m1.depth.cmp(&m2.depth));

    for module in ordered_modules {
        let deps = module.deps.iter().map(|(d, _)| ctx.modules.get(d).expect("Fatal error: Dependent module not found")).collect();
        ctx.modules.insert(module.name.clone(), typecheck_module(module, deps));
    }

    errors.handle_errors().map(|()| ctx)
}