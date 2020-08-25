use crate::compiler::ModuleTreeContext;
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::HashMap;
use std::ops::Index;
use im_rc::HashMap as ImmMap;
use crate::ast::{Expr, Statement, TopLevelStatement, Block, TopLevelBlock};

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

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Context {
            id_counter: 0,
            types: Vec::new()
        }
    }
}

impl<'a> Index<TypeId> for Context<'a> {
    type Output = Type<'a>;

    fn index(&self, index: TypeId) -> &Self::Output {
        return &self.types[index.0];
    }
}

struct Environment<'a> {
    values: ImmMap<&'a str, TypeId>
}

impl<'a> Environment<'a> {
    fn new() -> Self {
        Environment {
            values: ImmMap::new()
        }
    }
}

fn infer_expr<'input>(env: Environment<'input>, ctx: &mut Context<'input>, level: Level, expr: &Expr<'input, Span>) -> Expr<'input, TypeData> {
    unimplemented!()
}

fn infer_statement<'input>(env: Environment<'input>, ctx: &mut Context<'input>, level: Level, statement: &Statement<'input, Span>) -> Statement<'input, TypeData> {
    unimplemented!()
}

fn infer_top_level_statement<'input>(env: Environment<'input>, ctx: &mut Context<'input>, level: Level, tls: &TopLevelStatement<'input, Span>) -> TopLevelStatement<'input, TypeData> {
    unimplemented!()
}

fn infer_block<'input>(env: Environment<'input>, ctx: &mut Context<'input>, level: Level, block: &Block<'input, Span>) -> Block<'input, TypeData> {
    unimplemented!()
}

fn infer_top_level_block<'input>(env: Environment<'input>, ctx: &mut Context<'input>, level: Level, tlb: &TopLevelBlock<'input, Span>) -> TopLevelBlock<'input, TypeData> {
    unimplemented!()
}

fn typecheck_module<'input>(unchecked: Module<'input, Span>, deps: Vec<&TypedModule<'input>>) -> TypedModule<'input> {
    //TODO: Import dependencies into context
    let mut context = Context::new();
    let env = Environment::new();
    let infered_ast = infer_top_level_block(env, &mut context, Level(0), &unchecked.ast);
    let module = Module {
        ast: infered_ast,
        name: unchecked.name,
        path: unchecked.path,
        depth: unchecked.depth,
        deps: unchecked.deps
    };
    TypedModule {
        module,
        context
    }
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