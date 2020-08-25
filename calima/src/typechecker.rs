use crate::compiler::ModuleTreeContext;
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::HashMap;
use std::ops::{Index, Add};
use im_rc::HashMap as ImmMap;
use crate::ast::{Expr, Statement, TopLevelStatement, Block, TopLevelBlock, RegionAnnotation, Pattern, Identifier};

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

#[derive(Clone)]
struct Environment<'a> {
    values: ImmMap<&'a str, TypeId>
}

impl<'a> Environment<'a> {
    fn new() -> Self {
        Environment {
            values: ImmMap::new()
        }
    }

    fn add(&self, name: &'a str, tp: TypeId) -> Self {
        Environment {
            values: self.values.update(name, tp)
        }
    }
}

fn infer_expr<'input>(env: &mut Environment<'input>, ctx: &mut Context<'input>, level: Level, expr: &Expr<'input, Span>) -> (TypeId, Expr<'input, TypeData>) {
    unimplemented!()
}

fn infer_statement<'input>(env: &mut Environment<'input>, ctx: &mut Context<'input>, level: Level, statement: &Statement<'input, Span>) -> Option<Statement<'input, TypeData>> {
    match statement {
        Statement::Region(_, _) => None,
        Statement::Do(reg, expr, dat) => {
            let (expr_type, expr) = infer_expr(env, ctx, level, expr);
            Some(Statement::Do(reg.map(map_region), expr, TypeData { typ: Some(expr_type), position: *dat }))
        },
        Statement::Let(mods, reg, pat, expr, pos) => {
            //TODO: Recursion
            let (expr_type, expr) = infer_expr(env, ctx, level, expr);
            *env = bind_in_env(ctx, env, expr_type, pat);
            Some(Statement::Let(mods.clone(), reg.map(map_region), map_pattern(pat), expr, TypeData { typ: None, position: *pos }))
        }
    }
}

fn map_pattern<'input>(p: &Pattern<'input, Span>) -> Pattern<'input, TypeData> {
    match p {
        Pattern::Any(pos) => Pattern::Any(TypeData { typ: None, position: *pos }),
        Pattern::Name(ident, ta, pos) => Pattern::Name(map_identifier(ident), None, TypeData { typ: None, position: *pos }),
        _ => unimplemented!()
    }
}

fn map_identifier<'input>(ident: &Identifier<'input, Span>) -> Identifier<'input, TypeData> {
    match ident {
        Identifier::Operator(name, position) => Identifier::Operator(name, TypeData { typ: None, position: *position }),
        Identifier::Simple(name, position) => Identifier::Simple(name, TypeData { typ: None, position: *position }),
    }
}

fn bind_in_env<'input>(ctx: &mut Context<'input>, env: &mut Environment<'input>, tp: TypeId, pattern: &Pattern<'input, Span>) -> Environment<'input> {
    match pattern {
        Pattern::Name(ident, ta, _) => {
            //TODO: Type annotation checking
            env.add(ident.to_name(), tp)
        },
        Pattern::Any(_) => env.clone(),
        _ => unimplemented!()
    }
}

fn map_region(r: RegionAnnotation<Span>) -> RegionAnnotation<TypeData> {
    RegionAnnotation(r.0, TypeData { typ: None, position: r.1 })
}

fn infer_top_level_statement<'input>(env: &mut Environment<'input>, ctx: &mut Context<'input>, level: Level, tls: &TopLevelStatement<'input, Span>) -> TopLevelStatement<'input, TypeData> {
    unimplemented!()
}

fn infer_block<'input>(env: &mut Environment<'input>, ctx: &mut Context<'input>, level: Level, block: &Block<'input, Span>) -> Block<'input, TypeData> {
    let mut block_env = env;
    Block {
        statements: block.statements.iter().filter_map(|st| infer_statement(block_env, ctx, level, st)).collect(),
        result: Box::new(infer_expr(env, ctx, level, &block.result).1)
    }
}

fn infer_top_level_block<'input>(env: &mut Environment<'input>, ctx: &mut Context<'input>, level: Level, tlb: &TopLevelBlock<'input, Span>) -> TopLevelBlock<'input, TypeData> {
    TopLevelBlock {
        top_levels: tlb.top_levels.iter().map(|st| infer_top_level_statement(env, ctx, level, st)).collect(),
        block: infer_block(env, ctx, level, &tlb.block)
    }
}

fn typecheck_module<'input>(unchecked: Module<'input, Span>, deps: Vec<&TypedModule<'input>>) -> TypedModule<'input> {
    //TODO: Import dependencies into context
    let mut context = Context::new();
    let mut env = Environment::new();
    let infered_ast = infer_top_level_block(&mut env, &mut context, Level(0), &unchecked.ast);
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
    typ: Option<TypeId>
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