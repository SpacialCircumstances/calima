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
    context: Context
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct TypeRef(usize);

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Level(u64);

impl Level {
    fn incremented(&self) -> Self {
        Level(self.0 + 1)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct GenericId(u64);

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum TypeVar {
    Link(TypeRef),
    Generic(GenericId),
    Unbound(GenericId, Level)
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum BaseType {
    Bool,
    Int,
    Float,
    String,
    Char
}

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Constant(BaseType),
    Parameterized(Box<Type>, Vec<Type>),
    Arrow(Box<Type>, Box<Type>),
    Var(TypeRef)
}

pub struct Context {
    types: Vec<TypeVar>,
    id_counter: usize
}

impl Context {
    pub fn new() -> Self {
        Context {
            id_counter: 0,
            types: Vec::new()
        }
    }

    fn next_type(&mut self) -> TypeRef {
        let tid = TypeRef(self.id_counter);
        self.id_counter += 1;
        tid
    }
}

impl Index<TypeRef> for Context {
    type Output = TypeVar;

    fn index(&self, index: TypeRef) -> &Self::Output {
        return &self.types[index.0];
    }
}

#[derive(Clone)]
struct Environment<'a> {
    values: HashMap<&'a str, Type>
}

impl<'a> Environment<'a> {
    fn new() -> Self {
        Environment {
            values: HashMap::new()
        }
    }

    fn add(&mut self, name: &'a str, tp: Type) {
        self.values.insert(name, tp);
    }

    fn lookup(&self, name: &'a str) -> Option<Type> {
        self.values.get(name).map(|t| t.clone())
    }
}

fn infer_expr<'input>(env: &mut Environment<'input>, ctx: &mut Context, level: Level, expr: &Expr<'input, Span>) -> (Type, Expr<'input, TypeData>) {
    match expr {
        Expr::Variable(name, data) => {
            //For now only deal with simple names
            //TODO: Inst
            let name = name.first().expect("Identifier must have size >= 1");
            let tp = env.lookup(name).expect("Error: Variable not found");
            (tp.clone(), Expr::Variable(vec![ name ], TypeData { typ: Some(tp), position: *data }))
        },
        Expr::Lambda { regions, params, body, data } => {
            let mut body_env = env;
            let mut param_types = Vec::with_capacity(params.len());
            let mut tparams = Vec::with_capacity(params.len());
            for param in params {
                let tp = Type::Var(ctx.next_type());
                param_types.push(tp.clone());
                bind_in_env(ctx, body_env, tp, param);
                tparams.push(map_pattern(param));
            }
            let (body_tp, body) = infer_block(body_env, ctx, level, body);
            let func_type = make_func_type(&param_types, body_tp);
            (func_type.clone(), Expr::Lambda {
                regions: regions.iter().map(|r| map_region(r)).collect(),
                params: tparams,
                body,
                data: TypeData {
                    typ: Some(func_type),
                    position: *data
                }
            })
        }
        _ => unimplemented!()
    }
}

fn make_func_type(params: &[Type], res: Type) -> Type {
    if params.len() == 1 {
        Type::Arrow(Box::new(params.first().unwrap().clone()), Box::new(res))
    } else {
        let head = params.first().expect("Func must have at least one parameter");
        Type::Arrow(Box::new(head.clone()), Box::new(make_func_type(&params[1..], res)))
    }
}

fn infer_statement<'input>(env: &mut Environment<'input>, ctx: &mut Context, level: &mut Level, statement: &Statement<'input, Span>) -> Option<Statement<'input, TypeData>> {
    match statement {
        Statement::Region(_, _) => None,
        Statement::Do(reg, expr, dat) => {
            let (expr_type, expr) = infer_expr(env, ctx, *level, expr);
            Some(Statement::Do(reg.map(|r| map_region(&r)), expr, TypeData { typ: Some(expr_type), position: *dat }))
        },
        Statement::Let(mods, reg, pat, expr, pos) => {
            //TODO: Recursion
            let (expr_type, expr) = infer_expr(env, ctx, *level, expr);
            bind_in_env(ctx, env, expr_type, pat);
            *level = level.incremented();
            Some(Statement::Let(mods.clone(), reg.map(|r| map_region(&r)), map_pattern(pat), expr, TypeData { typ: None, position: *pos }))
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

fn bind_in_env<'input>(ctx: &mut Context, env: &mut Environment<'input>, tp: Type, pattern: &Pattern<'input, Span>) {
    match pattern {
        Pattern::Name(ident, ta, _) => {
            //TODO: Type annotation checking
            env.add(ident.to_name(), tp)
        },
        Pattern::Any(_) => (),
        _ => unimplemented!()
    }
}

fn map_region<'a>(r: &RegionAnnotation<'a, Span>) -> RegionAnnotation<'a, TypeData> {
    RegionAnnotation(r.0, TypeData { typ: None, position: r.1 })
}

fn infer_top_level_statement<'input>(env: &mut Environment<'input>, ctx: &mut Context, level: Level, tls: &TopLevelStatement<'input, Span>) -> TopLevelStatement<'input, TypeData> {
    unimplemented!()
}

fn infer_block<'input>(env: &mut Environment<'input>, ctx: &mut Context, level: Level, block: &Block<'input, Span>) -> (Type, Block<'input, TypeData>) {
    let mut block_env = env.clone();
    let mut level = level;
    let (result_tp, result) = infer_expr(&mut block_env, ctx, level, &block.result);
    (result_tp, Block {
        statements: block.statements.iter().filter_map(|st| infer_statement(&mut block_env, ctx, &mut level, st)).collect(),
        result: Box::new(result)
    })
}

fn infer_top_level_block<'input>(env: &mut Environment<'input>, ctx: &mut Context, level: Level, tlb: &TopLevelBlock<'input, Span>) -> TopLevelBlock<'input, TypeData> {
    TopLevelBlock {
        top_levels: tlb.top_levels.iter().map(|st| infer_top_level_statement(env, ctx, level, st)).collect(),
        block: infer_block(env, ctx, level, &tlb.block).1
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
    typ: Option<Type>
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