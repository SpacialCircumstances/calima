use crate::compiler::{ModuleTreeContext, UntypedModuleData};
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::{HashMap, HashSet};
use std::ops::{Index, Add};
use im_rc::HashMap as ImmMap;
use crate::ast_common::{NumberType, Literal, Identifier, Pattern};
use crate::ast::{Expr, Statement, TopLevelStatement, Block, TopLevelBlock, RegionAnnotation, TypeAnnotation};
use std::collections::hash_map::Entry;
use crate::typed_ast::{TBlock, TStatement, TExpression, TExprData};

pub struct TypedModuleData<'input>(Context, TBlock<'input>);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct GenericId(usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    String,
    Unit,
    Char
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParameterizedType {
    Function,
    Tuple(u32)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TypeDefinition {
    Primitive(PrimitiveType),
    Parameterized(ParameterizedType) //TODO: User-defined records, sums
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Basic(TypeDefinition),
    Parameterized(Box<Type>, Vec<Type>),
    Var(GenericId)
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scheme(HashSet<GenericId>, Type);

impl Scheme {
    fn simple(tp: Type) -> Self {
        Scheme(HashSet::new(), tp)
    }
}

pub struct Substitution {
    subst: Vec<Option<Type>>
}

impl Substitution {
    fn new() -> Self {
        Substitution {
            subst: Vec::new()
        }
    }

    fn add(&mut self, key: GenericId, value: Type) {
        let idx = key.0;
        if idx >= self.subst.len() {
            self.subst.resize(idx - 1, Option::None);
        }
        self.subst[idx] = Some(value);
    }

    fn subst(&self, typ: Type) -> Type {
        match typ {
            Type::Basic(td) => typ,
            Type::Var(v) => self[v].as_ref().map(|t| t.clone()).unwrap_or(typ),
            Type::Parameterized(t, params) => Type::Parameterized(Box::new(self.subst(*t)), params.into_iter().map(|t| self.subst(t)).collect())
        }
    }
}

impl Index<GenericId> for Substitution {
    type Output = Option<Type>;

    fn index(&self, index: GenericId) -> &Self::Output {
        let idx = index.0;
        self.subst.get(idx).unwrap_or(&Option::None)
    }
}

pub struct Context {
    generic_id: usize,
    subst: Substitution
}

impl Context {
    pub fn new() -> Self {
        Context {
            generic_id: 0,
            subst: Substitution::new()
        }
    }

    fn next_id(&mut self) -> GenericId {
        let id = self.generic_id;
        self.generic_id += 1;
        GenericId(id)
    }

    fn new_generic(&mut self) -> Type {
        let tr = self.next_id();
        Type::Var(tr)
    }

    fn unify(&mut self, t1: Type, t2: Type) {
        unimplemented!();
    }
}

#[derive(Clone)]
struct Environment<'a> {
    values: HashMap<&'a str, Scheme>,
    mono_vars: HashSet<GenericId>
}

impl<'a> Environment<'a> {
    fn new() -> Self {
        Environment {
            values: HashMap::new(),
            mono_vars: HashSet::new()
        }
    }

    fn add(&mut self, name: &'a str, sch: Scheme) {
        self.values.insert(name, sch);
    }

    fn lookup(&self, name: &'a str) -> Option<&Scheme> {
        self.values.get(name)
    }

    fn add_monomorphic_var(&mut self, id: GenericId) {
        self.mono_vars.insert(id);
    }

    fn is_mono(&self, gid: GenericId) -> bool {
        self.mono_vars.contains(&gid)
    }
}

fn infer_expr<'input>(env: &mut Environment<'input>, ctx: &mut Context, expr: &Expr<'input, Span>) -> TExpression<'input> {
    match expr {
        Expr::Literal(lit, _) => TExpression::new(TExprData::Literal(lit.clone()), Scheme::simple(get_literal_type(lit))),
        _ => unimplemented!()
    }
}

fn get_literal_type(lit: &Literal) -> Type {
    Type::Basic(TypeDefinition::Primitive(match lit {
        Literal::Boolean(_) => PrimitiveType::Bool,
        Literal::Unit => PrimitiveType::Unit,
        Literal::String(_) => PrimitiveType::String,
        Literal::Number(_, NumberType::Float) => PrimitiveType::Float,
        Literal::Number(_, NumberType::Integer) => PrimitiveType::Int
    }))
}

fn infer_statement<'input>(env: &mut Environment<'input>, ctx: &mut Context, statement: &Statement<'input, Span>) -> Option<TStatement<'input>> {
    match statement {
        Statement::Region(_, _) => None,
        Statement::Do(_, expr, _) => Some(TStatement::Do(infer_expr(env, ctx, expr))),
        Statement::Let(mods, _, pattern, value, _) => {
            None
        }
    }
}

fn infer_block<'input>(env: &mut Environment<'input>, ctx: &mut Context, block: &Block<'input, Span>) -> TBlock<'input> {
    let mut block_env = env.clone();
    let result = infer_expr(&mut block_env, ctx, &block.result);
    TBlock {
        statements: block.statements.iter().filter_map(|st| infer_statement(&mut block_env, ctx, st)).collect(),
        res: Box::new(result)
    }
}

fn process_top_level<'input>(env: &mut Environment<'input>, ctx: &mut Context, tls: &TopLevelStatement<'input, Span>) {

}

fn infer_top_level_block<'input>(env: &mut Environment<'input>, ctx: &mut Context,tlb: &TopLevelBlock<'input, Span>) -> TBlock<'input> {
    tlb.top_levels.iter().for_each(|st| process_top_level(env, ctx, st));
    TBlock {
        statements: tlb.block.statements.iter().filter_map(|st| infer_statement(env, ctx, st)).collect(),
        res: Box::new(infer_expr(env, ctx, &*tlb.block.result))
    }
}

fn typecheck_module<'input>(unchecked: Module<UntypedModuleData<'input>>, deps: Vec<&Module<TypedModuleData<'input>>>) -> Module<TypedModuleData<'input>> {
    //TODO: Import dependencies into context
    let mut context = Context::new();
    let mut env = Environment::new();
    let infered_ast = infer_top_level_block(&mut env, &mut context, &unchecked.data.0);
    Module {
        data: TypedModuleData(context, infered_ast),
        name: unchecked.name,
        path: unchecked.path,
        depth: unchecked.depth,
        deps: unchecked.deps
    }
}

pub struct TypeData {
    position: Span,
    typ: Option<Type>
}

pub struct TypedContext<'input> {
    pub modules: HashMap<ModuleIdentifier, Module<TypedModuleData<'input>>>,
}

pub fn typecheck<'input>(string_interner: &StringInterner, errors: &mut ErrorContext, mut module_ctx: ModuleTreeContext<'input>) -> Result<TypedContext<'input>, ()> {
    let mut ctx = TypedContext {
        modules: HashMap::new()
    };
    let mut ordered_modules: Vec<Module<UntypedModuleData<'input>>> = module_ctx.modules.drain().map(|(_, module)| module).collect();
    ordered_modules.sort_by(|m1, m2| m1.depth.cmp(&m2.depth));

    for module in ordered_modules {
        let deps = module.deps.iter().map(|(d, _)| ctx.modules.get(d).expect("Fatal error: Dependent module not found")).collect();
        ctx.modules.insert(module.name.clone(), typecheck_module(module, deps));
    }

    errors.handle_errors().map(|()| ctx)
}