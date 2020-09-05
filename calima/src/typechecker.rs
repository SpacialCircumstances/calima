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
use crate::typed_ast::{TBlock, TStatement, TExpression};

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
            Type::Constant(_) => typ,
            Type::Var(v) => self[v].as_ref().map(|t| t.clone()).unwrap_or(typ),
            Type::Arrow(t1, t2) => Type::Arrow(Box::new(self.subst(*t1)), Box::new(self.subst(*t2))),
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

fn instantiate(ctx: &mut Context, env: &Environment, typ: Type) -> Type {
    fn instantiate_rec(ctx: &mut Context, env: &Environment, typ: Type, mapping: &mut HashMap<GenericId, Type>) -> Type {
        match typ {
            Type::Constant(_) => typ,
            Type::Arrow(t1, t2) => Type::Arrow(Box::new(instantiate_rec(ctx, env, *t1, mapping)), Box::new(instantiate_rec(ctx, env, *t2, mapping))),
            Type::Parameterized(t, params) => Type::Parameterized(Box::new(instantiate_rec(ctx, env, *t, mapping)), params.into_iter().map(|t| instantiate_rec(ctx, env, t, mapping)).collect()),
            Type::Var(gid) => match &ctx.subst[gid] {
                Some(t) => instantiate_rec(ctx, env, t.clone(), mapping),
                None => if env.is_mono(gid) {
                    typ
                } else {
                    match mapping.entry(gid) {
                        Entry::Vacant(e) => {
                            let var = ctx.new_generic();
                            e.insert(var.clone());
                            var
                        },
                        Entry::Occupied(e) => e.get().clone()
                    }
                }
            }
        }
    }

    instantiate_rec(ctx, env, typ, &mut HashMap::new())
}

fn infer_expr<'input>(env: &mut Environment<'input>, ctx: &mut Context, expr: &Expr<'input, Span>) -> TExpression<'input> {
    match expr {
        Expr::Variable(name, data) => {
            //For now only deal with simple names
            let name = name.first().expect("Identifier must have size >= 1");
            let tp = instantiate(ctx, &env, env.lookup(name).expect("Error: Variable not found"));
            (tp.clone(), Expr::Variable(vec![ name ], TypeData { typ: Some(tp), position: *data }))
        },
        Expr::Lambda { regions, params, body, data } => {
            let mut body_env = env.clone();
            let mut param_types = Vec::with_capacity(params.len());
            let mut tparams = Vec::with_capacity(params.len());
            for param in params {
                let tid = ctx.next_id();
                let tp = Type::Var(tid);
                param_types.push(tp.clone());
                bind_in_env(ctx, &mut body_env, tp, param);
                body_env.add_monomorphic_var(tid);
                tparams.push(map_pattern(param));
            }
            let (body_tp, body) = infer_block(&mut body_env, ctx, body);
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
        },
        Expr::Literal(lit, data) => {
            let tp = get_literal_type(lit);
            (tp.clone(), Expr::Literal(*lit, TypeData { typ: Some(tp), position: *data }))
        },
        Expr::FunctionCall(fexpr, params, data) => {
            let (f_tp, tfexpr) = infer_expr(env, ctx, &**fexpr);
            let (res_type, ptypes) = apply_function_type(f_tp, params.len());
            let typed_params = params
                .iter()
                .zip(ptypes.into_iter())
                .map(|(p, expected_tp)| {
                    let (pt, npexpr) = infer_expr(env, ctx, p);
                    ctx.unify(pt, expected_tp);
                    npexpr
                }).collect();

            (res_type.clone(), Expr::FunctionCall(Box::new(tfexpr), typed_params, TypeData { typ: Some(res_type), position: *data }))
        },
        _ => unimplemented!()
    }
}

fn apply_function_type(ft: Type, depth: usize) -> (Type, Vec<Type>) {
    if depth == 0 {
        (ft, Vec::new())
    } else {
        match ft {
            Type::Arrow(f, n) => {
                let (rt, mut rest) = apply_function_type(*n, depth - 1);
                rest.push(*f);
                (rt, rest)
            },
            _ => panic!("Too many parameters")
        }
    }
}

fn get_literal_type(lit: &Literal) -> Type {
    Type::Constant(match lit {
        Literal::Boolean(_) => PrimitiveType::Bool,
        Literal::Unit => PrimitiveType::Unit,
        Literal::String(_) => PrimitiveType::String,
        Literal::Number(_, NumberType::Float) => PrimitiveType::Float,
        Literal::Number(_, NumberType::Integer) => PrimitiveType::Int
    })
}

fn make_func_type(params: &[Type], res: Type) -> Type {
    if params.len() == 1 {
        Type::Arrow(Box::new(params.first().unwrap().clone()), Box::new(res))
    } else {
        let head = params.first().expect("Func must have at least one parameter");
        Type::Arrow(Box::new(head.clone()), Box::new(make_func_type(&params[1..], res)))
    }
}

fn infer_statement<'input>(env: &mut Environment<'input>, ctx: &mut Context, statement: &Statement<'input, Span>) -> Option<TStatement<'input>> {
    None
}

fn generalize(ctx: &mut Context, env: &Environment, tp: Type) -> Type {
    unimplemented!();
}

fn bind_in_env<'input, Data>(ctx: &mut Context, env: &mut Environment<'input>, tp: Type, pattern: &Pattern<'input, TypeAnnotation<'input, Data>, Span>) {
    match pattern {
        Pattern::Name(ident, ta, _) => {
            //TODO: Type annotation checking
            env.add(ident.to_name(), tp)
        },
        Pattern::Any(_) => (),
        _ => unimplemented!()
    }
}

fn infer_block<'input>(env: &mut Environment<'input>, ctx: &mut Context, block: &Block<'input, Span>) -> (Type, Block<'input, TypeData>) {
    let mut block_env = env.clone();
    let (result_tp, result) = infer_expr(&mut block_env, ctx, &block.result);
    (result_tp, Block {
        statements: block.statements.iter().filter_map(|st| infer_statement(&mut block_env, ctx, st)).collect(),
        result: Box::new(result)
    })
}

fn process_top_level<'input>(env: &mut Environment<'input>, ctx: &mut Context, tls: &TopLevelStatement<'input, Span>) {

}

fn infer_top_level_block<'input>(env: &mut Environment<'input>, ctx: &mut Context,tlb: &TopLevelBlock<'input, Span>) -> TBlock<'input> {
    tlb.top_levels.iter().for_each(|st| process_top_level(env, ctx, st));
    TBlock {
        statements: tlb.block.statements.iter().filter_map(|st| infer_statement(env, ctx, st)).collect(),
        res: Box::new(infer_expr(env, ctx, &**tlb.block.result))
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