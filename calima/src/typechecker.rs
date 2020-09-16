use crate::compiler::{ModuleTreeContext, UntypedModuleData};
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::{HashMap, HashSet};
use std::ops::Index;
use crate::ast_common::{NumberType, Literal, Identifier, Pattern};
use crate::ast::{Expr, Statement, TopLevelStatement, Block, TopLevelBlock, TypeAnnotation, Modifier};
use crate::typed_ast::{TBlock, TStatement, TExpression, TExprData, Unit};

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

fn func() -> Type {
    Type::Basic(TypeDefinition::Parameterized(ParameterizedType::Function))
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
            subst: (1..10).map(|_| None).collect()
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
            Type::Basic(_) => typ,
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

    fn bind(&mut self, gid: GenericId, t2: &Type) {
        match &self.subst[gid] {
            Some(t) => self.unify(&t.clone(), t2),
            None => self.subst.add(gid, t2.clone())
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) {
        if t1 != t2 {
            match (t1, t2) {
                (Type::Basic(td1), Type::Basic(td2)) => if td1 != td2 {
                    panic!(format!("Cannot unify {:?} with {:?}", td1, td2))
                },
                (Type::Var(gid), _) => self.bind(*gid, t2),
                (_, Type::Var(gid)) => self.bind(*gid, t1),
                (Type::Parameterized(p1, params1), Type::Parameterized(p2, params2)) => {
                    if p1 != p2 || params1.len() != params2.len() {
                        panic!(format!("Cannot unify {:?} with {:?}", t1, t2))
                    }
                    params1.iter().zip(params2.iter()).for_each(|(p1, p2)| self.unify(p1, p2));
                },
                _ => panic!(format!("Cannot unify {:?} with {:?}", t1, t2))
            }
        }
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

    fn inst(&self, ctx: &mut Context, sch: &Scheme) -> Type {
        fn inst_rec(tp: &Type, mapping: &HashMap<GenericId, Type>) -> Type {
            match tp {
                Type::Basic(_) => tp.clone(),
                Type::Parameterized(p, ps) => Type::Parameterized(inst_rec(&*p, mapping).into(), ps.iter().map(|p| inst_rec(p, mapping)).collect()),
                Type::Var(id) => mapping.get(id).cloned().unwrap_or_else(|| tp.clone())
            }
        }

        let mapping = sch.0.iter().map(|v| (*v, ctx.new_generic())).collect();
        inst_rec(&sch.1, &mapping)
    }

    fn generalize(&self, tp: &Type) -> Scheme {
        fn gen_rec(tp: &Type, mono_vars: &HashSet<GenericId>, scheme_vars: &mut HashSet<GenericId>) {
            match tp {
                Type::Parameterized(p, params) => {
                    gen_rec(&*p, mono_vars, scheme_vars);
                    for p in params {
                        gen_rec(p, mono_vars, scheme_vars);
                    }
                },
                Type::Var(gid) if !mono_vars.contains(gid) => {
                    scheme_vars.insert(*gid);
                },
                _ => ()
            }
        }
        let mut scheme_vars = HashSet::new();
        gen_rec(tp, &self.mono_vars, &mut scheme_vars);
        Scheme(scheme_vars, tp.clone())
    }
}

fn infer_expr<'input>(env: &mut Environment<'input>, ctx: &mut Context, expr: &Expr<'input, Span>) -> TExpression<'input> {
    match expr {
        Expr::Literal(lit, _) => TExpression::new(TExprData::Literal(lit.clone()), get_literal_type(lit)),
        Expr::Variable(name, _) => {
            let varname = *name.first().expect("Variable names must have at least one element");
            let scheme = env.lookup(varname).expect(format!("Variable {} not found", varname).as_str());
            TExpression::new(TExprData::Variable(name.clone()), env.inst(ctx, scheme))
        },
        Expr::Lambda { regions: _, params, body, data: _ } => {
            let mut body_env = env.clone();
            let mut param_types = Vec::with_capacity(params.len());
            for param in params {
                let gen = ctx.next_id();
                body_env.add_monomorphic_var(gen);
                let param_type = Scheme::simple(Type::Var(gen));
                bind_to_pattern(&mut body_env, param, &param_type);
                param_types.push(Type::Var(gen));
            }

            let body = infer_block(&mut body_env, ctx, body);
            let scheme = create_function_type(&param_types, body.res.typ());
            TExpression::new(TExprData::Lambda(params.iter().map(|p| map_pattern(p)).collect(), body), scheme)
        },
        Expr::FunctionCall(func, args, _) => {
            let tfunc = infer_expr(env, ctx, &*func);
            let targs: Vec<TExpression> = args.iter().map(|a| infer_expr(env, ctx, a)).collect();
            let argtypes: Vec<&Type> = targs.iter().map(TExpression::typ).collect();
            let ret = apply_function(ctx, tfunc.typ(), &argtypes);
            TExpression::new(TExprData::FunctionCall(tfunc.into(), targs), ret)
        },
        Expr::If { data: _, cond, if_true, if_false } => {
            let tcond = infer_expr(env, ctx, &*cond);
            ctx.unify(tcond.typ(), &Type::Basic(TypeDefinition::Primitive(PrimitiveType::Bool)));
            let mut true_env = env.clone();
            let mut false_env = env.clone();
            let ttrue = infer_block(&mut true_env, ctx, if_true);
            let tfalse = infer_block(&mut false_env, ctx, if_false);
            ctx.unify(ttrue.res.typ(), tfalse.res.typ());
            let rett = ttrue.res.typ().clone();
            let case = vec![
                (Pattern::Literal(Literal::Boolean(true), Unit::unit()), ttrue),
                (Pattern::Literal(Literal::Boolean(false), Unit::unit()), tfalse)
            ];
            TExpression::new(TExprData::Case(tcond.into(), case), rett)
        },
        _ => unimplemented!()
    }
}

fn as_function(func: &Type) -> Option<(&Type, &Type)> {
    match func {
        Type::Parameterized(p, params) => {
            match **p {
                Type::Basic(TypeDefinition::Parameterized(ParameterizedType::Function)) if params.len() == 2 => Some((&params[0], &params[1])),
                _ => None
            }
        },
        _ => None
    }
}

fn apply_function(ctx: &mut Context, func: &Type, args: &[&Type]) -> Type {
    match (as_function(func), args) {
        (Some((in_tp, out_tp)), [arg]) => {
            ctx.unify(in_tp, arg);
            out_tp.clone()
        },
        (Some((in_tp, out_tp)), _) if !args.is_empty() => {
            ctx.unify(in_tp, args.first().unwrap());
            apply_function(ctx, out_tp, &args[1..])
        },
        _ => panic!("Error calling function")
    }
}

fn create_function_type(params: &[Type], ret: &Type) -> Type {
    match params {
        [last] => Type::Parameterized(func().into(), vec![ last.clone(), ret.clone() ]),
        _ => {
            let c = params.first().expect("Error getting parameter");
            Type::Parameterized(func().into(), vec![ c.clone(), create_function_type(&params[1..], ret) ])
        }
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
            if mods.contains(&Modifier::Rec) {
                let var = ctx.new_generic();
                let mut body_env = env.clone();
                bind_to_pattern(&mut body_env, pattern, &Scheme::simple(var.clone()));
                let v = infer_expr(&mut body_env, ctx, value);
                ctx.unify(&var, &v.typ());
                let vt = &env.generalize(v.typ());
                bind_to_pattern(env, pattern, vt);
                Some(TStatement::Let(v, map_pattern(pattern)))
            } else {
                let v = infer_expr(env, ctx, value);
                bind_to_pattern(env, pattern, &env.generalize(&v.typ()));
                Some(TStatement::Let(v, map_pattern(pattern)))
            }
        }
    }
}

fn map_pattern<'input>(pattern: &Pattern<'input, TypeAnnotation<Span>, Span>) -> Pattern<'input, Unit, Unit> {
    match pattern {
        Pattern::Any(_) => Pattern::Any(Unit::unit()),
        Pattern::Literal(lit, _) => Pattern::Literal(lit.clone(), Unit::unit()),
        Pattern::Name(id, _, _) => Pattern::Name(map_identifier(id), None, Unit::unit()),
        _ => unimplemented!()
    }
}

fn map_identifier<'input>(id: &Identifier<'input, Span>) -> Identifier<'input, Unit> {
    match id {
        Identifier::Operator(name, _) => Identifier::Operator(name, Unit::unit()),
        Identifier::Simple(name, _) => Identifier::Simple(name, Unit::unit())
    }
}

fn bind_to_pattern<'input>(env: &mut Environment<'input>, pattern: &Pattern<'input, TypeAnnotation<Span>, Span>, sch: &Scheme) {
    match pattern {
        Pattern::Any(_) => (),
        Pattern::Name(idt, ta, _) => {
            //TODO: Check type annotation
            env.add(idt.to_name(), sch.clone());
        },
        _ => ()
    }
}

fn infer_block<'input>(env: &mut Environment<'input>, ctx: &mut Context, block: &Block<'input, Span>) -> TBlock<'input> {
    let mut block_env = env.clone();
    let tstatements = block.statements.iter().filter_map(|st| infer_statement(&mut block_env, ctx, st)).collect();
    let result = infer_expr(&mut block_env, ctx, &block.result);
    TBlock {
        statements: tstatements,
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
    //TODO
    let rettype = context.subst.subst(infered_ast.res.typ().clone());
    println!("Return type of program: {:?}", rettype);
    Module {
        data: TypedModuleData(context, infered_ast),
        name: unchecked.name,
        path: unchecked.path,
        depth: unchecked.depth,
        deps: unchecked.deps
    }
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