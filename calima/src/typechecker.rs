use crate::compiler::{ModuleTreeContext, UntypedModuleData};
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier};
use crate::token::Span;
use std::collections::{HashMap, HashSet};
use std::ops::{Index, Deref};
use crate::ast_common::{NumberType, Literal, Identifier, MatchPattern, BindPattern, Associativity};
use crate::ast::{Expr, Statement, TopLevelStatement, Block, TopLevelBlock, TypeAnnotation, Modifier};
use crate::typed_ast::{TBlock, TStatement, TExpression, TExprData, Unit};
use crate::types::{Type, GenericId, Scheme, TypeDefinition, PrimitiveType, build_function};
use crate::prelude::prelude;
use crate::util::all_max;

pub struct TypedModuleData<'input>(Context, TBlock<'input>);

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
            self.subst.resize(idx + 1, Option::None);
        }
        self.subst[idx] = Some(value);
    }

    fn subst(&self, typ: Type) -> Type {
        match typ {
            Type::Basic(_) => typ,
            Type::Var(v) => self[v].as_ref().map(|t| t.clone()).unwrap_or(typ),
            Type::Parameterized(t, params) => Type::Parameterized(t, params.into_iter().map(|t| self.subst(t)).collect())
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
        let existing = self.subst[gid].clone();
        match existing {
            Some(t) => self.unify(&t, t2),
            None => self.subst.add(gid, t2.clone())
        }
    }

    fn check_occurs(&self, gid: GenericId, t: &Type) {
        match t {
            Type::Var(i) => {
                if gid == *i {
                    panic!("Recursive type")
                }

                if let Some(next) = &self.subst[*i] {
                    self.check_occurs(gid, next)
                }
            }
            Type::Parameterized(_, params) => {
                for p in params {
                    self.check_occurs(gid, p);
                }
            }
            Type::Basic(_) => ()
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) {
        if t1 != t2 {
            match (t1, t2) {
                (Type::Basic(td1), Type::Basic(td2)) => if td1 != td2 {
                    panic!(format!("Cannot unify {} with {}", td1, td2))
                },
                (Type::Var(gid), _) => {
                    self.check_occurs(*gid, t2);
                    self.bind(*gid, t2)
                },
                (_, Type::Var(gid)) => self.bind(*gid, t1),
                (Type::Parameterized(p1, params1), Type::Parameterized(p2, params2)) => {
                    if p1 != p2 || params1.len() != params2.len() {
                        panic!(format!("Cannot unify {} with {}", t1, t2))
                    }
                    params1.iter().zip(params2.iter()).for_each(|(p1, p2)| self.unify(p1, p2));
                },
                _ => panic!(format!("Cannot unify {} with {}", t1, t2))
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

    fn replace<F: Fn(GenericId) -> Option<Type>>(tp: &Type, mapper: &F) -> Type {
        match tp {
            Type::Basic(_) => tp.clone(),
            Type::Parameterized(p, ps) => Type::Parameterized(*p, ps.iter().map(|p| Self::replace(p, mapper)).collect()),
            Type::Var(id) => (mapper)(*id).unwrap_or_else(|| tp.clone())
        }
    }

    fn inst(&self, ctx: &mut Context, sch: &Scheme) -> Type {
        let mapping: HashMap<GenericId, Type> = sch.0.iter().map(|v| (*v, ctx.new_generic())).collect();
        Self::replace(&sch.1, &|id| mapping.get(&id).cloned())
    }

    fn generalize(&self, tp: &Type) -> Scheme {
        fn gen_rec(tp: &Type, mono_vars: &HashSet<GenericId>, scheme_vars: &mut HashSet<GenericId>) {
            match tp {
                Type::Parameterized(_, params) => {
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

    fn import<'b: 'a>(&mut self, ctx: &mut Context, name: &'b str, tp: &Scheme) {
        let mapping: HashMap<GenericId, GenericId> = tp.0.iter().map(|v| (*v, ctx.next_id())).collect();
        let tp = Self::replace(&tp.1, &|gid| mapping.get(&gid).copied().map(Type::Var));
        let vars = mapping.values().copied().collect();
        let scheme = Scheme(vars, tp);
        self.add(name, scheme)
    }
}

fn function_call<'input>(env: &mut Environment<'input>, ctx: &mut Context, tfunc: TExpression<'input>, args: Vec<TExpression<'input>>) -> TExpression<'input> {
    let argtypes: Vec<Type> = args.iter().map(TExpression::typ).cloned().collect();
    let ret = ctx.new_generic();
    let arg_fun_type = build_function(&argtypes, &ret);
    println!("{}, {}", tfunc.typ(), &arg_fun_type);
    ctx.unify(tfunc.typ(), &arg_fun_type);
    TExpression::new(TExprData::FunctionCall(tfunc.into(), args), ret)
}

//Multiples of 10 -> Left assoc
//Multiples of 5 -> Right assoc
//Others -> No assoc
fn get_precedence(op: &str) -> u32 {
    match op {
        "|>" => 100,
        "*" | "/" | ".*" | "./" | "%" => 80,
        "+" | "-" | ".+" | ".-" => 60,
        "==" | "!=" | "<" | ">" | "<=" | ">=" | "=>" => 59,
        "&&" => 40,
        "||" => 20,
        _ => 10
    }
}

fn get_assoc(prec: u32) -> Associativity {
    if prec % 10 == 0 {
        Associativity::Left
    } else if prec % 5 == 0 {
        Associativity::Right
    } else {
        Associativity::None
    }
}

fn transform_operators<'input>(env: &mut Environment<'input>, ctx: &mut Context, ops: &[(&'input str, u32, Scheme)], exprs: &[Expr<'input, Span>]) -> TExpression<'input> {
    let highest_ops = all_max(ops.iter().enumerate(), |(_, (_, p, _))| *p);
    let (next_op_idx, op) = ops.iter().enumerate().max_by_key(|(_, op)| op.1).expect("Error finding highest precedence operator");
    let l_ops = &ops[..next_op_idx];
    let l_exprs = &exprs[..next_op_idx + 1];
    let r_ops = &ops[next_op_idx + 1..];
    let r_exprs = &exprs[next_op_idx + 1..];
    let l = if l_ops.is_empty() {
        infer_expr(env, ctx, &l_exprs[0])
    } else {
        transform_operators(env, ctx, l_ops, l_exprs)
    };
    let r = if r_ops.is_empty() {
        infer_expr(env, ctx, &r_exprs[0])
    } else {
        transform_operators(env, ctx, r_ops, r_exprs)
    };
    let (op_name, _, op_scheme) = op;
    let op_type = env.inst(ctx, &op_scheme);
    let op_expr = TExpression::new(TExprData::Variable(vec![ op_name ]), op_type);
    function_call(env, ctx, op_expr, vec![ l, r ])
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
                let tp = Type::Var(gen);
                let param_type = Scheme::simple(tp.clone());
                bind_to_pattern(&mut body_env, param, &param_type);
                param_types.push(tp);
            }

            let body = infer_block(&mut body_env, ctx, body);
            let scheme = build_function(&param_types, body.res.typ());
            TExpression::new(TExprData::Lambda(params.iter().map(|p| map_bind_pattern(p)).collect(), body), scheme)
        },
        Expr::FunctionCall(func, args, _) => {
            let tfunc = infer_expr(env, ctx, func);
            let targs = args.iter().map(|e| infer_expr(env, ctx, e)).collect();
            function_call(env, ctx, tfunc, targs)
        },
        Expr::UnaryOperatorCall(op, expr, _) => {
            let schem = env.lookup(op).expect(format!("Operator not found: {}", op).as_str());
            let tp = env.inst(ctx, schem);
            let texpr = infer_expr(env, ctx, expr.deref());
            function_call(env, ctx, TExpression::new(TExprData::UnaryOperator(op), tp), vec![ texpr ])
        }
        Expr::OperatorCall(exprs, operators, _) => {
            //TODO: Figure out precedence and stuff
            //Find precedence and types for operators
            let resolve_operators: Vec<Result<(&str, u32, Scheme), &str>> = operators.iter().map(|op| {
                match env.lookup(op) {
                    Some(tp) => Ok((*op, get_precedence(op), tp.clone())),
                    None => Err(*op)
                }
            }).collect();
            let operators: Vec<(&str, u32, Scheme)> = resolve_operators.into_iter().map(|r| r.unwrap()).collect();
            transform_operators(env, ctx, &operators[..], exprs)
        }
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
                (MatchPattern::Literal(Literal::Boolean(true), Unit::unit()), ttrue),
                (MatchPattern::Literal(Literal::Boolean(false), Unit::unit()), tfalse)
            ];
            TExpression::new(TExprData::Case(tcond.into(), case), rett)
        },
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
            if mods.contains(&Modifier::Rec) {
                let var = ctx.new_generic();
                let mut body_env = env.clone();
                bind_to_pattern(&mut body_env, pattern, &Scheme::simple(var.clone()));
                let v = infer_expr(&mut body_env, ctx, value);
                ctx.unify(&var, &v.typ());
                let vt = &env.generalize(v.typ());
                bind_to_pattern(env, pattern, vt);
                Some(TStatement::Let(v, map_bind_pattern(pattern)))
            } else {
                let v = infer_expr(env, ctx, value);
                bind_to_pattern(env, pattern, &env.generalize(&v.typ()));
                Some(TStatement::Let(v, map_bind_pattern(pattern)))
            }
        }
    }
}

fn map_bind_pattern<'input>(pattern: &BindPattern<'input, TypeAnnotation<Span>, Span>) -> BindPattern<'input, Unit, Unit> {
    match pattern {
        BindPattern::Any(_) => BindPattern::Any(Unit::unit()),
        BindPattern::Name(id, _, _) => BindPattern::Name(map_identifier(id), None, Unit::unit()),
        _ => unimplemented!()
    }
}

fn map_match_pattern<'input>(pattern: &MatchPattern<'input, TypeAnnotation<Span>, Span>) -> MatchPattern<'input, Unit, Unit> {
    match pattern {
        MatchPattern::Any(_) => MatchPattern::Any(Unit::unit()),
        MatchPattern::Literal(lit, _) => MatchPattern::Literal(lit.clone(), Unit::unit()),
        MatchPattern::Name(id, _, _) => MatchPattern::Name(map_identifier(id), None, Unit::unit()),
        _ => unimplemented!()
    }
}

fn map_identifier<'input>(id: &Identifier<'input, Span>) -> Identifier<'input, Unit> {
    match id {
        Identifier::Operator(name, _) => Identifier::Operator(name, Unit::unit()),
        Identifier::Simple(name, _) => Identifier::Simple(name, Unit::unit())
    }
}

fn bind_to_pattern<'input>(env: &mut Environment<'input>, pattern: &BindPattern<'input, TypeAnnotation<Span>, Span>, sch: &Scheme) {
    match pattern {
        BindPattern::Any(_) => (),
        BindPattern::Name(idt, ta, _) => {
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
    let prelude = prelude();
    prelude.iter_vars().for_each(|(name, tp)| env.import(&mut context, name, tp));

    let infered_ast = infer_top_level_block(&mut env, &mut context, &unchecked.data.0);
    //TODO
    let rettype = context.subst.subst(infered_ast.res.typ().clone());
    println!("Return type of program: {}", rettype);
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
        let name = module.name.clone();
        let typed_module = typecheck_module(module, deps);
        ctx.modules.insert(name, typed_module);
    }

    errors.handle_errors().map(|()| ctx)
}