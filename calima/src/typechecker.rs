use crate::compiler::{ModuleTreeContext, UntypedModuleData};
use crate::errors::ErrorContext;
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier, Associativity, OperatorSpecification};
use crate::token::Span;
use std::collections::{HashMap, HashSet};
use std::ops::Index;
use crate::ast_common::{NumberType, Literal, MatchPattern, BindPattern};
use crate::ast::{Expr, Statement, TopLevelStatement, Block, TopLevelBlock, TypeAnnotation, Modifier, OperatorElement};
use crate::typed_ast::{TBlock, TStatement, TExpression, TExprData, Unit};
use crate::types::{Type, GenericId, Scheme, TypeDefinition, PrimitiveType, build_function, ExportValue, Exports, ComplexType, Region};
use crate::prelude::prelude;
use crate::util::all_max;
use std::convert::TryFrom;

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

    fn subst(&self, typ: &Type) -> Type {
        match typ {
            Type::Basic(_) => typ.clone(),
            Type::Var(v) => self[*v].as_ref().map(|t| t.clone()).unwrap_or_else(|| typ.clone()),
            Type::Parameterized(t, params) => Type::Parameterized(t.clone(), params.into_iter().map(|t| self.subst(t)).collect()),
            Type::Reference(reg, t) => Type::Reference(*reg, Box::new(self.subst(&*t)))
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
    subst: Substitution,
    region_id: usize
}

impl Context {
    pub fn new() -> Self {
        Context {
            generic_id: 0,
            subst: Substitution::new(),
            region_id: 0
        }
    }

    fn next_id(&mut self) -> GenericId {
        let id = self.generic_id;
        self.generic_id += 1;
        GenericId(id)
    }

    fn next_region_id(&mut self) -> usize {
        let id = self.region_id;
        self.region_id += 1;
        id
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
            Type::Basic(_) => (),
            Type::Reference(_, t) => self.check_occurs(gid, &*t)
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

fn to_type<Data>(ctx: &mut Context, env: &Environment, ta: &TypeAnnotation<Data>) -> Result<Type, String> {
    match ta {
        TypeAnnotation::Name(name, _) => match PrimitiveType::try_from(*name) {
            Ok(pt) => Ok(Type::Basic(TypeDefinition::Primitive(pt))),
            Err(()) => Err(format!("{} is not a recognized type", name))
        },
        TypeAnnotation::Function(ta1, ta2) => {
            to_type(ctx, env, &*ta1).and_then(|t1| to_type(ctx, env, &*ta2).map(|t2| (t1, t2))).map(|(t1, t2)| Type::Parameterized(ComplexType::Function, vec![ t1, t2 ]))
        },
        _ => unimplemented!()
    }
}

#[derive(Clone)]
struct Environment<'a> {
    values: HashMap<&'a str, Scheme>,
    operators: HashMap<&'a str, OperatorSpecification>,
    mono_vars: HashSet<GenericId>,
    depth: usize
}

impl<'a> Environment<'a> {
    fn new() -> Self {
        Environment {
            values: HashMap::new(),
            operators: HashMap::new(),
            mono_vars: HashSet::new(),
            depth: 0
        }
    }

    fn add_operator(&mut self, name: &'a str, sch: Scheme, op: OperatorSpecification) {
        self.values.insert(name, sch);
        self.operators.insert(name, op);
    }

    fn add(&mut self, name: &'a str, sch: Scheme) {
        self.values.insert(name, sch);
    }

    fn lookup(&self, name: &'a str) -> Option<&Scheme> {
        self.values.get(name)
    }

    fn lookup_operator(&self, name: &'a str) -> Option<(&Scheme, &OperatorSpecification)> {
        self.values.get(name).and_then(|sch| self.operators.get(name).map(|op| (sch, op)))
    }

    fn add_monomorphic_var(&mut self, id: GenericId) {
        self.mono_vars.insert(id);
    }

    fn replace<F: Fn(GenericId) -> Option<Type>>(tp: &Type, mapper: &F) -> Type {
        match tp {
            Type::Basic(_) => tp.clone(),
            Type::Parameterized(p, ps) => Type::Parameterized(*p, ps.iter().map(|p| Self::replace(p, mapper)).collect()),
            Type::Var(id) => (mapper)(*id).unwrap_or_else(|| tp.clone()),
            Type::Reference(reg, t) => Type::Reference(*reg, Box::new(Self::replace(&*t, mapper)))
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

    fn import_scheme(ctx: &mut Context, tp: &Scheme) -> Scheme {
        let mapping: HashMap<GenericId, GenericId> = tp.0.iter().map(|v| (*v, ctx.next_id())).collect();
        let tp = Self::replace(&tp.1, &|gid| mapping.get(&gid).copied().map(Type::Var));
        let vars = mapping.values().copied().collect();
        Scheme(vars, tp)
    }

    fn import_module(&mut self, ctx: &mut Context, exports: &Exports<'a>) {
        exports.iter_vars().for_each(|(name, tp)| self.import(ctx, name, tp));
    }

    fn import<'b: 'a>(&mut self, ctx: &mut Context, name: &'b str, exv: &ExportValue) {
        match exv {
            ExportValue::Value(tp) => {
                self.add(name, Self::import_scheme(ctx, tp))
            },
            ExportValue::Operator(op, tp) => {
                self.add_operator(name, Self::import_scheme(ctx, tp), op.clone())
            }
        }
    }

    fn new_region(&mut self, ctx: &mut Context) -> Region {
        let rid = ctx.next_region_id();
        Region::new(rid, self.depth)
    }
}

fn function_call<'input>(ctx: &mut Context, tfunc: TExpression<'input>, args: Vec<TExpression<'input>>) -> TExpression<'input> {
    let argtypes: Vec<Type> = args.iter().map(TExpression::typ).cloned().collect();
    let ret = ctx.new_generic();
    let arg_fun_type = build_function(&argtypes, &ret);
    println!("{}, {}", tfunc.typ(), &arg_fun_type);
    ctx.unify(tfunc.typ(), &arg_fun_type);
    TExpression::new(TExprData::FunctionCall(tfunc.into(), args), ret)
}

fn infer_expr<'input, Data>(env: &mut Environment<'input>, ctx: &mut Context, expr: &Expr<'input, Data>) -> TExpression<'input> {
    match expr {
        Expr::Literal(lit, _) => TExpression::new(TExprData::Literal(lit.clone()), get_literal_type(lit)),
        Expr::Variable(varname, _) => {
            let scheme = env.lookup(varname).expect(format!("Variable {} not found", varname).as_str());
            TExpression::new(TExprData::Variable(varname), env.inst(ctx, scheme))
        },
        Expr::Lambda { params, body, data: _ } => {
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
            function_call(ctx, tfunc, targs)
        },
        Expr::OperatorCall(elements, _) => {
            transform_operators(env, ctx, elements)
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
        Expr::OperatorAsFunction(name, _) => {
            let (op_sch, _) = env.lookup_operator(name).unwrap();
            let op_tp = env.inst(ctx, op_sch);
            TExpression::new(TExprData::Variable(name), op_tp)
        }
        _ => unimplemented!()
    }
}

fn call_operator<'input>(ctx: &mut Context, exprs: &mut Vec<TExpression<'input>>, last_name: &'input str, last_type: &Type) {
    let r = exprs.pop().unwrap();
    let l = exprs.pop().unwrap();
    let last_expr = TExpression::new(TExprData::Variable(last_name), last_type.clone());
    let fc = function_call(ctx, last_expr, vec![l, r ]);
    exprs.push(fc);
}

fn transform_operators<'input, Data>(env: &mut Environment<'input>, ctx: &mut Context, elements: &Vec<OperatorElement<'input, Data>>) -> TExpression<'input> {
    let mut bin_ops: Vec<(&str, Type, u32, Associativity)> = Vec::new();
    let mut un_ops: Vec<(&str, Type)> = Vec::new();
    let mut exprs = Vec::new();

    for el in elements {
        match el {
            OperatorElement::Operator(name, _) => {
                let (op_tp, op_spec) = env.lookup_operator(name).expect("Operator not found");
                let op_tp = env.inst(ctx, op_tp);
                match op_spec {
                    OperatorSpecification::Infix(op_prec, assoc) => {
                        match bin_ops.last() {
                            None => bin_ops.push((*name, op_tp, *op_prec, *assoc)),
                            Some((_, _, last_prec, last_assoc)) => {
                                if last_prec == op_prec {
                                    if assoc == &Associativity::None || last_assoc == &Associativity::None {
                                        panic!("Encountered repeated operator without associativity")
                                    } else if assoc == &Associativity::Left {
                                        let (last_name, last_type, _, _) = bin_ops.pop().unwrap();
                                        call_operator(ctx, &mut exprs, last_name, &last_type);
                                    }
                                } else if last_prec > op_prec {
                                    let (last_name, last_type, _, _) = bin_ops.pop().unwrap();
                                    call_operator(ctx, &mut exprs, last_name, &last_type);
                                }
                                bin_ops.push((*name, op_tp, *op_prec, *assoc))
                            }
                        }
                    }
                    OperatorSpecification::Prefix => {
                        un_ops.push((*name, op_tp))
                    }
                }
            },
            OperatorElement::Expression(expr) => {
                let mut expr = infer_expr(env, ctx, expr);

                while let Some((op_name, op_tp)) = un_ops.pop() {
                    let op_expr = TExpression::new(TExprData::Variable(op_name), op_tp.clone());
                    expr = function_call(ctx, op_expr, vec![ expr ]);
                }

                exprs.push(expr);
            }
        }
    }

    //TODO: If unary ops remain, error

    while let Some((op_name, op_type, _, _)) = bin_ops.pop() {
        call_operator(ctx, &mut exprs, op_name, &op_type);
    }

    exprs.pop().unwrap()
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

fn infer_statement<'input, Data>(env: &mut Environment<'input>, ctx: &mut Context, statement: &Statement<'input, Data>) -> Option<TStatement<'input>> {
    match statement {
        Statement::Region(_, _) => None,
        Statement::Do(expr, _) => Some(TStatement::Do(infer_expr(env, ctx, expr))),
        Statement::Let(mods, pattern, value, _) => {
            let v = if mods.contains(&Modifier::Rec) {
                let var = ctx.new_generic();
                let mut body_env = env.clone();
                bind_to_pattern(&mut body_env, pattern, &Scheme::simple(var.clone()));
                let v = infer_expr(&mut body_env, ctx, value);
                ctx.unify(&var, &v.typ());
                v
            } else {
                infer_expr(env, ctx, value)
            };
            bind_to_pattern(env, pattern, &env.generalize(&v.typ()));
            Some(TStatement::Let(v, map_bind_pattern(pattern)))
        },
        Statement::LetOperator(mods, op, name, ta, expr, _) => {
            //TODO: Type annotations
            //TODO: Check for unary/binary
            let v = if mods.contains(&Modifier::Rec) {
                let var = ctx.new_generic();
                let mut body_env = env.clone();
                body_env.add_operator(name, Scheme::simple(var.clone()), op.clone());
                let v = infer_expr(&mut body_env, ctx, expr);
                ctx.unify(&var, &v.typ());
                v
            } else {
                infer_expr(env, ctx, expr)
            };
            env.add_operator(name, env.generalize(v.typ()), op.clone());
            Some(TStatement::Let(v, BindPattern::Name(name, None, Unit::unit())))
        }
    }
}

fn map_bind_pattern<'input, Data>(pattern: &BindPattern<'input, TypeAnnotation<Data>, Data>) -> BindPattern<'input, Unit, Unit> {
    match pattern {
        BindPattern::Any(_) => BindPattern::Any(Unit::unit()),
        BindPattern::Name(id, _, _) => BindPattern::Name(id, None, Unit::unit()),
        _ => unimplemented!()
    }
}

fn map_match_pattern<'input, Data>(pattern: &MatchPattern<'input, TypeAnnotation<Data>, Data>) -> MatchPattern<'input, Unit, Unit> {
    match pattern {
        MatchPattern::Any(_) => MatchPattern::Any(Unit::unit()),
        MatchPattern::Literal(lit, _) => MatchPattern::Literal(lit.clone(), Unit::unit()),
        MatchPattern::Name(id, _, _) => MatchPattern::Name(id, None, Unit::unit()),
        _ => unimplemented!()
    }
}

fn bind_to_pattern<'input, Data>(env: &mut Environment<'input>, pattern: &BindPattern<'input, TypeAnnotation<Data>, Data>, sch: &Scheme) {
    match pattern {
        BindPattern::Any(_) => (),
        BindPattern::Name(idt, ta, _) => {
            //TODO: Check type annotation
            env.add(idt, sch.clone());
        },
        _ => ()
    }
}

fn infer_block<'input, Data>(env: &mut Environment<'input>, ctx: &mut Context, block: &Block<'input, Data>) -> TBlock<'input> {
    let mut block_env = env.clone();
    let tstatements = block.statements.iter().filter_map(|st| infer_statement(&mut block_env, ctx, st)).collect();
    let result = infer_expr(&mut block_env, ctx, &block.result);
    TBlock {
        statements: tstatements,
        res: Box::new(result)
    }
}

fn process_top_level<'input, Data>(env: &mut Environment<'input>, ctx: &mut Context, tls: &TopLevelStatement<'input, Data>) {

}

fn infer_top_level_block<'input, Data>(env: &mut Environment<'input>, ctx: &mut Context,tlb: &TopLevelBlock<'input, Data>) -> TBlock<'input> {
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
    env.import_module(&mut context, &prelude);

    let infered_ast = infer_top_level_block(&mut env, &mut context, &unchecked.data.0);
    //TODO
    let rettype = context.subst.subst(infered_ast.res.typ());
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

#[cfg(test)]
mod tests {
    use crate::typechecker::{Environment, Context, transform_operators};
    use crate::prelude::prelude;
    use crate::ast::OperatorElement::*;
    use crate::ast::{Expr, OperatorElement};
    use crate::ast_common::{Literal, NumberType};
    use crate::typed_ast::{TExpression, TExprData};
    use crate::types::{Type, int};

    fn int_lit(lit: &str) -> OperatorElement<()> {
        Expression(Expr::Literal(Literal::Number(lit, NumberType::Integer), ()))
    }

    fn int_lit_typed(lit: &str) -> TExpression {
        TExpression::new(TExprData::Literal(Literal::Number(lit, NumberType::Integer)), int())
    }

    fn lookup(env: &Environment, ctx: &mut Context, name: &str) -> Type {
        let sch = env.lookup(name).unwrap();
        env.inst(ctx, sch)
    }

    fn add_op<'a>(env: &Environment<'a>, ctx: &mut Context) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("+"), lookup(env, ctx, "+"))
    }

    fn mul_op<'a>(env: &Environment<'a>, ctx: &mut Context) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("*"), lookup(env, ctx, "*"))
    }

    fn neg_op<'a>(env: &Environment<'a>, ctx: &mut Context) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("~"), lookup(env, ctx, "~"))
    }

    fn neg_expr<'a>(env: &Environment<'a>, ctx: &mut Context, expr: TExpression<'a>) -> TExpression<'a> {
        TExpression::new(TExprData::FunctionCall(neg_op(env, ctx).into(), vec![
            expr
        ]), int())
    }

    #[test]
    fn op_transform_simple_binary() {
        let mut ctx = Context::new();
        let mut env = Environment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            int_lit("1"),
            Operator("+", ()),
            int_lit("2")
        ];
        let exprs = TExpression::new(TExprData::FunctionCall(add_op(&env, &mut ctx).into(), vec![
            int_lit_typed("1"),
            int_lit_typed("2")
        ]), int());
        let res = transform_operators(&mut env, &mut ctx, &ops);
        ctx.unify(exprs.typ(), res.typ());
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_binary_precedence() {
        let mut ctx = Context::new();
        let mut env = Environment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            int_lit("2"),
            Operator("+", ()),
            int_lit("2"),
            Operator("*", ()),
            int_lit("3")
        ];
        let exprs = TExpression::new(TExprData::FunctionCall(add_op(&env, &mut ctx).into(), vec![
            int_lit_typed("2"),
            TExpression::new(TExprData::FunctionCall(mul_op(&env, &mut ctx).into(), vec![
                int_lit_typed("2"),
                int_lit_typed("3")
            ]), int())
        ]), int());
        let res = transform_operators(&mut env, &mut ctx, &ops);
        ctx.unify(exprs.typ(), res.typ());
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_unary_simple() {
        let mut ctx = Context::new();
        let mut env = Environment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            Operator("~", ()),
            Operator("~", ()),
            int_lit("1")
        ];
        let e1 = neg_expr(&env, &mut ctx, int_lit_typed("1"));
        let exprs = neg_expr(&env, &mut ctx, e1);
        let res = transform_operators(&mut env, &mut ctx, &ops);
        ctx.unify(exprs.typ(), res.typ());
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_unary_binary() {
        let mut ctx = Context::new();
        let mut env = Environment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            Operator("~", ()),
            int_lit("1"),
            Operator("+", ()),
            Operator("~", ()),
            Operator("~", ()),
            int_lit("2")
        ];
        let e2 = neg_expr(&env, &mut ctx, int_lit_typed("2"));
        let exprs = TExpression::new(TExprData::FunctionCall(add_op(&env, &mut ctx).into(), vec![
            neg_expr(&env, &mut ctx, int_lit_typed("1")),
            neg_expr(&env, &mut ctx, e2)
        ]), int());
        let res = transform_operators(&mut env, &mut ctx, &ops);
        ctx.unify(exprs.typ(), res.typ());
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_complex() {
        let mut ctx = Context::new();
        let mut env = Environment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            Operator("~", ()),
            int_lit("1"),
            Operator("*", ()),
            int_lit("2"),
            Operator("+", ()),
            Operator("~", ()),
            int_lit("3")
        ];
        let e2 = neg_expr(&env, &mut ctx, int_lit_typed("2"));
        let exprs = TExpression::new(TExprData::FunctionCall(add_op(&env, &mut ctx).into(), vec![
            TExpression::new(TExprData::FunctionCall(mul_op(&env, &mut ctx).into(), vec![
                neg_expr(&env, &mut ctx, int_lit_typed("1")),
                int_lit_typed("2")
            ]), int()),
            neg_expr(&env, &mut ctx, int_lit_typed("3"))
        ]), int());
        let res = transform_operators(&mut env, &mut ctx, &ops);
        ctx.unify(exprs.typ(), res.typ());
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_bin_assoc() {
        let mut ctx = Context::new();
        let mut env = Environment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            int_lit("4"),
            Operator("+", ()),
            int_lit("5"),
            Operator("+", ()),
            int_lit("6")
        ];
        let exprs = TExpression::new(TExprData::FunctionCall(add_op(&env, &mut ctx).into(), vec![
            TExpression::new(TExprData::FunctionCall(add_op(&env, &mut ctx).into(), vec![
                int_lit_typed("4"),
                int_lit_typed("5")
            ]), int()),
            int_lit_typed("6")
        ]), int());
        let res = transform_operators(&mut env, &mut ctx, &ops);
        ctx.unify(exprs.typ(), res.typ());
        assert_eq!(exprs.data(), res.data())
    }
}