use crate::compiler::{ModuleTreeContext, UntypedModuleData};
use crate::errors::{ErrorContext, CompilerError};
use crate::string_interner::StringInterner;
use crate::common::{Module, ModuleIdentifier, Associativity, OperatorSpecification};
use std::collections::{HashMap, HashSet};
use crate::ast_common::*;
use crate::ast::*;
use crate::typed_ast::*;
use crate::types::*;
use crate::prelude::prelude;
use std::convert::TryFrom;
use crate::token::Span;
use crate::typechecker::symbol_table::{SymbolTable, Location};
use crate::util::format_iter;
use crate::typechecker::substitution::Substitution;
use std::fmt::Debug;

mod symbol_table;
mod substitution;

pub struct TypedModuleData<'input>(Substitution<Type>, TBlock<'input>);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnificationSource {
    TypeAnnotation,
    TypeInference,
    OperatorConstraint(OperatorSpecification),
    If,
    BlockReturn
}

//TODO: Additional information
#[derive(Debug, Clone)]
pub struct TypeError<Data> {
    pub location: Data,
    pub message: String
}

impl<Data> TypeError<Data> {
    fn type_not_found(typename: &str, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Type {} not defined or imported", typename)
        }
    }

    fn var_not_found(varname: &str, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Variable {} not defined or imported", varname)
        }
    }

    fn operator_not_found(opname: &str, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Operator {} is not defined or imported", opname)
        }
    }

    fn unification(ue: UnificationError, expected: &Type, actual: &Type, location: Data) -> Self {
        let message = match ue {
            UnificationError::RecursiveType => String::from("Recursive type detected"),
            UnificationError::UnificationError => format!("Cannot unify types: Expected {}, but got {}", expected, actual),
            UnificationError::Propagation => String::from("Propagated type error")
        };
        TypeError {
            location,
            message
        }
    }

    fn repeated_unassociative_operators(location: Data, ops: &[&str]) -> Self {
        TypeError {
            location,
            message: format!("Repeated operators without associativity used: {}", format_iter(ops.iter(), ", "))
        }
    }

    fn remaining_unary_operators(location: Data, ops: &[&str]) -> Self {
        TypeError {
            location,
            message: format!("Unary prefix operators {} were used without an expression to apply them to", format_iter(ops.iter(), ", "))
        }
    }
}

#[derive(Debug, Clone)]
enum UnificationError {
    UnificationError,
    RecursiveType,
    Propagation
}

fn substitute(subst: &Substitution<Type>, typ: &Type) -> Type {
    match typ {
        Type::Basic(_) => typ.clone(),
        Type::Var(v) => match subst[(*v).0].as_ref() {
            Some(t) => substitute(subst, t),
            None => typ.clone()
        },
        Type::Parameterized(t, params) => Type::Parameterized(t.clone(), params.into_iter().map(|t| substitute(subst, t)).collect()),
        Type::Error => Type::Error,
        _ => unimplemented!()
    }
}

pub struct Context<Data: Copy + Debug> {
    generic_id: usize,
    type_subst: Substitution<Type>,
    errors: Vec<TypeError<Data>>,
    module: ModuleIdentifier
}

impl<Data: Copy + Debug> Context<Data> {
    pub fn new(module: ModuleIdentifier) -> Self {
        Context {
            generic_id: 0,
            type_subst: Substitution::new(),
            errors: Vec::new(),
            module
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

    fn bind(&mut self, gid: GenericId, t2: &Type) -> Result<(), UnificationError> {
        let existing = self.type_subst[gid.0].clone();
        match existing {
            Some(t) => self.unify_rec(&t, t2),
            None => Ok(self.type_subst.add(gid.0, t2.clone()))
        }
    }

    fn check_occurs(&self, gid: GenericId, t: &Type) -> Result<(), ()> {
        match t {
            Type::Var(i) => {
                if gid == *i {
                    Err(())
                } else if let Some(next) = &self.type_subst[(*i).0] {
                    self.check_occurs(gid, next)
                } else {
                    Ok(())
                }
            }
            Type::Parameterized(_, params) => {
                for p in params {
                    self.check_occurs(gid, p)?;
                }
                Ok(())
            }
            Type::Basic(_) => Ok(()),
            Type::Reference(_, t) => self.check_occurs(gid, &*t),
            Type::Error => panic!("Cannot occurs_check on erroneous type")
        }
    }

    fn unify_rec(&mut self, t1: &Type, t2: &Type) -> Result<(), UnificationError> {
        match (t1, t2) {
            (Type::Error, _) => Err(UnificationError::Propagation),
            (_, Type::Error) => Err(UnificationError::Propagation),
            (_, _) if t1 == t2 => Ok(()),
            (Type::Basic(td1), Type::Basic(td2)) if td1 != td2 => {
                Err(UnificationError::UnificationError)
            },
            (Type::Var(gid), _) => {
                self.check_occurs(*gid, t2).map_err(|()| UnificationError::RecursiveType)?;
                self.bind(*gid, t2)
            },
            (_, Type::Var(gid)) => self.bind(*gid, t1),
            (Type::Parameterized(p1, params1), Type::Parameterized(p2, params2)) => {
                if p1 != p2 || params1.len() != params2.len() {
                    Err(UnificationError::UnificationError)
                } else {
                    params1.iter().zip(params2.iter()).map(|(p1, p2)| self.unify_rec(p1, p2)).collect()
                }
            },
            _ => unimplemented!()
        }
    }

    fn unify(&mut self, target: &mut Type, with: &Type, source: UnificationSource, loc: Data) {
        if let Err(e) = self.unify_rec(target, with) {
            self.add_error(TypeError::unification(e, &with, &target, loc));
            *target = Type::Error;
        }
    }

    fn unify_check(&mut self, target: &mut Type, unify_target: &Type, with: &Type, source: UnificationSource, loc: Data) {
        if let Err(e) = self.unify_rec(unify_target, with) {
            self.add_error(TypeError::unification(e, &with, &unify_target, loc));
            *target = Type::Error;
        }
    }

    fn add_error(&mut self, te: TypeError<Data>) {
        self.errors.push(te);
    }

    fn lookup_var(&mut self, env: &Environment<Data>, name: &str, loc: Data) -> Type {
        match env.lookup(name) {
            Some(sch) => env.inst(self, sch),
            None => {
                self.add_error(TypeError::var_not_found(name, loc));
                Type::Error
            }
        }
    }

    fn lookup_operator(&mut self, env: &Environment<Data>, name: &str, loc: Data) -> (Type, Option<OperatorSpecification>) {
        match env.lookup_operator(name) {
            Some((tp, op)) => (env.inst(self, tp), Some(*op)),
            None => {
                self.add_error(TypeError::operator_not_found(name, loc));
                (Type::Error, None)
            }
        }
    }

    fn type_from_annotation<'input>(&mut self, env: &mut Environment<'input, Data>, ta: &TypeAnnotation<'input, Data>) -> Type {
        fn to_type<'input, Data: Copy + Debug>(ctx: &mut Context<Data>, env: &mut Environment<'input, Data>, ta: &TypeAnnotation<'input, Data>) -> Result<Type, TypeError<Data>> {
            match ta {
                TypeAnnotation::Name(name, loc) => match PrimitiveType::try_from(*name) {
                    Ok(pt) => Ok(Type::Basic(TypeDef::Primitive(pt))),
                    Err(()) => Err(TypeError::type_not_found(name, *loc))
                },
                TypeAnnotation::Function(ta1, ta2) => {
                    to_type(ctx, env, &*ta1).and_then(|t1| to_type(ctx, env, &*ta2).map(|t2| (t1, t2))).map(|(t1, t2)| Type::Parameterized(ComplexType::Function, vec![ t1, t2 ]))
                },
                TypeAnnotation::Generic(gname) => Ok(Type::Var(env.get_or_create_generic(ctx, gname.0, gname.1))),
                TypeAnnotation::Tuple(params) => params.iter().map(|p| to_type(ctx, env, p)).collect::<Result<Vec<Type>, TypeError<Data>>>().map(|ps| Type::Parameterized(ComplexType::Tuple(ps.len()), ps)),
                _ => unimplemented!()
            }
        }

        to_type(self, env, ta).unwrap_or_else(|e| {
            self.add_error(e);
            Type::Error
        })
    }

    fn bind_to_pattern<'input>(&mut self, env: &mut Environment<'input, Data>, pattern: &BindPattern<'input, TypeAnnotation<'input, Data>, Data>, sch: &Scheme) {
        match pattern {
            BindPattern::Any(_) => (),
            BindPattern::Name(idt, ta, loc) => {
                let mut sch = sch.clone();
                match ta {
                    Some(ta) => {
                        let tp = self.type_from_annotation(env, ta);
                        self.unify(&mut sch.2, &tp, UnificationSource::TypeAnnotation, *loc);
                    },
                    None => ()
                }
                env.add(idt, sch, *loc);
            },
            _ => ()
        }
    }

    fn call_operator<'input>(&mut self, exprs: &mut Vec<TExpression<'input>>, last_name: &'input str, last_type: &Type, loc: Data) {
        let r = exprs.pop().unwrap();
        let l = exprs.pop().unwrap();
        let last_expr = TExpression::new(TExprData::Variable(last_name), last_type.clone());
        let fc = function_call(self, last_expr, vec![l, r ], loc);
        exprs.push(fc)
    }

    fn transform_operators<'input>(&mut self, env: &mut Environment<'input, Data>, elements: &Vec<OperatorElement<'input, Data>>, top_location: Data) -> TExpression<'input> {
        let mut bin_ops: Vec<(&str, Type, u32, Associativity, Data)> = Vec::new();
        let mut un_ops: Vec<(&str, Type)> = Vec::new();
        let mut exprs = Vec::new();

        for el in elements {
            match el {
                OperatorElement::Operator(name, data) => {
                    let data = *data;
                    let (op_tp, op_spec) = self.lookup_operator(env, name, data);
                    match op_spec {
                        None => (),
                        Some(OperatorSpecification::Infix(op_prec, assoc)) => {
                            match bin_ops.last() {
                                None => bin_ops.push((*name, op_tp, op_prec, assoc, data)),
                                Some((last_op, _, last_prec, last_assoc, data)) => {
                                    let data = *data;
                                    if last_prec == &op_prec {
                                        if assoc == Associativity::None || last_assoc == &Associativity::None {
                                            self.add_error(TypeError::repeated_unassociative_operators(top_location, &vec![ *last_op, *name ]));
                                        } else if assoc == Associativity::Left {
                                            let (last_name, last_type, _, _, data) = bin_ops.pop().unwrap();
                                            self.call_operator(&mut exprs, last_name, &last_type, data);
                                        }
                                    } else if last_prec > &op_prec {
                                        let (last_name, last_type, _, _, data) = bin_ops.pop().unwrap();
                                        self.call_operator(&mut exprs, last_name, &last_type, data)
                                    }
                                    bin_ops.push((*name, op_tp, op_prec, assoc, data))
                                }
                            }
                        }
                        Some(OperatorSpecification::Prefix) => {
                            un_ops.push((*name, op_tp))
                        }
                    }
                },
                OperatorElement::Expression(oexpr) => {
                    let mut expr = infer_expr(env, self, oexpr);

                    while let Some((op_name, op_tp)) = un_ops.pop() {
                        let op_expr = TExpression::new(TExprData::Variable(op_name), op_tp.clone());
                        expr = function_call(self, op_expr, vec![ expr ], *oexpr.get_location());
                    }

                    exprs.push(expr);
                }
            }
        }

        if !un_ops.is_empty() {
            let op_names: Vec<&str> = un_ops.iter().map(|(a, _)| *a).collect();
            self.add_error(TypeError::remaining_unary_operators(top_location, &op_names))
        }

        while let Some((op_name, op_type, _, _, data)) = bin_ops.pop() {
            self.call_operator(&mut exprs, op_name, &op_type, data);
        }

        exprs.pop().unwrap()
    }
}

impl Context<Span> {
    fn publish_errors(&mut self, error_ctx: &mut ErrorContext) {
        let name = self.module.clone();
        self.errors.drain(..).for_each(|e| error_ctx.add_error(CompilerError::TypeError(e, name.clone())))
    }
}

//TODO: Immutable environments or something similar to allow variable shadowing?
#[derive(Debug, Clone)]
struct Environment<'a, Data: Copy + Debug> {
    values: SymbolTable<'a, Scheme, Data>,
    operators: SymbolTable<'a, (Scheme, OperatorSpecification), Data>,
    mono_vars: HashSet<GenericId>,
    named_generics: SymbolTable<'a, GenericId, Data>,
    depth: usize
}

impl<'a, Data: Copy + Debug> Environment<'a, Data> {
    fn new() -> Self {
        Environment {
            values: SymbolTable::new(),
            operators: SymbolTable::new(),
            mono_vars: HashSet::new(),
            named_generics: SymbolTable::new(),
            depth: 0
        }
    }

    fn lookup_generic(&self, name: &'a str) -> Option<GenericId> {
        self.named_generics.get(name).copied()
    }

    fn get_or_create_generic(&mut self, ctx: &mut Context<Data>, name: &'a str, location: Data) -> GenericId {
        match self.named_generics.get(name) {
            None => {
                let gid = ctx.next_id();
                self.named_generics.add(name, gid, Location::Local(location));
                gid
            },
            Some(gid) => *gid
        }
    }

    fn add_operator(&mut self, name: &'a str, sch: Scheme, op: OperatorSpecification, location: Data) {
        self.operators.add(name, (sch, op), Location::Local(location));
    }

    fn add(&mut self, name: &'a str, sch: Scheme, location: Data) {
        self.values.add(name, sch, Location::Local(location));
    }

    fn lookup(&self, name: &'a str) -> Option<&Scheme> {
        self.values.get(name)
    }

    fn lookup_operator(&self, name: &'a str) -> Option<&(Scheme, OperatorSpecification)> {
        self.operators.get(name)
    }

    fn add_monomorphic_var(&mut self, id: GenericId) {
        self.mono_vars.insert(id);
    }

    fn replace<F: Fn(GenericId) -> Option<Type>>(tp: &Type, mapper: &F) -> Type {
        match tp {
            Type::Basic(_) => tp.clone(),
            Type::Parameterized(p, ps) => Type::Parameterized(*p, ps.iter().map(|p| Self::replace(p, mapper)).collect()),
            Type::Var(id) => (mapper)(*id).unwrap_or_else(|| tp.clone()),
            _ => tp.clone()
        }
    }

    fn inst(&self, ctx: &mut Context<Data>, sch: &Scheme) -> Type {
        let mapping: HashMap<GenericId, Type> = sch.1.iter().map(|v| (*v, ctx.new_generic())).collect();
        Self::replace(&sch.2, &|id| mapping.get(&id).cloned())
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
        Scheme(HashSet::new(), scheme_vars, tp.clone())
    }

    fn import_scheme(ctx: &mut Context<Data>, tp: &Scheme) -> Scheme {
        let mapping: HashMap<GenericId, GenericId> = tp.1.iter().map(|v| (*v, ctx.next_id())).collect();
        let tp = Self::replace(&tp.2, &|gid| mapping.get(&gid).copied().map(Type::Var));
        let vars = mapping.values().copied().collect();
        Scheme(HashSet::new(), vars, tp)
    }

    fn import_module(&mut self, ctx: &mut Context<Data>, exports: &Exports<'a>) {
        exports.iter_vars().for_each(|(name, tp)| self.import(ctx, name, tp));
    }

    fn import<'b: 'a>(&mut self, ctx: &mut Context<Data>, name: &'b str, exv: &ExportValue) {
        match exv {
            ExportValue::Value(tp) => {
                self.values.add(name, Self::import_scheme(ctx, tp), Location::External)
            },
            ExportValue::Operator(op, tp) => {
                self.operators.add(name, (Self::import_scheme(ctx, tp), op.clone()), Location::External);
            }
        }
    }
}

fn function_call<'input, Data: Copy + Debug>(ctx: &mut Context<Data>, tfunc: TExpression<'input>, args: Vec<TExpression<'input>>, loc: Data) -> TExpression<'input> {
    let argtypes: Vec<Type> = args.iter().map(TExpression::typ).cloned().collect();
    let ret = ctx.new_generic();
    let mut arg_fun_type = build_function(&argtypes, &ret);
    ctx.unify(&mut arg_fun_type, tfunc.typ(), UnificationSource::TypeInference, loc);
    TExpression::new(TExprData::FunctionCall(tfunc.into(), args), ret)
}

fn infer_expr<'input, Data: Copy + Debug>(env: &mut Environment<'input, Data>, ctx: &mut Context<Data>, expr: &Expr<'input, Data>) -> TExpression<'input> {
    match expr {
        Expr::Literal(lit, _) => TExpression::new(TExprData::Literal(lit.clone()), get_literal_type(lit)),
        Expr::Variable(varname, loc) => {
            let vartype = ctx.lookup_var(env, varname, *loc);
            TExpression::new(TExprData::Variable(varname), vartype)
        },
        Expr::Lambda { params, body, data: _ } => {
            let mut body_env = env.clone();
            let mut param_types = Vec::with_capacity(params.len());
            for param in params {
                let gen = ctx.next_id();
                body_env.add_monomorphic_var(gen);
                let tp = Type::Var(gen);
                let param_type = Scheme::simple(tp.clone());
                ctx.bind_to_pattern(&mut body_env, param, &param_type);
                param_types.push(tp);
            }

            let body = infer_block(&mut body_env, ctx, body);
            let scheme = build_function(&param_types, body.res.typ());
            TExpression::new(TExprData::Lambda(params.iter().map(|p| map_bind_pattern(p)).collect(), body), scheme)
        },
        Expr::FunctionCall(func, args, loc) => {
            let tfunc = infer_expr(env, ctx, func);
            let targs = args.iter().map(|e| infer_expr(env, ctx, e)).collect();
            function_call(ctx, tfunc, targs, *loc)
        },
        Expr::OperatorCall(elements, loc) => {
            ctx.transform_operators(env, elements, *loc)
        },
        Expr::If { data: loc, cond, if_true, if_false } => {
            let tcond = infer_expr(env, ctx, &*cond);
            let mut true_env = env.clone();
            let mut false_env = env.clone();
            let ttrue = infer_block(&mut true_env, ctx, if_true);
            let tfalse = infer_block(&mut false_env, ctx, if_false);
            let mut rett = ttrue.res.typ().clone();
            ctx.unify_check(&mut rett, tcond.typ(), &Type::Basic(TypeDef::Primitive(PrimitiveType::Bool)), UnificationSource::If, *loc);
            ctx.unify(&mut rett, tfalse.res.typ(), UnificationSource::BlockReturn, *loc);
            let case = vec![
                (MatchPattern::Literal(Literal::Boolean(true), Unit::unit()), ttrue),
                (MatchPattern::Literal(Literal::Boolean(false), Unit::unit()), tfalse)
            ];
            TExpression::new(TExprData::Case(tcond.into(), case), rett)
        },
        Expr::OperatorAsFunction(name, loc) => {
            let vartype = ctx.lookup_var(env, name, *loc);
            TExpression::new(TExprData::Variable(name), vartype)
        },
        Expr::Tuple(exprs, _) => {
            let texprs: Vec<TExpression> = exprs.iter().map(|e| infer_expr(env, ctx, e)).collect();
            let types = texprs.iter().map(|t| t.typ().clone()).collect();
            let tp = Type::Parameterized(ComplexType::Tuple(texprs.len()), types);
            TExpression::new(TExprData::Tuple(texprs), tp)
        }
        _ => unimplemented!()
    }
}

fn get_literal_type(lit: &Literal) -> Type {
    Type::Basic(TypeDef::Primitive(match lit {
        Literal::Boolean(_) => PrimitiveType::Bool,
        Literal::Unit => PrimitiveType::Unit,
        Literal::String(_) => PrimitiveType::String,
        Literal::Number(_, NumberType::Float) => PrimitiveType::Float,
        Literal::Number(_, NumberType::Integer) => PrimitiveType::Int
    }))
}

fn infer_statement<'input, Data: Copy + Debug>(env: &mut Environment<'input, Data>, ctx: &mut Context<Data>, statement: &Statement<'input, Data>) -> Option<TStatement<'input>> {
    match statement {
        Statement::Region(name, _) => None,
        Statement::Do(expr, _) => Some(TStatement::Do(infer_expr(env, ctx, expr))),
        Statement::Let(mods, pattern, value, loc) => {
            let v = if mods.contains(&Modifier::Rec) {
                let mut var = ctx.new_generic();
                let mut body_env = env.clone();
                ctx.bind_to_pattern(&mut body_env, pattern, &Scheme::simple(var.clone()));
                let v = infer_expr(&mut body_env, ctx, value);
                ctx.unify(&mut var, &v.typ(), UnificationSource::TypeInference, *loc);
                v
            } else {
                let mut body_env = env.clone();
                infer_expr(&mut body_env, ctx, value)
            };
            ctx.bind_to_pattern(env, pattern, &env.generalize(&v.typ()));
            Some(TStatement::Let(v, map_bind_pattern(pattern)))
        },
        Statement::LetOperator(mods, op, name, ta, expr, loc) => {
            let (mut op_type, v) = if mods.contains(&Modifier::Rec) {
                let recursive_type = ctx.new_generic();
                let mut body_env = env.clone();
                body_env.add_operator(name, Scheme::simple(recursive_type.clone()), op.clone(), *loc);
                let value = infer_expr(&mut body_env, ctx, expr);
                let mut typ = value.typ().clone();
                ctx.unify(&mut typ, &value.typ(), UnificationSource::TypeInference, *loc);
                (typ, value)
            } else {
                let value = infer_expr(env, ctx, expr);
                (value.typ().clone(), value)
            };
            if let Some(ta) = ta {
                let tp = ctx.type_from_annotation(env, ta);
                ctx.unify(&mut op_type, &tp, UnificationSource::TypeAnnotation, *loc);
            }
            match op {
                OperatorSpecification::Infix(_, _) => {
                    let expected_type = build_function(&[ ctx.new_generic(), ctx.new_generic() ], &ctx.new_generic());
                    ctx.unify(&mut op_type, &expected_type, UnificationSource::OperatorConstraint(*op), *loc);
                },
                OperatorSpecification::Prefix => {
                    let expected_type = build_function(&[ ctx.new_generic() ], &ctx.new_generic());
                    ctx.unify(&mut op_type, &expected_type, UnificationSource::OperatorConstraint(*op), *loc);
                }
            }
            env.add_operator(name, env.generalize(&op_type), op.clone(), *loc);
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

fn infer_block<'input, Data: Copy + Debug>(env: &mut Environment<'input, Data>, ctx: &mut Context<Data>, block: &Block<'input, Data>) -> TBlock<'input> {
    let mut block_env = env.clone();
    let tstatements = block.statements.iter().filter_map(|s| infer_statement(&mut block_env, ctx, s)).collect();
    let result = infer_expr(&mut block_env, ctx, &block.result);
    TBlock {
        statements: tstatements,
        res: Box::new(result)
    }
}

fn process_top_level<'input, Data: Copy + Debug>(env: &mut Environment<'input, Data>, ctx: &mut Context<Data>, tls: &TopLevelStatement<'input, Data>) {

}

fn infer_top_level_block<'input, Data: Copy + Debug>(env: &mut Environment<'input, Data>, ctx: &mut Context<Data>,tlb: &TopLevelBlock<'input, Data>) -> TBlock<'input> {
    tlb.top_levels.iter().for_each(|st| process_top_level(env, ctx, st));
    let statements = tlb.block.statements.iter().filter_map(|st| infer_statement(env, ctx, st)).collect();
    let res = infer_expr(env, ctx, &*tlb.block.result);
    TBlock {
        statements,
        res: res.into()
    }
}

fn typecheck_module<'input>(unchecked: Module<UntypedModuleData<'input>>, deps: Vec<&Module<TypedModuleData<'input>>>, error_context: &mut ErrorContext<'input>) -> Module<TypedModuleData<'input>> {
    //TODO: Import dependencies into context
    let mut context = Context::new(unchecked.name.clone());
    let mut env = Environment::new();
    let prelude = prelude();
    env.import_module(&mut context, &prelude);

    let tast = infer_top_level_block(&mut env, &mut context, &unchecked.data.0);
    let rettype = substitute(&context.type_subst, tast.res.typ());
    context.publish_errors(error_context);
    println!("Return type of program: {}", rettype);
    Module {
        data: TypedModuleData(context.type_subst, tast),
        name: unchecked.name,
        path: unchecked.path,
        depth: unchecked.depth,
        deps: unchecked.deps
    }
}

pub struct TypedContext<'input> {
    pub modules: HashMap<ModuleIdentifier, Module<TypedModuleData<'input>>>,
}

pub fn typecheck<'input>(errors: &mut ErrorContext<'input>, mut module_ctx: ModuleTreeContext<'input>) -> Result<TypedContext<'input>, ()> {
    let mut ctx = TypedContext {
        modules: HashMap::new()
    };
    let mut ordered_modules: Vec<Module<UntypedModuleData<'input>>> = module_ctx.modules.drain().map(|(_, module)| module).collect();
    ordered_modules.sort_by(|m1, m2| m1.depth.cmp(&m2.depth));

    for module in ordered_modules {
        let deps = module.deps.iter().map(|(d, _)| ctx.modules.get(d).expect("Fatal error: Dependent module not found")).collect();
        let name = module.name.clone();
        let typed_mod = typecheck_module(module, deps, errors);
        ctx.modules.insert(name, typed_mod);
    }

    errors.handle_errors().map(|()| ctx)
}

#[cfg(test)]
mod tests {
    use crate::typechecker::{Environment, Context};
    use crate::prelude::prelude;
    use crate::ast::OperatorElement::*;
    use crate::ast::{Expr, OperatorElement};
    use crate::ast_common::{Literal, NumberType};
    use crate::typed_ast::{TExpression, TExprData};
    use crate::types::{Type, int};
    use crate::errors::ErrorContext;
    use crate::common::ModuleIdentifier;
    use std::fmt::Debug;

    fn int_lit(lit: &str) -> OperatorElement<()> {
        Expression(Expr::Literal(Literal::Number(lit, NumberType::Integer), ()))
    }

    fn int_lit_typed(lit: &str) -> TExpression {
        TExpression::new(TExprData::Literal(Literal::Number(lit, NumberType::Integer)), int())
    }

    fn lookup<Data: Copy + Debug>(env: &Environment<Data>, ctx: &mut Context<Data>, name: &str) -> Type {
        let sch = env.lookup(name).unwrap();
        env.inst(ctx, sch)
    }

    fn lookup_operator<Data: Copy + Debug>(env: &Environment<Data>, ctx: &mut Context<Data>, name: &str) -> Type {
        let (sch, _) = env.lookup_operator(name).unwrap();
        env.inst(ctx, sch)
    }

    fn add_op<'a, Data: Copy + Debug>(env: &Environment<'a, Data>, ctx: &mut Context<Data>) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("+"), lookup_operator(env, ctx, "+"))
    }

    fn mul_op<'a, Data: Copy + Debug>(env: &Environment<'a, Data>, ctx: &mut Context<Data>) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("*"), lookup_operator(env, ctx, "*"))
    }

    fn neg_op<'a, Data: Copy + Debug>(env: &Environment<'a, Data>, ctx: &mut Context<Data>) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("~"), lookup_operator(env, ctx, "~"))
    }

    fn neg_expr<'a, Data: Copy + Debug>(env: &Environment<'a, Data>, ctx: &mut Context<Data>, expr: TExpression<'a>) -> TExpression<'a> {
        TExpression::new(TExprData::FunctionCall(neg_op(env, ctx).into(), vec![
            expr
        ]), int())
    }

    #[test]
    fn op_transform_simple_binary() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
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
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_binary_precedence() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
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
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_unary_simple() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = Environment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            Operator("~", ()),
            Operator("~", ()),
            int_lit("1")
        ];
        let e1 = neg_expr(&env, &mut ctx, int_lit_typed("1"));
        let exprs = neg_expr(&env, &mut ctx, e1);
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_unary_binary() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
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
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_complex() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
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
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_bin_assoc() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
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
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }
}