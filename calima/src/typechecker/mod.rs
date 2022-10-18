use crate::ast::*;
use crate::common::ModuleIdentifier;
use crate::errors::{CompilerError, ErrorContext, MainFunctionErrorKind};
use crate::formatting::format_iter;
use crate::ir;
use crate::ir::{BindTarget, Binding, Constant, Module, Val, VarRef};
use crate::modules::{
    TypedModule, TypedModuleData, TypedModuleTree, UntypedModule, UntypedModuleTree,
};
use crate::parsing::token::Span;
use crate::symbol_names::{IText, StringInterner};
use crate::typechecker::environment::{ClosedEnvironment, ScopeEnvironment};
use crate::typechecker::ir_lowering::*;
use crate::typechecker::prelude::prelude;
use crate::typechecker::substitution::{substitute, substitute_scheme, Substitution};
use crate::types::*;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::rc::Rc;

pub mod environment;
mod ir_lowering;
mod prelude;
pub mod substitution;

//TODO: Additional information
#[derive(Debug, Clone)]
pub struct TypeError<Data> {
    pub location: Data,
    pub message: String,
}

impl<Data: Copy + Debug> TypeError<Data> {
    fn type_not_found(typename: &IText, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Type {} not defined or imported", typename),
        }
    }

    fn var_not_found(varname: &IText, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Variable {} not defined or imported", varname),
        }
    }

    fn operator_not_found(opname: &IText, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Operator {} is not defined or imported", opname),
        }
    }

    pub fn unification(
        ctx: &Context<Data>,
        ue: UnificationError,
        expected: &Type,
        actual: &Type,
        location: Data,
    ) -> Self {
        let expected = substitute(&ctx.type_subst, expected);
        let actual = substitute(&ctx.type_subst, actual);
        let message = match ue {
            UnificationError::RecursiveType => String::from("Recursive type detected"),
            UnificationError::UnificationError => format!(
                "Cannot unify types: Expected {}, but got {}",
                expected, actual
            ),
            UnificationError::Propagation => String::from("Propagated type error"),
        };
        TypeError { location, message }
    }

    fn repeated_unassociative_operators(location: Data, ops: &[&IText]) -> Self {
        TypeError {
            location,
            message: format!(
                "Repeated operators without associativity used: {}",
                format_iter(ops.iter(), ", ")
            ),
        }
    }

    fn remaining_unary_operators(location: Data, ops: &[&IText]) -> Self {
        TypeError {
            location,
            message: format!(
                "Unary prefix operators {} were used without an expression to apply them to",
                format_iter(ops.iter(), ", ")
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnificationError {
    UnificationError,
    RecursiveType,
    Propagation,
}

#[derive(Clone, Eq, PartialEq)]
pub enum Opening {
    All,
    Identifiers(Vec<IText>),
}

impl Opening {
    fn contains(&self, name: &IText) -> bool {
        match self {
            Self::All => true,
            Self::Identifiers(idents) => idents.contains(&name),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnificationSource {
    TypeAnnotation,
    TypeInference,
    FunctionCall,
    OperatorConstraint(OperatorSpecification),
    If,
    BlockReturn,
    PatternMatching,
}

pub struct ValueTypeContext {
    name_hints: HashMap<VarRef, IText>,
    types: HashMap<VarRef, Scheme>, //TODO: Use symbol table once generic enough
}

impl ValueTypeContext {
    fn new() -> Self {
        Self {
            name_hints: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn get_type(&self, v: &Val) -> Option<Scheme> {
        match v {
            Val::Var(vr) => self.types.get(vr).cloned(),
            Val::Constant(Constant::Unit) => Some(Scheme::simple(Type::Basic(TypeDef::Primitive(
                PrimitiveType::Unit,
            )))),
            Val::Constant(Constant::String(_)) => Some(Scheme::simple(Type::Basic(
                TypeDef::Primitive(PrimitiveType::String),
            ))),
            Val::Constant(Constant::Number(_, NumberType::Float)) => Some(Scheme::simple(
                Type::Basic(TypeDef::Primitive(PrimitiveType::Float)),
            )),
            Val::Constant(Constant::Number(_, NumberType::Integer)) => Some(Scheme::simple(
                Type::Basic(TypeDef::Primitive(PrimitiveType::Int)),
            )),
            Val::Constant(Constant::Boolean(_)) => Some(Scheme::simple(Type::Basic(
                TypeDef::Primitive(PrimitiveType::Bool),
            ))),
        }
    }

    pub fn get_name_hint(&self, vr: &VarRef) -> Option<&IText> {
        self.name_hints.get(vr)
    }

    fn fully_substituted(&self, subst: &Substitution<Type>) -> Self {
        Self {
            name_hints: self.name_hints.clone(),
            types: self
                .types
                .iter()
                .map(|(v, sch)| (*v, substitute_scheme(subst, sch)))
                .collect(),
        }
    }
}

pub struct Context<Data: Copy + Debug> {
    generic_id: usize,
    type_subst: Substitution<Type>,
    errors: Vec<TypeError<Data>>,
    module: ModuleIdentifier,
    var_id: usize,
    vtc: ValueTypeContext,
}

impl<Data: Copy + Debug> Context<Data> {
    pub fn new(module: ModuleIdentifier) -> Self {
        Context {
            generic_id: 0,
            type_subst: Substitution::new(),
            errors: Vec::new(),
            module,
            var_id: 0,
            vtc: ValueTypeContext::new(),
        }
    }

    pub fn get_type(&self, v: &Val) -> Option<Scheme> {
        self.vtc.get_type(v)
    }

    pub fn new_var(&mut self, sch: Scheme, name_hint: Option<IText>) -> VarRef {
        let v = VarRef(self.var_id);
        self.var_id += 1;

        if let Some(nh) = name_hint {
            self.vtc.name_hints.insert(v.clone(), nh);
        }
        self.vtc.types.insert(v, sch);
        v
    }

    pub fn get_name_hint(&self, vr: &VarRef) -> Option<&IText> {
        self.vtc.get_name_hint(vr)
    }

    pub fn add_operator(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        name: IText,
        sch: Scheme,
        op_spec: OperatorSpecification,
    ) -> VarRef {
        let v = self.new_var(sch, Some(name.clone()));
        env.bind_operator(name, Val::Var(v), op_spec);
        v
    }

    pub fn add(&mut self, env: &mut ScopeEnvironment<Data>, name: IText, sch: Scheme) -> VarRef {
        let v = self.new_var(sch, Some(name.clone()));
        env.bind(name, Val::Var(v));
        v
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

    fn import_prelude(&mut self, env: &mut ScopeEnvironment<Data>, interner: &StringInterner) {
        prelude(self, env, interner);
    }

    fn bind(&mut self, gid: GenericId, t2: &Type) -> Result<(), UnificationError> {
        let existing = self.type_subst[gid.0].clone();
        match existing {
            Some(t) => self.unify_rec(&t, t2),
            None => Ok(self.type_subst.add(gid.0, t2.clone())),
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
            Type::Reference(t) => self.check_occurs(gid, &*t),
            Type::Error => panic!("Cannot occurs_check on erroneous type"),
        }
    }

    fn unify_rec(&mut self, t1: &Type, t2: &Type) -> Result<(), UnificationError> {
        match (t1, t2) {
            (Type::Error, _) => Err(UnificationError::Propagation),
            (_, Type::Error) => Err(UnificationError::Propagation),
            (_, _) if t1 == t2 => Ok(()),
            (Type::Basic(td1), Type::Basic(td2)) if td1 != td2 => {
                Err(UnificationError::UnificationError)
            }
            (Type::Var(gid), _) => {
                self.check_occurs(*gid, t2)
                    .map_err(|()| UnificationError::RecursiveType)?;
                self.bind(*gid, t2)
            }
            (_, Type::Var(gid)) => self.bind(*gid, t1),
            (Type::Parameterized(p1, params1), Type::Parameterized(p2, params2)) => {
                if p1 != p2 || params1.len() != params2.len() {
                    Err(UnificationError::UnificationError)
                } else {
                    params1
                        .iter()
                        .zip(params2.iter())
                        .try_for_each(|(p1, p2)| self.unify_rec(p1, p2))
                }
            }
            _ => unimplemented!(),
        }
    }

    fn unify(&mut self, target: &mut Type, with: &Type, source: UnificationSource, loc: Data) {
        if let Err(e) = self.unify_rec(target, with) {
            self.add_error(TypeError::unification(self, e, &with, &target, loc));
            *target = Type::Error;
        }
    }

    fn unify_check(
        &mut self,
        target: &mut Type,
        unify_target: &Type,
        with: &Type,
        source: UnificationSource,
        loc: Data,
    ) {
        if let Err(e) = self.unify_rec(unify_target, with) {
            self.add_error(TypeError::unification(self, e, &with, &unify_target, loc));
            *target = Type::Error;
        }
    }

    fn add_error(&mut self, te: TypeError<Data>) {
        self.errors.push(te);
    }

    fn type_from_annotation(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        ta: &TypeAnnotation<Name<Data>, IText, Data>,
    ) -> Type {
        fn to_type<Data: Copy + Debug>(
            ctx: &mut Context<Data>,
            env: &mut ScopeEnvironment<Data>,
            ta: &TypeAnnotation<Name<Data>, IText, Data>,
        ) -> Result<Type, TypeError<Data>> {
            match ta {
                TypeAnnotation::Name(name) => match PrimitiveType::try_from(&name.0[0]) {
                    Ok(pt) => Ok(Type::Basic(TypeDef::Primitive(pt))),
                    Err(()) => Err(TypeError::type_not_found(&name.0[0], name.1)),
                },
                TypeAnnotation::Function(ta1, ta2) => to_type(ctx, env, &*ta1)
                    .and_then(|t1| to_type(ctx, env, &*ta2).map(|t2| (t1, t2)))
                    .map(|(t1, t2)| Type::Parameterized(ComplexType::Function, vec![t1, t2])),
                TypeAnnotation::Generic(gname) => {
                    Ok(Type::Var(env.get_or_create_generic(ctx, &gname.0, gname.1)))
                }
                TypeAnnotation::Tuple(params) => params
                    .iter()
                    .map(|p| to_type(ctx, env, p))
                    .collect::<Result<Vec<Type>, TypeError<Data>>>()
                    .map(|ps| Type::Parameterized(ComplexType::Tuple(ps.len()), ps)),
                _ => unimplemented!(),
            }
        }

        to_type(self, env, ta).unwrap_or_else(|e| {
            self.add_error(e);
            Type::Error
        })
    }

    fn inst(&mut self, sch: &Scheme) -> Type {
        let mapping: HashMap<GenericId, Type> =
            sch.0.iter().map(|v| (*v, self.new_generic())).collect();
        replace(&sch.1, &|id| mapping.get(&id).cloned())
    }

    fn bind_to_pattern_parameter(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        pattern: &BindPattern<IText, TypeAnnotation<Name<Data>, IText, Data>, Data>,
        tp: &mut Type,
        vals: &mut Vec<ir::BindTarget>,
    ) {
        match pattern {
            BindPattern::Any(_) => vals.push(BindTarget::Discard),
            BindPattern::Name(name, ta, _) => {
                // TODO: Check type annotation
                let v = self.add(env, name.clone(), Scheme::simple(tp.clone()));
                vals.push(BindTarget::Var(v));
            }
            BindPattern::UnitLiteral(data) => {
                self.unify(tp, &unit(), UnificationSource::PatternMatching, *data);
                vals.push(BindTarget::Discard);
            }
            _ => todo!(),
        }
    }

    fn bind_to_pattern_types(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        pattern: &BindPattern<IText, TypeAnnotation<Name<Data>, IText, Data>, Data>,
        tp: &mut Type,
    ) {
        match pattern {
            BindPattern::Any(_) => (),
            BindPattern::Name(name, ta, _) => {
                self.add(env, name.clone(), Scheme::simple(tp.clone()));
            }
            _ => todo!(),
        }
    }

    fn bind_to_pattern_generalized(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        pattern: &BindPattern<IText, TypeAnnotation<Name<Data>, IText, Data>, Data>,
        block_builder: &mut BlockBuilder,
        bind_val: Val,
        tp: Type,
    ) {
        match pattern {
            BindPattern::Any(_) => (),
            BindPattern::Name(name, ta, _) => {
                let generalized = env.generalize(&tp);
                let gen_var = self.add(env, name.clone(), generalized);
                block_builder.add_binding(Binding(
                    BindTarget::Var(gen_var),
                    ir::Expr::Generalize(bind_val),
                ))
            }
            _ => todo!(),
        }
    }
}

impl Context<Span> {
    fn publish_errors(&mut self, error_ctx: &mut ErrorContext) {
        let name = self.module.clone();
        self.errors
            .drain(..)
            .for_each(|e| error_ctx.add_error(CompilerError::TypeError(e, name.clone())))
    }
}

fn call_operator<Data: Copy + Debug>(
    ctx: &mut Context<Data>,
    env: &ScopeEnvironment<Data>,
    block_builder: &mut BlockBuilder,
    vals: &mut Vec<ir::Val>,
    last_name: &IText,
    last_type: &Type,
    loc: Data,
) {
    //TODO: Error handling
    let r = vals.pop().unwrap();
    let l = vals.pop().unwrap();
    let r_tp = ctx.inst(&ctx.get_type(&r).unwrap());
    let l_tp = ctx.inst(&ctx.get_type(&l).unwrap());
    let (op_val, _op_spec) = env.lookup_operator(last_name).unwrap();
    let op_tp = ctx.inst(&ctx.get_type(&op_val).unwrap());
    let (op_expr, res_tp) = function_call(ctx, op_val, &op_tp, vec![(l, l_tp), (r, r_tp)], loc);

    let res_var = ctx.new_var(Scheme::simple(res_tp), None);
    block_builder.add_binding(Binding(BindTarget::Var(res_var), op_expr));
    vals.push(Val::Var(res_var));
}

fn transform_operators<Data: Copy + Debug>(
    ctx: &mut Context<Data>,
    env: &mut ScopeEnvironment<Data>,
    block_builder: &mut BlockBuilder,
    elements: &Vec<OperatorElement<Name<Data>, IText, Data>>,
    top_location: Data,
) -> (ir::Val, Type) {
    let mut bin_ops: Vec<(IText, Type, u32, Associativity, Data)> = Vec::new();
    let mut un_ops: Vec<(IText, Type)> = Vec::new();
    let mut vals: Vec<Val> = Vec::new();

    for el in elements {
        match el {
            OperatorElement::Operator(name, data) => {
                let data = *data;
                let (op_val, op_spec) = env.lookup_operator(name).unwrap();
                let op_tp = ctx.inst(&ctx.get_type(&op_val).unwrap());
                match op_spec {
                    OperatorSpecification::Infix(op_prec, assoc) => match bin_ops.last() {
                        None => bin_ops.push((name.clone(), op_tp, op_prec, assoc, data)),
                        Some((last_op, _, last_prec, last_assoc, data)) => {
                            let data = *data;
                            if last_prec == &op_prec {
                                if assoc == Associativity::None
                                    || last_assoc == &Associativity::None
                                {
                                    ctx.add_error(TypeError::repeated_unassociative_operators(
                                        top_location,
                                        &[last_op, name],
                                    ));
                                } else if assoc == Associativity::Left {
                                    let (last_name, last_type, _, _, data) = bin_ops.pop().unwrap();
                                    call_operator(
                                        ctx,
                                        env,
                                        block_builder,
                                        &mut vals,
                                        &last_name,
                                        &last_type,
                                        data,
                                    );
                                }
                            } else if last_prec > &op_prec {
                                let (last_name, last_type, _, _, data) = bin_ops.pop().unwrap();
                                call_operator(
                                    ctx,
                                    env,
                                    block_builder,
                                    &mut vals,
                                    &last_name,
                                    &last_type,
                                    data,
                                )
                            }
                            bin_ops.push((name.clone(), op_tp, op_prec, assoc, data))
                        }
                    },
                    OperatorSpecification::Prefix => un_ops.push((name.clone(), op_tp)),
                }
            }
            OperatorElement::Expression(oexpr, loc) => {
                let mut last_val = infer_expr(env, ctx, block_builder, oexpr);

                while let Some((op_name, op_tp)) = un_ops.pop() {
                    let (op_val, _) = env.lookup_operator(&op_name).unwrap();
                    let (fc, tp) = function_call(ctx, op_val, &op_tp, vec![last_val], *loc);
                    let fcv = ctx.new_var(Scheme::simple(tp.clone()), None);
                    block_builder.add_binding(Binding(BindTarget::Var(fcv), fc));
                    last_val = (Val::Var(fcv), tp)
                }

                vals.push(last_val.0);
            }
        }
    }

    if !un_ops.is_empty() {
        let op_names: Vec<&IText> = un_ops.iter().map(|(a, _)| a).collect();
        ctx.add_error(TypeError::remaining_unary_operators(
            top_location,
            &op_names,
        ))
    }

    while let Some((op_name, op_type, _, _, data)) = bin_ops.pop() {
        call_operator(ctx, env, block_builder, &mut vals, &op_name, &op_type, data);
    }

    let res = vals.pop().unwrap();
    let res_tp = ctx.inst(&ctx.get_type(&res).unwrap());
    (res, res_tp)
}

fn replace<F: Fn(GenericId) -> Option<Type>>(tp: &Type, mapper: &F) -> Type {
    match tp {
        Type::Basic(_) => tp.clone(),
        Type::Parameterized(p, ps) => {
            Type::Parameterized(*p, ps.iter().map(|p| replace(p, mapper)).collect())
        }
        Type::Var(id) => (mapper)(*id).unwrap_or_else(|| tp.clone()),
        _ => tp.clone(),
    }
}

fn function_call<Data: Copy + Debug>(
    ctx: &mut Context<Data>,
    func_val: Val,
    func_tp: &Type,
    args: Vec<(Val, Type)>,
    loc: Data,
) -> (ir::Expr, Type) {
    let argtypes: Vec<Type> = args.iter().map(|(_, b)| b.clone()).collect();
    let ret = ctx.new_generic();
    let mut arg_fun_type = build_function(&argtypes, &ret);
    ctx.unify(
        &mut arg_fun_type,
        func_tp,
        UnificationSource::FunctionCall,
        loc,
    );
    let fc = ir::Expr::FunctionCall {
        func: func_val,
        args: args.iter().map(|(a, b)| a.clone()).collect(),
    };
    (fc, ret)
}

fn infer_expr<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut Context<Data>,
    block_builder: &mut BlockBuilder,
    expr: &Expr<Name<Data>, IText, Data>,
) -> (ir::Val, Type) {
    match expr {
        Expr::Literal(lit, _) => {
            let tp = get_literal_type(lit);
            let c = match lit {
                Literal::String(text) => Constant::String(text.clone()),
                Literal::Unit => Constant::Unit,
                Literal::Number(nr, nt) => Constant::Number(nr.clone(), *nt),
                Literal::Boolean(b) => Constant::Boolean(*b),
            };
            (Val::Constant(c), tp)
        }
        Expr::Variable(varname) => {
            //TODO: Error handling
            let val = env
                .lookup_value(&varname.0[0])
                .expect(&format!("Variable not found: {}", varname.to_string())); //TODO: Lookup into data structures
            let vartp = ctx.get_type(val).unwrap();
            (val.clone(), ctx.inst(&vartp))
        }
        Expr::Lambda {
            params,
            body,
            data: _,
        } => {
            let mut body_env = env.clone();
            let mut param_vals = Vec::new();
            let mut param_types = Vec::new();
            for param in params {
                let gen = ctx.next_id();
                body_env.add_monomorphic_var(gen);
                let mut tp = Type::Var(gen);
                ctx.bind_to_pattern_parameter(&mut body_env, param, &mut tp, &mut param_vals);
                param_types.push(tp);
            }

            let (block, ret_tp) = infer_block(&mut body_env, ctx, block_builder, body);
            let lambda = ir::Expr::Lambda {
                params: param_vals,
                block,
            };
            let func_tp = build_function(&param_types, &ret_tp);
            let func_sch = Scheme::simple(func_tp.clone());
            let lambda_var = ctx.new_var(func_sch, None);
            block_builder.add_binding(Binding(BindTarget::Var(lambda_var), lambda));
            (Val::Var(lambda_var), func_tp)
        }
        Expr::FunctionCall(func, args, loc) => {
            let targs = args
                .iter()
                .map(|e| infer_expr(env, ctx, block_builder, e))
                .collect();
            let (tfunc, functp) = infer_expr(env, ctx, block_builder, func);
            let (func_call, ret_tp) = function_call(ctx, tfunc, &functp, targs, *loc);
            let funcc_var = ctx.new_var(Scheme::simple(ret_tp.clone()), None);
            block_builder.add_binding(Binding(BindTarget::Var(funcc_var), func_call));
            (Val::Var(funcc_var), ret_tp)
        }
        Expr::OperatorCall(elements, loc) => {
            transform_operators(ctx, env, block_builder, elements, *loc)
        }
        Expr::If {
            data: loc,
            cond,
            if_true,
            if_false,
        } => {
            let (tcond, cond_tp) = infer_expr(env, ctx, block_builder, &*cond);
            let mut true_env = env.clone();
            let mut false_env = env.clone();
            let (ttrue, true_tp) = infer_block(&mut true_env, ctx, block_builder, if_true);
            let (tfalse, false_tp) = infer_block(&mut false_env, ctx, block_builder, if_false);
            let mut rett = true_tp.clone();
            ctx.unify_check(
                &mut rett,
                &cond_tp,
                &Type::Basic(TypeDef::Primitive(PrimitiveType::Bool)),
                UnificationSource::If,
                *loc,
            );
            ctx.unify(&mut rett, &false_tp, UnificationSource::BlockReturn, *loc);
            let if_expr = ir::Expr::If {
                condition: tcond,
                if_true: ttrue,
                if_false: tfalse,
            };
            let res_var = ctx.new_var(Scheme::simple(rett.clone()), None);
            block_builder.add_binding(Binding(BindTarget::Var(res_var), if_expr));
            (Val::Var(res_var), rett)
        }
        Expr::OperatorAsFunction(name, loc) => {
            let val = env.lookup_value(name).expect("Variable not found"); //TODO: Lookup into data structures
            let vartp = ctx.get_type(val).unwrap();
            (val.clone(), ctx.inst(&vartp))
        }
        Expr::Tuple(exprs, _) => {
            /*let texprs = exprs
                .iter()
                .map(|e| infer_expr(env, ctx, block_builder, e))
                .collect();
            let types = texprs.iter().map(|t| t.typ().clone()).collect();
            let tp = Type::Parameterized(ComplexType::Tuple(texprs.len()), types);*/
            todo!()
        }
        _ => unimplemented!(),
    }
}

fn get_literal_type(lit: &Literal) -> Type {
    Type::Basic(TypeDef::Primitive(match lit {
        Literal::Boolean(_) => PrimitiveType::Bool,
        Literal::Unit => PrimitiveType::Unit,
        Literal::String(_) => PrimitiveType::String,
        Literal::Number(_, NumberType::Float) => PrimitiveType::Float,
        Literal::Number(_, NumberType::Integer) => PrimitiveType::Int,
    }))
}

fn infer_let<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut Context<Data>,
    block_builder: &mut BlockBuilder,
    l: &Let<Name<Data>, IText, Data>,
    export: bool,
) {
    let (lval, tp) = if l.mods.contains(&Modifier::Rec) {
        let mut rec_tp = ctx.new_generic();
        let mut body_env = env.clone();
        ctx.bind_to_pattern_types(&mut body_env, &l.pattern, &mut rec_tp);
        let (val, tp) = infer_expr(&mut body_env, ctx, block_builder, &l.value);
        ctx.unify(&mut rec_tp, &tp, UnificationSource::TypeInference, l.data);
        (val, tp)
    } else {
        let mut body_env = env.clone();
        infer_expr(&mut body_env, ctx, block_builder, &l.value)
    };
    ctx.bind_to_pattern_generalized(env, &l.pattern, block_builder, lval, tp);
}

fn infer_let_operator<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut Context<Data>,
    block_builder: &mut BlockBuilder,
    l: &LetOperator<Name<Data>, IText, Data>,
    export: bool,
) {
    let (lval, mut op_type) = if l.mods.contains(&Modifier::Rec) {
        let mut rec_tp = ctx.new_generic();
        let mut body_env = env.clone();
        ctx.add_operator(
            &mut body_env,
            l.name.clone(),
            Scheme::simple(rec_tp.clone()),
            l.op,
        );
        let (val, tp) = infer_expr(&mut body_env, ctx, block_builder, &l.value);
        ctx.unify(&mut rec_tp, &tp, UnificationSource::TypeInference, l.data);
        (val, tp)
    } else {
        let mut body_env = env.clone();
        infer_expr(&mut body_env, ctx, block_builder, &l.value)
    };

    match l.op {
        OperatorSpecification::Infix(_, _) => {
            let expected_type =
                build_function(&[ctx.new_generic(), ctx.new_generic()], &ctx.new_generic());
            ctx.unify(
                &mut op_type,
                &expected_type,
                UnificationSource::OperatorConstraint(l.op),
                l.data,
            );
        }
        OperatorSpecification::Prefix => {
            let expected_type = build_function(&[ctx.new_generic()], &ctx.new_generic());
            ctx.unify(
                &mut op_type,
                &expected_type,
                UnificationSource::OperatorConstraint(l.op),
                l.data,
            );
        }
    }
    let scheme = env.generalize(&op_type);
    let op_var = ctx.add_operator(env, l.name.clone(), scheme, l.op);
    block_builder.add_binding(Binding(BindTarget::Var(op_var), ir::Expr::Generalize(lval)))
}

fn infer_statement<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut Context<Data>,
    block_builder: &mut BlockBuilder,
    statement: &Statement<Name<Data>, IText, Data>,
) {
    match statement {
        Statement::Do(expr, _) => {
            todo!()
        }
        Statement::Let(l) => infer_let(env, ctx, block_builder, l, false),
        Statement::LetOperator(l) => infer_let_operator(env, ctx, block_builder, l, false),
    }
}

fn infer_block<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut Context<Data>,
    block_builder: &mut BlockBuilder,
    block: &Block<Name<Data>, IText, Data>,
) -> (ir::Block, Type) {
    block_builder.begin();
    let mut block_env = env.clone();

    block
        .statements
        .iter()
        .for_each(|s| infer_statement(&mut block_env, ctx, block_builder, s));

    let (result_val, result_tp) = infer_expr(&mut block_env, ctx, block_builder, &block.result);
    (block_builder.end(result_val), result_tp)
}

fn infer_top_level<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut Context<Data>,
    block_builder: &mut BlockBuilder,
    tls: &TopLevelStatement<Name<Data>, IText, Data>,
) {
    match tls {
        TopLevelStatement::Let(vis, l) => {
            infer_let(env, ctx, block_builder, l, vis == &Some(Visibility::Public))
        }
        TopLevelStatement::LetOperator(vis, l) => {
            infer_let_operator(env, ctx, block_builder, l, vis == &Some(Visibility::Public))
        }
        TopLevelStatement::Import { .. } => (), //Imports are resolved before typechecking
        TopLevelStatement::Type { .. } => unimplemented!(),
    }
}

fn infer_top_level_block<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut Context<Data>,
    block_builder: &mut BlockBuilder,
    tlb: &TopLevelBlock<Name<Data>, IText, Data>,
) -> ir::Block {
    block_builder.begin();
    for st in &tlb.0 {
        infer_top_level(env, ctx, block_builder, st);
    }
    block_builder.end(Val::Constant(Constant::Unit))
}

fn typecheck_module(
    unchecked: &UntypedModule,
    deps: Vec<TypedModule>,
    error_context: &mut ErrorContext,
    interner: &StringInterner,
) -> Result<TypedModule, ()> {
    let mut context = Context::new(unchecked.0.name.clone());
    let mut block_builder = BlockBuilder::new();
    let mut env = ScopeEnvironment::new();
    context.import_prelude(&mut env, interner);

    /*for dep in &deps {
        //TODO
        env.import(&dep.0.name, &dep.0.env, Opening::All, Location::External)
    }*/

    let ir_block =
        infer_top_level_block(&mut env, &mut context, &mut block_builder, &unchecked.0.ast);

    let ir_module = Module {
        externs: vec![],
        main_block: ir_block,
        export: vec![],
    };

    context.publish_errors(error_context);

    let vtc = context.vtc.fully_substituted(&context.type_subst);

    error_context.handle_errors().map(|_| {
        let mod_data = TypedModuleData {
            name: unchecked.0.name.clone(),
            path: unchecked.0.path.clone(),
            deps,
            ir_module,
            subst: context.type_subst,
            vtc,
            env: ClosedEnvironment::new(env),
        };
        TypedModule(Rc::new(mod_data))
    })
}

fn typecheck_tree(
    tree: &mut HashMap<ModuleIdentifier, TypedModule>,
    untyped: &UntypedModule,
    err: &mut ErrorContext,
    interner: &StringInterner,
) -> Result<TypedModule, ()> {
    let dependencies: Result<Vec<TypedModule>, ()> = untyped
        .0
        .dependencies
        .iter()
        .map(|ud| match tree.get(&ud.0.name) {
            Some(d) => Ok(d.clone()),
            None => typecheck_tree(tree, ud, err, interner),
        })
        .collect();
    dependencies.and_then(|d| typecheck_module(untyped, d, err, interner))
}

fn verify_main_module(
    main_mod: &TypedModule,
    errors: &mut ErrorContext,
    interner: &StringInterner,
) {
    let main_name = interner.intern_str("main");
    let main_md = &main_mod.0;
    let main_val = main_md.env.lookup_value(&main_name);
    let main_type = main_val.and_then(|v| main_md.vtc.get_type(v));

    if let Some(main_sch) = main_type {
        let mut ctx: Context<Span> = Context::new(main_mod.0.name.clone());
        if let Err(_e) = ctx.unify_rec(&main_sch.1, &build_function(&[unit()], &unit())) {
            errors.add_error(CompilerError::MainFunctionError(
                main_mod.0.name.clone(),
                MainFunctionErrorKind::SignatureWrong,
            ))
        }
    } else {
        errors.add_error(CompilerError::MainFunctionError(
            main_mod.0.name.clone(),
            MainFunctionErrorKind::Missing,
        ))
    }
}

pub fn typecheck(
    errors: &mut ErrorContext,
    module_ctx: UntypedModuleTree,
    interner: &StringInterner,
) -> Result<TypedModuleTree, ()> {
    let mut lookup = HashMap::new();
    let main_mod = typecheck_tree(&mut lookup, &module_ctx.main_module, errors, interner)?;
    verify_main_module(&main_mod, errors, interner);
    errors.handle_errors().map(|_| TypedModuleTree {
        main_module: main_mod,
        lookup,
    })
}

#[cfg(test)]
mod ir_gen_tests {
    use crate::common::ModuleIdentifier;
    use crate::formatting::context::format_to_string;
    use crate::ir::FormattingContext;
    use crate::modules::{UntypedModule, UntypedModuleData};
    use crate::parsing::parser::parse;
    use crate::typechecker::typecheck_module;
    use crate::{ErrorContext, StringInterner};
    use goldenfile::Mint;
    use std::fs::read_dir;
    use std::io::Write;
    use std::rc::Rc;

    #[test]
    fn test_ir_generation() {
        let mut mint_code = Mint::new("tests/ir_programs/");

        for entry in read_dir("tests/typechecking/").unwrap() {
            match entry {
                Ok(entry) => {
                    let interner = StringInterner::new();
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        let filename = entry_path.file_name().unwrap();
                        let mut parsed_file = mint_code.new_goldenfile(filename).unwrap();
                        let file_content = std::fs::read_to_string(&entry_path).unwrap();
                        let parsed = parse(&file_content, &interner).expect(
                            format!("Error parsing {}", filename.to_string_lossy()).as_ref(),
                        );
                        let module: UntypedModule = UntypedModule(Rc::new(UntypedModuleData {
                            name: ModuleIdentifier::from_filename(
                                filename.to_string_lossy().to_string(),
                            ),
                            ast: parsed,
                            dependencies: vec![],
                            path: entry_path,
                        }));
                        let mut error_context = ErrorContext::new();
                        let checked =
                            typecheck_module(&module, vec![], &mut error_context, &interner)
                                .expect("Typechecking error");
                        let mut fc = FormattingContext::new(&checked.0.vtc);
                        let ir_text = format_to_string(&checked.0.ir_module, &mut fc);
                        write!(parsed_file, "{}", ir_text).unwrap();
                    }
                }
                Err(_) => (),
            }
        }
    }
}

//TODO: Rewrite tests
/*
#[cfg(test)]
mod operator_tests {
    use std::fmt::Debug;

    use crate::ast::OperatorElement::*;
    use crate::ast::{Expr, Literal, Name, NumberType, OperatorElement};
    use crate::common::ModuleIdentifier;
    use crate::errors::ErrorContext;
    use crate::modules::{UntypedModule, UntypedModuleData};
    use crate::parsing::parser::parse;
    use crate::symbol_names::{IText, StringInterner};
    use crate::typechecker::env::Environment;
    use crate::typechecker::prelude::prelude;
    use crate::typechecker::{infer_top_level_block, typecheck_module, Context, LocalEnvironment};
    use crate::typed_ast::{TExprData, TExpression};
    use crate::types::{int, Type};
    use std::fs::read_to_string;
    use std::rc::Rc;

    fn int_lit(lit: IText) -> OperatorElement<Name<()>, IText, ()> {
        Expression(
            Expr::Literal(Literal::Number(lit, NumberType::Integer), ()),
            (),
        )
    }

    fn int_lit_typed(lit: IText) -> TExpression {
        TExpression::new(
            TExprData::Literal(Literal::Number(lit, NumberType::Integer)),
            int(),
        )
    }

    fn lookup<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        name: &IText,
    ) -> Type {
        let sch = env.lookup_value(name).unwrap();
        ctx.inst(sch)
    }

    fn lookup_operator<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        name: &IText,
    ) -> Type {
        let (sch, _) = env.lookup_operator(name).unwrap();
        ctx.inst(sch)
    }

    fn add_op<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        interner: &StringInterner,
    ) -> TExpression {
        TExpression::new(
            TExprData::Variable(interner.intern_str("+")),
            lookup_operator(env, ctx, &interner.intern_str("+")),
        )
    }

    fn mul_op<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        interner: &StringInterner,
    ) -> TExpression {
        TExpression::new(
            TExprData::Variable(interner.intern_str("*")),
            lookup_operator(env, ctx, &interner.intern_str("*")),
        )
    }

    fn neg_op<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        interner: &StringInterner,
    ) -> TExpression {
        TExpression::new(
            TExprData::Variable(interner.intern_str("~")),
            lookup_operator(env, ctx, &interner.intern_str("~")),
        )
    }

    fn neg_expr<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        interner: &StringInterner,
        expr: TExpression,
    ) -> TExpression {
        TExpression::new(
            TExprData::FunctionCall(neg_op(env, ctx, interner).into(), vec![expr]),
            int(),
        )
    }

    #[test]
    fn op_transform_simple_binary() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        let interner = StringInterner::new();
        env.import_prelude(&interner);
        let ops = vec![
            int_lit(interner.intern_str("1")),
            Operator(interner.intern_str("+"), ()),
            int_lit(interner.intern_str("2")),
        ];
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx, &interner).into(),
                vec![
                    int_lit_typed(interner.intern_str("1")),
                    int_lit_typed(interner.intern_str("2")),
                ],
            ),
            int(),
        );
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_binary_precedence() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        let interner = StringInterner::new();
        env.import_prelude(&interner);
        let ops = vec![
            int_lit(interner.intern_str("2")),
            Operator(interner.intern_str("+"), ()),
            int_lit(interner.intern_str("2")),
            Operator(interner.intern_str("*"), ()),
            int_lit(interner.intern_str("3")),
        ];
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx, &interner).into(),
                vec![
                    int_lit_typed(interner.intern_str("2")),
                    TExpression::new(
                        TExprData::FunctionCall(
                            mul_op(&env, &mut ctx, &interner).into(),
                            vec![
                                int_lit_typed(interner.intern_str("2")),
                                int_lit_typed(interner.intern_str("3")),
                            ],
                        ),
                        int(),
                    ),
                ],
            ),
            int(),
        );
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_unary_simple() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        let interner = StringInterner::new();
        env.import_prelude(&interner);
        let ops = vec![
            Operator(interner.intern_str("~"), ()),
            Operator(interner.intern_str("~"), ()),
            int_lit(interner.intern_str("1")),
        ];
        let e1 = neg_expr(
            &env,
            &mut ctx,
            &interner,
            int_lit_typed(interner.intern_str("1")),
        );
        let exprs = neg_expr(&env, &mut ctx, &interner, e1);
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_unary_binary() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        let interner = StringInterner::new();
        env.import_prelude(&interner);
        let ops = vec![
            Operator(interner.intern_str("~"), ()),
            int_lit(interner.intern_str("1")),
            Operator(interner.intern_str("+"), ()),
            Operator(interner.intern_str("~"), ()),
            Operator(interner.intern_str("~"), ()),
            int_lit(interner.intern_str("2")),
        ];
        let e2 = neg_expr(
            &env,
            &mut ctx,
            &interner,
            int_lit_typed(interner.intern_str("2")),
        );
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx, &interner).into(),
                vec![
                    neg_expr(
                        &env,
                        &mut ctx,
                        &interner,
                        int_lit_typed(interner.intern_str("1")),
                    ),
                    neg_expr(&env, &mut ctx, &interner, e2),
                ],
            ),
            int(),
        );
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_complex() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        let interner = StringInterner::new();
        env.import_prelude(&interner);
        let ops = vec![
            Operator(interner.intern_str("~"), ()),
            int_lit(interner.intern_str("1")),
            Operator(interner.intern_str("*"), ()),
            int_lit(interner.intern_str("2")),
            Operator(interner.intern_str("+"), ()),
            Operator(interner.intern_str("~"), ()),
            int_lit(interner.intern_str("3")),
        ];
        let e2 = neg_expr(
            &env,
            &mut ctx,
            &interner,
            int_lit_typed(interner.intern_str("2")),
        );
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx, &interner).into(),
                vec![
                    TExpression::new(
                        TExprData::FunctionCall(
                            mul_op(&env, &mut ctx, &interner).into(),
                            vec![
                                neg_expr(
                                    &env,
                                    &mut ctx,
                                    &interner,
                                    int_lit_typed(interner.intern_str("1")),
                                ),
                                int_lit_typed(interner.intern_str("2")),
                            ],
                        ),
                        int(),
                    ),
                    neg_expr(
                        &env,
                        &mut ctx,
                        &interner,
                        int_lit_typed(interner.intern_str("3")),
                    ),
                ],
            ),
            int(),
        );
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_bin_assoc() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        let interner = StringInterner::new();
        env.import_prelude(&interner);
        let ops = vec![
            int_lit(interner.intern_str("4")),
            Operator(interner.intern_str("+"), ()),
            int_lit(interner.intern_str("5")),
            Operator(interner.intern_str("+"), ()),
            int_lit(interner.intern_str("6")),
        ];
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx, &interner).into(),
                vec![
                    TExpression::new(
                        TExprData::FunctionCall(
                            add_op(&env, &mut ctx, &interner).into(),
                            vec![
                                int_lit_typed(interner.intern_str("4")),
                                int_lit_typed(interner.intern_str("5")),
                            ],
                        ),
                        int(),
                    ),
                    int_lit_typed(interner.intern_str("6")),
                ],
            ),
            int(),
        );
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }
}

#[cfg(test)]
mod typecheck_tests {
    use crate::ast::{Associativity, OperatorSpecification};
    use crate::common::ModuleIdentifier;
    use crate::errors::ErrorContext;
    use crate::parsing::parser::{parse, parse_type};
    use crate::parsing::token::Span;
    use crate::symbol_names::{IText, StringInterner};
    use crate::typechecker::env::Environment;
    use crate::typechecker::{generalize, infer_top_level_block, Context, LocalEnvironment};
    use std::fmt::Debug;
    use std::fs::read_to_string;

    fn assert_value_type<'a>(
        ctx: &mut Context<Span>,
        env: &mut LocalEnvironment<Span>,
        interner: &StringInterner,
        value: &IText,
        expected_type: &'a str,
    ) {
        let value_scheme = ctx.mod_env.lookup_value(value).unwrap().clone();
        let value_type = ctx.inst(&value_scheme);
        let expected_ta = parse_type(expected_type, interner).unwrap();
        let expected_tp = ctx.type_from_annotation(env, &expected_ta);
        ctx.unify_rec(&value_type, &expected_tp).unwrap();
    }

    fn assert_operator_type<'a>(
        ctx: &mut Context<Span>,
        env: &mut LocalEnvironment<Span>,
        interner: &StringInterner,
        name: &IText,
        expected_type: &'a str,
        expected_spec: OperatorSpecification,
    ) {
        let (op_scheme, op_spec) = ctx.mod_env.lookup_operator(name).unwrap().clone();
        let op_type = ctx.inst(&op_scheme);
        let expected_ta = parse_type(expected_type, interner).unwrap();
        let expected_tp = ctx.type_from_annotation(env, &expected_ta);
        assert_eq!(op_spec, expected_spec);
        ctx.unify_rec(&op_type, &expected_tp).unwrap();
    }

    #[test]
    fn simple_types() {
        let interner = StringInterner::new();
        let content = read_to_string("tests/typechecking_old/simple.ca").unwrap();
        let parsed = parse(&content, &interner).unwrap();
        let mut error_context = ErrorContext::new();
        let mut env = LocalEnvironment::new();
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Simple")));
        env.import_prelude(&interner);
        let typed = infer_top_level_block(&mut env, &mut ctx, &parsed);
        assert_value_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("fac"),
            "Int -> Int",
        );
        assert_operator_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("~"),
            "Bool -> Bool",
            OperatorSpecification::Prefix,
        );
        assert_operator_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("|||"),
            "Bool -> Bool -> Bool",
            OperatorSpecification::Infix(30, Associativity::None),
        );
        assert_value_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("add"),
            "Int -> Int -> Int",
        );
        assert_value_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("isEq"),
            "a -> a -> bool",
        );
        assert_value_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("unit"),
            "Unit",
        );
        assert_value_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("str"),
            "String",
        );
        assert_value_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("id"),
            "a -> a",
        );
        assert_value_type(
            &mut ctx,
            &mut env,
            &interner,
            &interner.intern_str("str2"),
            "String",
        );
        error_context.handle_errors().unwrap();
    }
}
*/
