use crate::ast::*;
use crate::common::{ModuleId, ModuleName};
use crate::errors::{CompilerError, ErrorContext, MainFunctionErrorKind};
use crate::formatting::format_iter;
use crate::ir;
use crate::ir::{BindTarget, Binding, Constant, Module, Val, VarId, VarRef};
use crate::modules::{
    TypedModule, TypedModuleData, TypedModuleTree, UntypedModule, UntypedModuleTree,
};
use crate::parsing::token::Span;
use crate::symbol_names::{IText, StringInterner};
use crate::typechecker::environment::{ClosedEnvironment, ScopeEnvironment};
use crate::typechecker::ir_lowering::*;
use crate::typechecker::prelude::prelude;
use crate::typechecker::substitution::{substitute, substitute_scheme, Substitution};
use crate::typechecker::type_resolution::TypeResolution;
use crate::types::*;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::Debug;
use std::rc::Rc;

pub mod environment;
mod ir_lowering;
mod prelude;
pub mod substitution;
pub mod type_resolution;

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
        ctx: &ModuleTypecheckContext<Data>,
        vtc: &TypeResolution,
        ue: UnificationError,
        expected: &Type,
        actual: &Type,
        location: Data,
    ) -> Self {
        let expected = substitute(&vtc.subst, expected);
        let actual = substitute(&vtc.subst, actual);
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

pub struct ModuleTypecheckContext<Data: Copy + Debug> {
    generic_id: usize,
    errors: Vec<TypeError<Data>>,
    module: ModuleName,
    mod_id: ModuleId,
    var_id: usize,
}

impl<Data: Copy + Debug> ModuleTypecheckContext<Data> {
    pub fn new(mod_id: ModuleId, module: ModuleName) -> Self {
        ModuleTypecheckContext {
            generic_id: 0,
            errors: Vec::new(),
            module,
            mod_id,
            var_id: 0,
        }
    }

    pub fn new_var(
        &mut self,
        vtc: &mut TypeResolution,
        sch: Scheme,
        name_hint: Option<IText>,
    ) -> VarRef {
        let v = VarRef {
            var_id: VarId(self.var_id),
            mod_id: self.mod_id,
        };

        self.var_id += 1;

        vtc.set_type(v, sch, name_hint);
        v
    }

    pub fn add_operator(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        vtc: &mut TypeResolution,
        name: IText,
        sch: Scheme,
        op_spec: OperatorSpecification,
    ) -> VarRef {
        let v = self.new_var(vtc, sch, Some(name.clone()));
        env.bind_operator(name, Val::Var(v), op_spec);
        v
    }

    pub fn add(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        vtc: &mut TypeResolution,
        name: IText,
        sch: Scheme,
    ) -> VarRef {
        let v = self.new_var(vtc, sch, Some(name.clone()));
        env.bind(name, Val::Var(v));
        v
    }

    fn next_id(&mut self) -> GenericId {
        let id = self.generic_id;
        self.generic_id += 1;
        GenericId {
            mod_id: self.mod_id,
            id,
        }
    }

    fn new_generic(&mut self) -> Type {
        let tr = self.next_id();
        Type::Var(tr)
    }

    fn import(&mut self, env: &mut ScopeEnvironment<Data>, other: &TypedModule) {
        // TODO
    }

    fn import_prelude(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        vtc: &mut TypeResolution,
        interner: &StringInterner,
    ) {
        prelude(self, env, vtc, interner);
    }

    fn bind(
        &mut self,
        vtc: &mut TypeResolution,
        gid: GenericId,
        t2: &Type,
    ) -> Result<(), UnificationError> {
        let existing = vtc.subst.resolve(&gid);
        match existing {
            Some(t) => self.unify_rec(vtc, &t, t2),
            None => Ok(vtc.subst.add(gid, t2.clone())),
        }
    }

    fn check_occurs(&self, vtc: &TypeResolution, gid: GenericId, t: &Type) -> Result<(), ()> {
        match t {
            Type::Var(i) => {
                if gid == *i {
                    Err(())
                } else if let Some(next) = &vtc.subst.resolve(i) {
                    self.check_occurs(vtc, gid, next)
                } else {
                    Ok(())
                }
            }
            Type::Parameterized(_, params) => {
                for p in params {
                    self.check_occurs(vtc, gid, p)?;
                }
                Ok(())
            }
            Type::Basic(_) => Ok(()),
            Type::Reference(t) => self.check_occurs(vtc, gid, &*t),
            Type::Error => panic!("Cannot occurs_check on erroneous type"),
        }
    }

    fn unify_rec(
        &mut self,
        vtc: &mut TypeResolution,
        t1: &Type,
        t2: &Type,
    ) -> Result<(), UnificationError> {
        match (t1, t2) {
            (Type::Error, _) => Err(UnificationError::Propagation),
            (_, Type::Error) => Err(UnificationError::Propagation),
            (_, _) if t1 == t2 => Ok(()),
            (Type::Basic(td1), Type::Basic(td2)) if td1 != td2 => {
                Err(UnificationError::UnificationError)
            }
            (Type::Var(gid), _) => {
                self.check_occurs(vtc, *gid, t2)
                    .map_err(|()| UnificationError::RecursiveType)?;
                self.bind(vtc, *gid, t2)
            }
            (_, Type::Var(gid)) => self.bind(vtc, *gid, t1),
            (Type::Parameterized(p1, params1), Type::Parameterized(p2, params2)) => {
                if p1 != p2 || params1.len() != params2.len() {
                    Err(UnificationError::UnificationError)
                } else {
                    params1
                        .iter()
                        .zip(params2.iter())
                        .try_for_each(|(p1, p2)| self.unify_rec(vtc, p1, p2))
                }
            }
            _ => unimplemented!(),
        }
    }

    fn unify(
        &mut self,
        vtc: &mut TypeResolution,
        target: &mut Type,
        with: &Type,
        source: UnificationSource,
        loc: Data,
    ) {
        if let Err(e) = self.unify_rec(vtc, target, with) {
            self.add_error(TypeError::unification(self, vtc, e, &with, &target, loc));
            *target = Type::Error;
        }
    }

    fn unify_check(
        &mut self,
        vtc: &mut TypeResolution,
        target: &mut Type,
        unify_target: &Type,
        with: &Type,
        source: UnificationSource,
        loc: Data,
    ) {
        if let Err(e) = self.unify_rec(vtc, unify_target, with) {
            self.add_error(TypeError::unification(
                self,
                vtc,
                e,
                &with,
                &unify_target,
                loc,
            ));
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
            ctx: &mut ModuleTypecheckContext<Data>,
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
        vtc: &mut TypeResolution,
        pattern: &BindPattern<IText, TypeAnnotation<Name<Data>, IText, Data>, Data>,
        tp: &mut Type,
        vals: &mut Vec<ir::BindTarget>,
    ) {
        match pattern {
            BindPattern::Any(_) => vals.push(BindTarget::Discard),
            BindPattern::Name(name, ta, _) => {
                // TODO: Check type annotation
                let v = self.add(env, vtc, name.clone(), Scheme::simple(tp.clone()));
                vals.push(BindTarget::Var(v));
            }
            BindPattern::UnitLiteral(data) => {
                self.unify(vtc, tp, &unit(), UnificationSource::PatternMatching, *data);
                vals.push(BindTarget::Discard);
            }
            _ => todo!(),
        }
    }

    fn bind_to_pattern_types(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        vtc: &mut TypeResolution,
        pattern: &BindPattern<IText, TypeAnnotation<Name<Data>, IText, Data>, Data>,
        tp: &mut Type,
    ) {
        match pattern {
            BindPattern::Any(_) => (),
            BindPattern::Name(name, ta, _) => {
                self.add(env, vtc, name.clone(), Scheme::simple(tp.clone()));
            }
            _ => todo!(),
        }
    }

    fn bind_to_pattern_generalized(
        &mut self,
        env: &mut ScopeEnvironment<Data>,
        vtc: &mut TypeResolution,
        pattern: &BindPattern<IText, TypeAnnotation<Name<Data>, IText, Data>, Data>,
        block_builder: &mut BlockBuilder,
        bind_val: Val,
        tp: Type,
    ) {
        match pattern {
            BindPattern::Any(_) => (),
            BindPattern::Name(name, ta, _) => {
                let generalized = env.generalize(&tp);
                let gen_var = self.add(env, vtc, name.clone(), generalized);
                block_builder.add_binding(Binding(
                    BindTarget::Var(gen_var),
                    ir::Expr::Generalize(bind_val),
                ))
            }
            _ => todo!(),
        }
    }
}

impl ModuleTypecheckContext<Span> {
    fn publish_errors(&mut self, error_ctx: &mut ErrorContext) {
        let name = self.module.clone();
        self.errors
            .drain(..)
            .for_each(|e| error_ctx.add_error(CompilerError::TypeError(e, name.clone())))
    }
}

fn call_operator<Data: Copy + Debug>(
    ctx: &mut ModuleTypecheckContext<Data>,
    env: &ScopeEnvironment<Data>,
    vtc: &mut TypeResolution,
    block_builder: &mut BlockBuilder,
    vals: &mut Vec<ir::Val>,
    last_name: &IText,
    last_type: &Type,
    loc: Data,
) {
    //TODO: Error handling
    let r = vals.pop().unwrap();
    let l = vals.pop().unwrap();
    let r_tp = ctx.inst(&vtc.get_type(&r).unwrap());
    let l_tp = ctx.inst(&vtc.get_type(&l).unwrap());
    let (op_val, _op_spec) = env.lookup_operator(last_name).unwrap();
    let op_tp = ctx.inst(&vtc.get_type(&op_val).unwrap());
    let (op_expr, res_tp) =
        function_call(ctx, vtc, op_val, &op_tp, vec![(l, l_tp), (r, r_tp)], loc);

    let res_var = ctx.new_var(vtc, Scheme::simple(res_tp), None);
    block_builder.add_binding(Binding(BindTarget::Var(res_var), op_expr));
    vals.push(Val::Var(res_var));
}

fn transform_operators<Data: Copy + Debug>(
    ctx: &mut ModuleTypecheckContext<Data>,
    env: &mut ScopeEnvironment<Data>,
    vtc: &mut TypeResolution,
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
                let op_tp = ctx.inst(&vtc.get_type(&op_val).unwrap());
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
                                        vtc,
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
                                    vtc,
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
                let mut last_val = infer_expr(env, ctx, vtc, block_builder, oexpr);

                while let Some((op_name, op_tp)) = un_ops.pop() {
                    let (op_val, _) = env.lookup_operator(&op_name).unwrap();
                    let (fc, tp) = function_call(ctx, vtc, op_val, &op_tp, vec![last_val], *loc);
                    let fcv = ctx.new_var(vtc, Scheme::simple(tp.clone()), None);
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
        call_operator(
            ctx,
            env,
            vtc,
            block_builder,
            &mut vals,
            &op_name,
            &op_type,
            data,
        );
    }

    let res = vals.pop().unwrap();
    let res_tp = ctx.inst(&vtc.get_type(&res).unwrap());
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
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
    func_val: Val,
    func_tp: &Type,
    args: Vec<(Val, Type)>,
    loc: Data,
) -> (ir::Expr, Type) {
    let argtypes: Vec<Type> = args.iter().map(|(_, b)| b.clone()).collect();
    let ret = ctx.new_generic();
    let mut arg_fun_type = build_function(&argtypes, &ret);
    ctx.unify(
        vtc,
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
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
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
            let vartp = vtc.get_type(val).unwrap();
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
                ctx.bind_to_pattern_parameter(&mut body_env, vtc, param, &mut tp, &mut param_vals);
                param_types.push(tp);
            }

            let (block, ret_tp) = infer_block(&mut body_env, ctx, vtc, block_builder, body);
            let lambda = ir::Expr::Lambda {
                params: param_vals,
                block,
            };
            let func_tp = build_function(&param_types, &ret_tp);
            let func_sch = Scheme::simple(func_tp.clone());
            let lambda_var = ctx.new_var(vtc, func_sch, None);
            block_builder.add_binding(Binding(BindTarget::Var(lambda_var), lambda));
            (Val::Var(lambda_var), func_tp)
        }
        Expr::FunctionCall(func, args, loc) => {
            let targs = args
                .iter()
                .map(|e| infer_expr(env, ctx, vtc, block_builder, e))
                .collect();
            let (tfunc, functp) = infer_expr(env, ctx, vtc, block_builder, func);
            let (func_call, ret_tp) = function_call(ctx, vtc, tfunc, &functp, targs, *loc);
            let funcc_var = ctx.new_var(vtc, Scheme::simple(ret_tp.clone()), None);
            block_builder.add_binding(Binding(BindTarget::Var(funcc_var), func_call));
            (Val::Var(funcc_var), ret_tp)
        }
        Expr::OperatorCall(elements, loc) => {
            transform_operators(ctx, env, vtc, block_builder, elements, *loc)
        }
        Expr::If {
            data: loc,
            cond,
            if_true,
            if_false,
        } => {
            let (tcond, cond_tp) = infer_expr(env, ctx, vtc, block_builder, &*cond);
            let mut true_env = env.clone();
            let mut false_env = env.clone();
            let (ttrue, true_tp) = infer_block(&mut true_env, ctx, vtc, block_builder, if_true);
            let (tfalse, false_tp) = infer_block(&mut false_env, ctx, vtc, block_builder, if_false);
            let mut rett = true_tp.clone();
            ctx.unify_check(
                vtc,
                &mut rett,
                &cond_tp,
                &Type::Basic(TypeDef::Primitive(PrimitiveType::Bool)),
                UnificationSource::If,
                *loc,
            );
            ctx.unify(
                vtc,
                &mut rett,
                &false_tp,
                UnificationSource::BlockReturn,
                *loc,
            );
            let if_expr = ir::Expr::If {
                condition: tcond,
                if_true: ttrue,
                if_false: tfalse,
            };
            let res_var = ctx.new_var(vtc, Scheme::simple(rett.clone()), None);
            block_builder.add_binding(Binding(BindTarget::Var(res_var), if_expr));
            (Val::Var(res_var), rett)
        }
        Expr::OperatorAsFunction(name, loc) => {
            let val = env.lookup_value(name).expect("Variable not found"); //TODO: Lookup into data structures
            let vartp = vtc.get_type(val).unwrap();
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
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
    block_builder: &mut BlockBuilder,
    l: &Let<Name<Data>, IText, Data>,
    export: bool,
) {
    let (lval, tp) = if l.mods.contains(&Modifier::Rec) {
        let mut rec_tp = ctx.new_generic();
        let mut body_env = env.clone();
        ctx.bind_to_pattern_types(&mut body_env, vtc, &l.pattern, &mut rec_tp);
        let (val, tp) = infer_expr(&mut body_env, ctx, vtc, block_builder, &l.value);
        ctx.unify(
            vtc,
            &mut rec_tp,
            &tp,
            UnificationSource::TypeInference,
            l.data,
        );
        (val, tp)
    } else {
        let mut body_env = env.clone();
        infer_expr(&mut body_env, ctx, vtc, block_builder, &l.value)
    };
    ctx.bind_to_pattern_generalized(env, vtc, &l.pattern, block_builder, lval, tp);
}

fn infer_let_operator<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
    block_builder: &mut BlockBuilder,
    l: &LetOperator<Name<Data>, IText, Data>,
    export: bool,
) {
    let (lval, mut op_type) = if l.mods.contains(&Modifier::Rec) {
        let mut rec_tp = ctx.new_generic();
        let mut body_env = env.clone();
        ctx.add_operator(
            &mut body_env,
            vtc,
            l.name.clone(),
            Scheme::simple(rec_tp.clone()),
            l.op,
        );
        let (val, tp) = infer_expr(&mut body_env, ctx, vtc, block_builder, &l.value);
        ctx.unify(
            vtc,
            &mut rec_tp,
            &tp,
            UnificationSource::TypeInference,
            l.data,
        );
        (val, tp)
    } else {
        let mut body_env = env.clone();
        infer_expr(&mut body_env, ctx, vtc, block_builder, &l.value)
    };

    match l.op {
        OperatorSpecification::Infix(_, _) => {
            let expected_type =
                build_function(&[ctx.new_generic(), ctx.new_generic()], &ctx.new_generic());
            ctx.unify(
                vtc,
                &mut op_type,
                &expected_type,
                UnificationSource::OperatorConstraint(l.op),
                l.data,
            );
        }
        OperatorSpecification::Prefix => {
            let expected_type = build_function(&[ctx.new_generic()], &ctx.new_generic());
            ctx.unify(
                vtc,
                &mut op_type,
                &expected_type,
                UnificationSource::OperatorConstraint(l.op),
                l.data,
            );
        }
    }
    let scheme = env.generalize(&op_type);
    let op_var = ctx.add_operator(env, vtc, l.name.clone(), scheme, l.op);
    block_builder.add_binding(Binding(BindTarget::Var(op_var), ir::Expr::Generalize(lval)))
}

fn infer_statement<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
    block_builder: &mut BlockBuilder,
    statement: &Statement<Name<Data>, IText, Data>,
) {
    match statement {
        Statement::Do(expr, _) => {
            todo!()
        }
        Statement::Let(l) => infer_let(env, ctx, vtc, block_builder, l, false),
        Statement::LetOperator(l) => infer_let_operator(env, ctx, vtc, block_builder, l, false),
    }
}

fn infer_block<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
    block_builder: &mut BlockBuilder,
    block: &Block<Name<Data>, IText, Data>,
) -> (ir::Block, Type) {
    block_builder.begin();
    let mut block_env = env.clone();

    block
        .statements
        .iter()
        .for_each(|s| infer_statement(&mut block_env, ctx, vtc, block_builder, s));

    let (result_val, result_tp) =
        infer_expr(&mut block_env, ctx, vtc, block_builder, &block.result);
    (block_builder.end(result_val), result_tp)
}

fn infer_top_level<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
    block_builder: &mut BlockBuilder,
    tls: &TopLevelStatement<Name<Data>, IText, Data>,
) {
    match tls {
        TopLevelStatement::Let(vis, l) => infer_let(
            env,
            ctx,
            vtc,
            block_builder,
            l,
            vis == &Some(Visibility::Public),
        ),
        TopLevelStatement::LetOperator(vis, l) => infer_let_operator(
            env,
            ctx,
            vtc,
            block_builder,
            l,
            vis == &Some(Visibility::Public),
        ),
        TopLevelStatement::Import { .. } => (), //Imports are resolved before typechecking
        TopLevelStatement::Type { .. } => unimplemented!(),
    }
}

fn infer_top_level_block<Data: Copy + Debug>(
    env: &mut ScopeEnvironment<Data>,
    ctx: &mut ModuleTypecheckContext<Data>,
    vtc: &mut TypeResolution,
    block_builder: &mut BlockBuilder,
    tlb: &TopLevelBlock<Name<Data>, IText, Data>,
) -> ir::Block {
    block_builder.begin();
    for st in &tlb.0 {
        infer_top_level(env, ctx, vtc, block_builder, st);
    }
    block_builder.end(Val::Constant(Constant::Unit))
}

pub fn typecheck_module(
    unchecked: &UntypedModule,
    deps: Vec<TypedModule>,
    error_context: &mut ErrorContext,
    interner: &StringInterner,
    vtc: &mut TypeResolution,
) -> Result<TypedModule, ()> {
    let mut context = ModuleTypecheckContext::new(unchecked.0.id, unchecked.0.name.clone());
    let mut block_builder = BlockBuilder::new();
    let mut env = ScopeEnvironment::new();
    context.import_prelude(&mut env, vtc, interner);

    for dep in &deps {
        context.import(&mut env, dep);
    }

    let ir_block = infer_top_level_block(
        &mut env,
        &mut context,
        vtc,
        &mut block_builder,
        &unchecked.0.ast,
    );

    let ir_module = Module {
        externs: vec![],
        main_block: ir_block,
        export: vec![],
    };

    context.publish_errors(error_context);

    error_context.handle_errors().map(|_| {
        let mod_data = TypedModuleData {
            name: unchecked.0.name.clone(),
            path: unchecked.0.path.clone(),
            deps,
            ir_module,
            env: ClosedEnvironment::new(env),
            id: unchecked.0.id,
        };
        TypedModule(Rc::new(mod_data))
    })
}

pub fn verify_main_module(
    main_mod: &TypedModule,
    vtc: &mut TypeResolution,
    errors: &mut ErrorContext,
    interner: &StringInterner,
) -> Result<(), ()> {
    let main_name = interner.intern_str("main");
    let main_md = &main_mod.0;
    let main_val = main_md.env.lookup_value(&main_name);
    let main_type = main_val.and_then(|v| vtc.get_type(v));

    if let Some(main_sch) = main_type {
        let mut ctx: ModuleTypecheckContext<Span> =
            ModuleTypecheckContext::new(main_mod.0.id, main_mod.0.name.clone());
        if let Err(_e) = ctx.unify_rec(vtc, &main_sch.1, &build_function(&[unit()], &unit())) {
            errors.add_error(CompilerError::MainFunctionError(
                main_mod.0.name.clone(),
                MainFunctionErrorKind::SignatureWrong,
            ));
            Err(())
        } else {
            Ok(())
        }
    } else {
        errors.add_error(CompilerError::MainFunctionError(
            main_mod.0.name.clone(),
            MainFunctionErrorKind::Missing,
        ));
        Err(())
    }
}

#[cfg(test)]
mod ir_gen_tests {
    use crate::common::{ModuleId, ModuleName};
    use crate::formatting::context::format_to_string;
    use crate::ir::FormattingContext;
    use crate::modules::{UntypedModule, UntypedModuleData};
    use crate::parsing::parser::parse;
    use crate::typechecker::type_resolution::TypeResolution;
    use crate::typechecker::typecheck_module;
    use crate::{CompilerState, ErrorContext, StringInterner};
    use goldenfile::Mint;
    use quetta::Text;
    use std::fs::read_dir;
    use std::io::Write;
    use std::path::PathBuf;
    use std::rc::Rc;

    #[test]
    fn test_ir_generation() {
        let mut mint_code = Mint::new("tests/ir_programs/");

        for entry in read_dir("tests/typechecking/").unwrap() {
            match entry {
                Ok(entry) => {
                    let entry_path = entry.path();
                    if entry_path.is_file() {
                        let filename = entry_path.file_name().unwrap();
                        let ir_filename = PathBuf::from(filename).with_extension("ir");
                        let mut parsed_file = mint_code.new_goldenfile(ir_filename).unwrap();

                        let mut errors = ErrorContext::new();
                        let interner = StringInterner::new();
                        let mut state =
                            CompilerState::construct(errors, interner, &entry_path, &vec![], &None)
                                .expect("Failed to construct compiler state for test");
                        let main_mod_id = ModuleName::from_name(state.project_name.clone());
                        match state.typecheck(&main_mod_id, true) {
                            Ok(main_mod) => {
                                let mut fc = FormattingContext::new(&state.vtc);
                                let ir_text = format_to_string(&main_mod.0.ir_module, &mut fc);
                                write!(parsed_file, "{}", ir_text).unwrap();
                            }
                            Err(e) => {
                                state
                                    .error_context
                                    .handle_errors()
                                    .expect("Failed to typecheck");
                            }
                        }
                    }
                }
                Err(_) => (),
            }
        }
    }
}
