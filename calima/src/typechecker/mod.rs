use crate::ast::*;
use crate::ast_common::*;
use crate::common::ModuleIdentifier;
use crate::errors::{CompilerError, ErrorContext, MainFunctionErrorKind};
use crate::formatting::format_iter;
use crate::modules::{
    TypedModule, TypedModuleData, TypedModuleTree, UntypedModule, UntypedModuleTree,
};
use crate::parsing::token::Span;
use crate::typechecker::env::{Environment, ModuleEnvironment, Opening};
use crate::typechecker::substitution::Substitution;
use crate::typechecker::symbol_table::{Location, SymbolTable};
use crate::typed_ast::*;
use crate::types::*;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::fmt::Debug;
use std::rc::Rc;

pub mod env;
mod prelude;
pub mod substitution;
mod symbol_table;

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

//TODO: Additional information
#[derive(Debug, Clone)]
pub struct TypeError<Data> {
    pub location: Data,
    pub message: String,
}

impl<Data: Copy + Debug> TypeError<Data> {
    fn type_not_found(typename: &str, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Type {} not defined or imported", typename),
        }
    }

    fn var_not_found(varname: &str, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Variable {} not defined or imported", varname),
        }
    }

    fn operator_not_found(opname: &str, location: Data) -> Self {
        TypeError {
            location,
            message: format!("Operator {} is not defined or imported", opname),
        }
    }

    fn unification(
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

    fn repeated_unassociative_operators(location: Data, ops: &[&str]) -> Self {
        TypeError {
            location,
            message: format!(
                "Repeated operators without associativity used: {}",
                format_iter(ops.iter(), ", ")
            ),
        }
    }

    fn remaining_unary_operators(location: Data, ops: &[&str]) -> Self {
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
enum UnificationError {
    UnificationError,
    RecursiveType,
    Propagation,
}

fn substitute(subst: &Substitution<Type>, typ: &Type) -> Type {
    match typ {
        Type::Basic(_) => typ.clone(),
        Type::Var(v) => match subst[(*v).0].as_ref() {
            Some(t) => substitute(subst, t),
            None => typ.clone(),
        },
        Type::Parameterized(t, params) => Type::Parameterized(
            t.clone(),
            params.into_iter().map(|t| substitute(subst, t)).collect(),
        ),
        Type::Error => Type::Error,
        _ => unimplemented!(),
    }
}

fn substitute_scheme(subst: &Substitution<Type>, schem: &Scheme) -> Scheme {
    //All variables in scheme cannot be substituted, because they should be general
    let subst_type = substitute(subst, &schem.2);
    Scheme(schem.0.clone(), schem.1.clone(), subst_type)
}

pub struct Context<'input, Data: Copy + Debug> {
    generic_id: usize,
    type_subst: Substitution<Type>,
    errors: Vec<TypeError<Data>>,
    module: ModuleIdentifier,
    mod_env: ModuleEnvironment<'input>,
}

impl<'input, Data: Copy + Debug> Context<'input, Data> {
    pub fn new(module: ModuleIdentifier) -> Self {
        Context {
            generic_id: 0,
            type_subst: Substitution::new(),
            errors: Vec::new(),
            module,
            mod_env: ModuleEnvironment::new(),
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
            Type::Reference(_, t) => self.check_occurs(gid, &*t),
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
                        .map(|(p1, p2)| self.unify_rec(p1, p2))
                        .collect()
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

    fn lookup_var(&mut self, env: &LocalEnvironment<Data>, name: &str, loc: Data) -> Type {
        match env.lookup_value(name) {
            Some(sch) => self.inst(sch),
            None => {
                self.add_error(TypeError::var_not_found(name, loc));
                Type::Error
            }
        }
    }

    fn lookup_operator(
        &mut self,
        env: &LocalEnvironment<Data>,
        name: &str,
        loc: Data,
    ) -> (Type, Option<OperatorSpecification>) {
        match env.lookup_operator(name) {
            Some((tp, op)) => (self.inst(tp), Some(*op)),
            None => {
                self.add_error(TypeError::operator_not_found(name, loc));
                (Type::Error, None)
            }
        }
    }

    fn export_value(&mut self, name: &'input str, typ: Scheme) {
        //TODO: Error if exists
        self.mod_env.add_value(name, typ);
    }

    fn export_operator(&mut self, name: &'input str, typ: Scheme, ops: OperatorSpecification) {
        //TODO: Error if exists
        self.mod_env.add_operator(name, typ, ops);
    }

    fn type_from_annotation(
        &mut self,
        env: &mut LocalEnvironment<'input, Data>,
        ta: &TypeAnnotation<'input, Data>,
    ) -> Type {
        fn to_type<'input, Data: Copy + Debug>(
            ctx: &mut Context<Data>,
            env: &mut LocalEnvironment<'input, Data>,
            ta: &TypeAnnotation<'input, Data>,
        ) -> Result<Type, TypeError<Data>> {
            match ta {
                TypeAnnotation::Name(name) => match PrimitiveType::try_from(name.0[0]) {
                    Ok(pt) => Ok(Type::Basic(TypeDef::Primitive(pt))),
                    Err(()) => Err(TypeError::type_not_found(&*name.to_string(), name.1)),
                },
                TypeAnnotation::Function(ta1, ta2) => to_type(ctx, env, &*ta1)
                    .and_then(|t1| to_type(ctx, env, &*ta2).map(|t2| (t1, t2)))
                    .map(|(t1, t2)| Type::Parameterized(ComplexType::Function, vec![t1, t2])),
                TypeAnnotation::Generic(gname) => {
                    Ok(Type::Var(env.get_or_create_generic(ctx, gname.0, gname.1)))
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
            sch.1.iter().map(|v| (*v, self.new_generic())).collect();
        replace(&sch.2, &|id| mapping.get(&id).cloned())
    }

    fn bind_to_pattern<F: Fn(&Type, &mut LocalEnvironment<'input, Data>) -> Scheme>(
        &mut self,
        env: &mut LocalEnvironment<'input, Data>,
        pattern: &BindPattern<'input, TypeAnnotation<'input, Data>, Data>,
        tp: &mut Type,
        to_scheme: F,
        export: bool,
    ) {
        match pattern {
            BindPattern::Any(_) => (),
            BindPattern::Name(idt, ta, loc) => {
                match ta {
                    Some(ta) => {
                        let annot = self.type_from_annotation(env, ta);
                        self.unify(tp, &annot, UnificationSource::TypeAnnotation, *loc);
                    }
                    None => (),
                }
                let scheme = to_scheme(&tp, env);
                if export {
                    self.export_value(idt, scheme.clone());
                }
                env.add(idt, scheme, Location::Local(*loc));
            }
            BindPattern::UnitLiteral(loc) => {
                self.unify(tp, &unit(), UnificationSource::PatternMatching, *loc)
            }
            _ => (),
        }
    }

    fn bind_to_pattern_generalized(
        &mut self,
        env: &mut LocalEnvironment<'input, Data>,
        pattern: &BindPattern<'input, TypeAnnotation<'input, Data>, Data>,
        tp: &mut Type,
        export: bool,
    ) {
        self.bind_to_pattern(env, pattern, tp, |t, env| generalize(env, t), export);
    }

    fn bind_to_pattern_directly(
        &mut self,
        env: &mut LocalEnvironment<'input, Data>,
        pattern: &BindPattern<'input, TypeAnnotation<'input, Data>, Data>,
        tp: &mut Type,
    ) {
        self.bind_to_pattern(env, pattern, tp, |t, _env| Scheme::simple(t.clone()), false);
    }

    fn call_operator(
        &mut self,
        exprs: &mut Vec<TExpression<'input>>,
        last_name: &'input str,
        last_type: &Type,
        loc: Data,
    ) {
        let r = exprs.pop().unwrap();
        let l = exprs.pop().unwrap();
        let last_expr = TExpression::new(TExprData::Variable(last_name), last_type.clone());
        let fc = function_call(self, last_expr, vec![l, r], loc);
        exprs.push(fc)
    }

    fn transform_operators(
        &mut self,
        env: &mut LocalEnvironment<'input, Data>,
        elements: &Vec<OperatorElement<'input, Data>>,
        top_location: Data,
    ) -> TExpression<'input> {
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
                                        if assoc == Associativity::None
                                            || last_assoc == &Associativity::None
                                        {
                                            self.add_error(
                                                TypeError::repeated_unassociative_operators(
                                                    top_location,
                                                    &[*last_op, *name],
                                                ),
                                            );
                                        } else if assoc == Associativity::Left {
                                            let (last_name, last_type, _, _, data) =
                                                bin_ops.pop().unwrap();
                                            self.call_operator(
                                                &mut exprs, last_name, &last_type, data,
                                            );
                                        }
                                    } else if last_prec > &op_prec {
                                        let (last_name, last_type, _, _, data) =
                                            bin_ops.pop().unwrap();
                                        self.call_operator(&mut exprs, last_name, &last_type, data)
                                    }
                                    bin_ops.push((*name, op_tp, op_prec, assoc, data))
                                }
                            }
                        }
                        Some(OperatorSpecification::Prefix) => un_ops.push((*name, op_tp)),
                    }
                }
                OperatorElement::Expression(oexpr) => {
                    let mut expr = infer_expr(env, self, oexpr);

                    while let Some((op_name, op_tp)) = un_ops.pop() {
                        let op_expr = TExpression::new(TExprData::Variable(op_name), op_tp.clone());
                        expr = function_call(self, op_expr, vec![expr], *oexpr.get_location());
                    }

                    exprs.push(expr);
                }
            }
        }

        if !un_ops.is_empty() {
            let op_names: Vec<&str> = un_ops.iter().map(|(a, _)| *a).collect();
            self.add_error(TypeError::remaining_unary_operators(
                top_location,
                &op_names,
            ))
        }

        while let Some((op_name, op_type, _, _, data)) = bin_ops.pop() {
            self.call_operator(&mut exprs, op_name, &op_type, data);
        }

        exprs.pop().unwrap()
    }
}

impl<'input> Context<'input, Span> {
    fn publish_errors(&mut self, error_ctx: &mut ErrorContext) {
        let name = self.module.clone();
        self.errors
            .drain(..)
            .for_each(|e| error_ctx.add_error(CompilerError::TypeError(e, name.clone())))
    }
}

//TODO: Immutable environments or something similar to allow variable shadowing?
#[derive(Clone)]
struct LocalEnvironment<'a, Data: Copy + Debug> {
    values: SymbolTable<'a, Scheme, Data>,
    operators: SymbolTable<'a, (Scheme, OperatorSpecification), Data>,
    modules: SymbolTable<'a, Rc<ModuleEnvironment<'a>>, Data>,
    mono_vars: HashSet<GenericId>,
    named_generics: SymbolTable<'a, GenericId, Data>,
    depth: usize,
}

impl<'a, Data: Copy + Debug> LocalEnvironment<'a, Data> {
    fn new() -> Self {
        LocalEnvironment {
            values: SymbolTable::new(),
            operators: SymbolTable::new(),
            mono_vars: HashSet::new(),
            named_generics: SymbolTable::new(),
            modules: SymbolTable::new(),
            depth: 0,
        }
    }

    fn lookup_generic(&self, name: &'a str) -> Option<GenericId> {
        self.named_generics.get(name).copied()
    }

    fn get_or_create_generic(
        &mut self,
        ctx: &mut Context<Data>,
        name: &'a str,
        location: Data,
    ) -> GenericId {
        match self.named_generics.get(name) {
            None => {
                let gid = ctx.next_id();
                self.named_generics
                    .add(name, gid, Location::Local(location));
                gid
            }
            Some(gid) => *gid,
        }
    }

    fn add_operator(
        &mut self,
        name: &'a str,
        sch: Scheme,
        op: OperatorSpecification,
        location: Location<Data>,
    ) {
        self.operators.add(name, (sch, op), location);
    }

    fn add(&mut self, name: &'a str, sch: Scheme, location: Location<Data>) {
        self.values.add(name, sch, location);
    }

    fn add_monomorphic_var(&mut self, id: GenericId) {
        self.mono_vars.insert(id);
    }

    fn import_scheme(ctx: &mut Context<Data>, tp: &Scheme) -> Scheme {
        let mapping: HashMap<GenericId, GenericId> =
            tp.1.iter().map(|v| (*v, ctx.next_id())).collect();
        let tp = replace(&tp.2, &|gid| mapping.get(&gid).copied().map(Type::Var));
        let vars = mapping.values().copied().collect();
        Scheme(HashSet::new(), vars, tp)
    }

    fn import(
        &mut self,
        name: &'a str,
        module: &Rc<ModuleEnvironment<'a>>,
        opening: Opening<'a>,
        location: Location<Data>,
    ) {
        let module = module.clone();

        for (name, scheme) in module.opened_values(&opening) {
            //TODO: Cause error if exists
            self.add(name, scheme, location);
        }

        for (name, scheme, op) in module.opened_operators(&opening) {
            self.add_operator(name, scheme, op, location)
        }

        self.modules.add(name, module, location);
    }
}

impl<'a, Data: Copy + Debug> Environment for LocalEnvironment<'a, Data> {
    fn lookup_value(&self, name: &str) -> Option<&Scheme> {
        self.values.get(name)
    }

    fn lookup_operator(&self, name: &str) -> Option<&(Scheme, OperatorSpecification)> {
        self.operators.get(name)
    }

    fn lookup_module(&self, name: &str) -> Option<&Box<dyn Environment>> {
        //TODO
        None
    }
}

fn generalize<Data: Copy + Debug>(env: &LocalEnvironment<Data>, tp: &Type) -> Scheme {
    fn gen_rec(tp: &Type, mono_vars: &HashSet<GenericId>, scheme_vars: &mut HashSet<GenericId>) {
        match tp {
            Type::Parameterized(_, params) => {
                for p in params {
                    gen_rec(p, mono_vars, scheme_vars);
                }
            }
            Type::Var(gid) if !mono_vars.contains(gid) => {
                scheme_vars.insert(*gid);
            }
            _ => (),
        }
    }
    let mut scheme_vars = HashSet::new();
    gen_rec(tp, &env.mono_vars, &mut scheme_vars);
    Scheme(HashSet::new(), scheme_vars, tp.clone())
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

fn function_call<'input, Data: Copy + Debug>(
    ctx: &mut Context<'input, Data>,
    tfunc: TExpression<'input>,
    args: Vec<TExpression<'input>>,
    loc: Data,
) -> TExpression<'input> {
    let argtypes: Vec<Type> = args.iter().map(TExpression::typ).cloned().collect();
    let ret = ctx.new_generic();
    let mut arg_fun_type = build_function(&argtypes, &ret);
    ctx.unify(
        &mut arg_fun_type,
        tfunc.typ(),
        UnificationSource::FunctionCall,
        loc,
    );
    TExpression::new(TExprData::FunctionCall(tfunc.into(), args), ret)
}

fn infer_expr<'input, Data: Copy + Debug>(
    env: &mut LocalEnvironment<'input, Data>,
    ctx: &mut Context<'input, Data>,
    expr: &Expr<'input, Data>,
) -> TExpression<'input> {
    match expr {
        Expr::Literal(lit, _) => {
            TExpression::new(TExprData::Literal(lit.clone()), get_literal_type(lit))
        }
        Expr::Variable(varname) => {
            let vartype = ctx.lookup_var(env, varname.0[0], varname.1);
            TExpression::new(TExprData::Variable(varname.0[0]), vartype)
        }
        Expr::Lambda {
            params,
            body,
            data: _,
        } => {
            let mut body_env = env.clone();
            let mut param_types = Vec::with_capacity(params.len());
            for param in params {
                let gen = ctx.next_id();
                body_env.add_monomorphic_var(gen);
                let mut tp = Type::Var(gen);
                ctx.bind_to_pattern_directly(&mut body_env, param, &mut tp);
                param_types.push(tp);
            }

            let body = infer_block(&mut body_env, ctx, body);
            let scheme = build_function(&param_types, body.res.typ());
            TExpression::new(
                TExprData::Lambda(params.iter().map(|p| map_bind_pattern(p)).collect(), body),
                scheme,
            )
        }
        Expr::FunctionCall(func, args, loc) => {
            let tfunc = infer_expr(env, ctx, func);
            let targs = args.iter().map(|e| infer_expr(env, ctx, e)).collect();
            function_call(ctx, tfunc, targs, *loc)
        }
        Expr::OperatorCall(elements, loc) => ctx.transform_operators(env, elements, *loc),
        Expr::If {
            data: loc,
            cond,
            if_true,
            if_false,
        } => {
            let tcond = infer_expr(env, ctx, &*cond);
            let mut true_env = env.clone();
            let mut false_env = env.clone();
            let ttrue = infer_block(&mut true_env, ctx, if_true);
            let tfalse = infer_block(&mut false_env, ctx, if_false);
            let mut rett = ttrue.res.typ().clone();
            ctx.unify_check(
                &mut rett,
                tcond.typ(),
                &Type::Basic(TypeDef::Primitive(PrimitiveType::Bool)),
                UnificationSource::If,
                *loc,
            );
            ctx.unify(
                &mut rett,
                tfalse.res.typ(),
                UnificationSource::BlockReturn,
                *loc,
            );
            let case = vec![
                (
                    MatchPattern::Literal(Literal::Boolean(true), Unit::unit()),
                    ttrue,
                ),
                (
                    MatchPattern::Literal(Literal::Boolean(false), Unit::unit()),
                    tfalse,
                ),
            ];
            TExpression::new(TExprData::Case(tcond.into(), case), rett)
        }
        Expr::OperatorAsFunction(name, loc) => {
            let vartype = ctx.lookup_var(env, name, *loc);
            TExpression::new(TExprData::Variable(name), vartype)
        }
        Expr::Tuple(exprs, _) => {
            let texprs: Vec<TExpression> = exprs.iter().map(|e| infer_expr(env, ctx, e)).collect();
            let types = texprs.iter().map(|t| t.typ().clone()).collect();
            let tp = Type::Parameterized(ComplexType::Tuple(texprs.len()), types);
            TExpression::new(TExprData::Tuple(texprs), tp)
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

fn infer_let<'input, Data: Copy + Debug>(
    env: &mut LocalEnvironment<'input, Data>,
    ctx: &mut Context<'input, Data>,
    l: &Let<'input, Data>,
    export: bool,
) -> TStatement<'input> {
    let v = if l.mods.contains(&Modifier::Rec) {
        let mut var = ctx.new_generic();
        let mut body_env = env.clone();
        ctx.bind_to_pattern_directly(&mut body_env, &l.pattern, &mut var);
        let v = infer_expr(&mut body_env, ctx, &l.value);
        ctx.unify(&mut var, &v.typ(), UnificationSource::TypeInference, l.data);
        v
    } else {
        let mut body_env = env.clone();
        infer_expr(&mut body_env, ctx, &l.value)
    };
    let mut tp = v.typ().clone();
    ctx.bind_to_pattern_generalized(env, &l.pattern, &mut tp, export);
    TStatement::Let(v, map_bind_pattern(&l.pattern))
}

fn infer_let_operator<'input, Data: Copy + Debug>(
    env: &mut LocalEnvironment<'input, Data>,
    ctx: &mut Context<'input, Data>,
    l: &LetOperator<'input, Data>,
    export: bool,
) -> TStatement<'input> {
    let (mut op_type, v) = if l.mods.contains(&Modifier::Rec) {
        let recursive_type = ctx.new_generic();
        let mut body_env = env.clone();
        body_env.add_operator(
            &l.name,
            Scheme::simple(recursive_type),
            l.op,
            Location::Local(l.data),
        );
        let value = infer_expr(&mut body_env, ctx, &l.value);
        let mut typ = value.typ().clone();
        ctx.unify(
            &mut typ,
            &value.typ(),
            UnificationSource::TypeInference,
            l.data,
        );
        (typ, value)
    } else {
        let value = infer_expr(env, ctx, &l.value);
        (value.typ().clone(), value)
    };
    if let Some(ta) = &l.ta {
        let tp = ctx.type_from_annotation(env, &ta);
        ctx.unify(&mut op_type, &tp, UnificationSource::TypeAnnotation, l.data);
    }
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
    let scheme = generalize(env, &op_type);
    if export {
        ctx.export_operator(l.name, scheme.clone(), l.op);
    }
    env.add_operator(l.name, scheme, l.op, Location::Local(l.data));
    TStatement::Let(v, BindPattern::Name(l.name, None, Unit::unit()))
}

fn infer_statement<'input, Data: Copy + Debug>(
    env: &mut LocalEnvironment<'input, Data>,
    ctx: &mut Context<'input, Data>,
    statement: &Statement<'input, Data>,
) -> Option<TStatement<'input>> {
    match statement {
        Statement::Region(name, _) => None,
        Statement::Do(expr, _) => Some(TStatement::Do(infer_expr(env, ctx, expr))),
        Statement::Let(l) => Some(infer_let(env, ctx, l, false)),
        Statement::LetOperator(l) => Some(infer_let_operator(env, ctx, l, false)),
    }
}

fn map_bind_pattern<'input, Data>(
    pattern: &BindPattern<'input, TypeAnnotation<Data>, Data>,
) -> BindPattern<'input, Unit, Unit> {
    match pattern {
        BindPattern::Any(_) => BindPattern::Any(Unit::unit()),
        BindPattern::Name(id, _, _) => BindPattern::Name(id, None, Unit::unit()),
        BindPattern::UnitLiteral(_) => BindPattern::UnitLiteral(Unit::unit()),
        _ => unimplemented!(),
    }
}

fn map_match_pattern<'input, Data>(
    pattern: &MatchPattern<'input, TypeAnnotation<Data>, Data>,
) -> MatchPattern<'input, Unit, Unit> {
    match pattern {
        MatchPattern::Any(_) => MatchPattern::Any(Unit::unit()),
        MatchPattern::Literal(lit, _) => MatchPattern::Literal(*lit, Unit::unit()),
        MatchPattern::Name(id, _, _) => MatchPattern::Name(id, None, Unit::unit()),
        _ => unimplemented!(),
    }
}

fn infer_block<'input, Data: Copy + Debug>(
    env: &mut LocalEnvironment<'input, Data>,
    ctx: &mut Context<'input, Data>,
    block: &Block<'input, Data>,
) -> TBlock<'input> {
    let mut block_env = env.clone();
    let tstatements = block
        .statements
        .iter()
        .filter_map(|s| infer_statement(&mut block_env, ctx, s))
        .collect();
    let result = infer_expr(&mut block_env, ctx, &block.result);
    TBlock {
        statements: tstatements,
        res: Box::new(result),
    }
}

fn infer_top_level<'input, Data: Copy + Debug>(
    env: &mut LocalEnvironment<'input, Data>,
    ctx: &mut Context<'input, Data>,
    tls: &TopLevelStatement<'input, Data>,
) -> Option<TStatement<'input>> {
    match tls {
        TopLevelStatement::Let(vis, l) => {
            Some(infer_let(env, ctx, l, vis == &Some(Visibility::Public)))
        }
        TopLevelStatement::LetOperator(vis, l) => Some(infer_let_operator(
            env,
            ctx,
            l,
            vis == &Some(Visibility::Public),
        )),
        TopLevelStatement::Import { .. } => None, //Imports are resolved before typechecking
        TopLevelStatement::Type { .. } => unimplemented!(),
    }
}

fn infer_top_level_block<'input, Data: Copy + Debug>(
    env: &mut LocalEnvironment<'input, Data>,
    ctx: &mut Context<'input, Data>,
    tlb: &TopLevelBlock<'input, Data>,
) -> TBlock<'input> {
    let statements = tlb
        .0
        .iter()
        .filter_map(|st| infer_top_level(env, ctx, st))
        .collect();
    TBlock {
        statements,
        res: Box::from(TExpression::new(TExprData::Literal(Literal::Unit), unit())),
    }
}

fn typecheck_module<'input>(
    unchecked: &UntypedModule<'input>,
    deps: Vec<TypedModule<'input>>,
    error_context: &mut ErrorContext<'input>,
) -> Result<TypedModule<'input>, ()> {
    //TODO: Import dependencies into context
    let mut context = Context::new(unchecked.0.name.clone());
    let mut env = LocalEnvironment::new();

    let prelude = prelude::prelude();
    env.import("Prelude", &prelude, Opening::All, Location::External);

    let tast = infer_top_level_block(&mut env, &mut context, &unchecked.0.ast);
    let rettype = substitute(&context.type_subst, tast.res.typ());
    context.publish_errors(error_context);
    println!("Return type of program: {}", rettype);
    error_context.handle_errors().map(|_| {
        let mod_data = TypedModuleData {
            name: unchecked.0.name.clone(),
            path: unchecked.0.path.clone(),
            deps,
            ir_block: tast,
            subst: context.type_subst,
            env: Rc::new(context.mod_env),
        };
        TypedModule(Rc::new(mod_data))
    })
}

fn typecheck_tree<'input>(
    tree: &mut HashMap<ModuleIdentifier, TypedModule<'input>>,
    untyped: &UntypedModule<'input>,
    err: &mut ErrorContext<'input>,
) -> Result<TypedModule<'input>, ()> {
    let dependencies: Result<Vec<TypedModule<'input>>, ()> = untyped
        .0
        .dependencies
        .iter()
        .map(|ud| match tree.get(&ud.0.name) {
            Some(d) => Ok(d.clone()),
            None => typecheck_tree(tree, ud, err),
        })
        .collect();
    dependencies.and_then(|d| typecheck_module(untyped, d, err))
}

fn verify_main_module(main_mod: &TypedModule, errors: &mut ErrorContext) {
    if let Some(main_type) = main_mod.0.env.lookup_value("main") {
        let mut ctx: Context<Span> = Context::new(main_mod.0.name.clone());
        if let Err(_e) = ctx.unify_rec(&main_type.2, &build_function(&[unit()], &unit())) {
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

pub fn typecheck<'input>(
    errors: &mut ErrorContext<'input>,
    mut module_ctx: UntypedModuleTree<'input>,
) -> Result<TypedModuleTree<'input>, ()> {
    let mut lookup = HashMap::new();
    let main_mod = typecheck_tree(&mut lookup, &module_ctx.main_module, errors)?;
    verify_main_module(&main_mod, errors);
    errors.handle_errors().map(|_| TypedModuleTree {
        main_module: main_mod,
        lookup,
    })
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::ast::OperatorElement::*;
    use crate::ast::{Expr, OperatorElement};
    use crate::ast_common::{Literal, NumberType};
    use crate::common::ModuleIdentifier;
    use crate::errors::ErrorContext;
    use crate::typechecker::env::Environment;
    use crate::typechecker::prelude::prelude;
    use crate::typechecker::{Context, LocalEnvironment};
    use crate::typed_ast::{TExprData, TExpression};
    use crate::types::{int, Type};

    fn int_lit(lit: &str) -> OperatorElement<()> {
        Expression(Expr::Literal(Literal::Number(lit, NumberType::Integer), ()))
    }

    fn int_lit_typed(lit: &str) -> TExpression {
        TExpression::new(
            TExprData::Literal(Literal::Number(lit, NumberType::Integer)),
            int(),
        )
    }

    fn lookup<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        name: &str,
    ) -> Type {
        let sch = env.lookup(name).unwrap();
        env.inst(ctx, sch)
    }

    fn lookup_operator<Data: Copy + Debug>(
        env: &LocalEnvironment<Data>,
        ctx: &mut Context<Data>,
        name: &str,
    ) -> Type {
        let (sch, _) = env.lookup_operator(name).unwrap();
        env.inst(ctx, sch)
    }

    fn add_op<'a, Data: Copy + Debug>(
        env: &LocalEnvironment<'a, Data>,
        ctx: &mut Context<Data>,
    ) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("+"), lookup_operator(env, ctx, "+"))
    }

    fn mul_op<'a, Data: Copy + Debug>(
        env: &LocalEnvironment<'a, Data>,
        ctx: &mut Context<Data>,
    ) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("*"), lookup_operator(env, ctx, "*"))
    }

    fn neg_op<'a, Data: Copy + Debug>(
        env: &LocalEnvironment<'a, Data>,
        ctx: &mut Context<Data>,
    ) -> TExpression<'a> {
        TExpression::new(TExprData::Variable("~"), lookup_operator(env, ctx, "~"))
    }

    fn neg_expr<'a, Data: Copy + Debug>(
        env: &LocalEnvironment<'a, Data>,
        ctx: &mut Context<Data>,
        expr: TExpression<'a>,
    ) -> TExpression<'a> {
        TExpression::new(
            TExprData::FunctionCall(neg_op(env, ctx).into(), vec![expr]),
            int(),
        )
    }

    #[test]
    fn op_transform_simple_binary() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![int_lit("1"), Operator("+", ()), int_lit("2")];
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx).into(),
                vec![int_lit_typed("1"), int_lit_typed("2")],
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
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            int_lit("2"),
            Operator("+", ()),
            int_lit("2"),
            Operator("*", ()),
            int_lit("3"),
        ];
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx).into(),
                vec![
                    int_lit_typed("2"),
                    TExpression::new(
                        TExprData::FunctionCall(
                            mul_op(&env, &mut ctx).into(),
                            vec![int_lit_typed("2"), int_lit_typed("3")],
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
        env.import_module(&mut ctx, &prelude());
        let ops = vec![Operator("~", ()), Operator("~", ()), int_lit("1")];
        let e1 = neg_expr(&env, &mut ctx, int_lit_typed("1"));
        let exprs = neg_expr(&env, &mut ctx, e1);
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }

    #[test]
    fn op_transform_unary_binary() {
        let mut ctx = Context::new(ModuleIdentifier::from_filename(String::from("Test")));
        let mut env = LocalEnvironment::new();
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            Operator("~", ()),
            int_lit("1"),
            Operator("+", ()),
            Operator("~", ()),
            Operator("~", ()),
            int_lit("2"),
        ];
        let e2 = neg_expr(&env, &mut ctx, int_lit_typed("2"));
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx).into(),
                vec![
                    neg_expr(&env, &mut ctx, int_lit_typed("1")),
                    neg_expr(&env, &mut ctx, e2),
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
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            Operator("~", ()),
            int_lit("1"),
            Operator("*", ()),
            int_lit("2"),
            Operator("+", ()),
            Operator("~", ()),
            int_lit("3"),
        ];
        let e2 = neg_expr(&env, &mut ctx, int_lit_typed("2"));
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx).into(),
                vec![
                    TExpression::new(
                        TExprData::FunctionCall(
                            mul_op(&env, &mut ctx).into(),
                            vec![
                                neg_expr(&env, &mut ctx, int_lit_typed("1")),
                                int_lit_typed("2"),
                            ],
                        ),
                        int(),
                    ),
                    neg_expr(&env, &mut ctx, int_lit_typed("3")),
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
        env.import_module(&mut ctx, &prelude());
        let ops = vec![
            int_lit("4"),
            Operator("+", ()),
            int_lit("5"),
            Operator("+", ()),
            int_lit("6"),
        ];
        let exprs = TExpression::new(
            TExprData::FunctionCall(
                add_op(&env, &mut ctx).into(),
                vec![
                    TExpression::new(
                        TExprData::FunctionCall(
                            add_op(&env, &mut ctx).into(),
                            vec![int_lit_typed("4"), int_lit_typed("5")],
                        ),
                        int(),
                    ),
                    int_lit_typed("6"),
                ],
            ),
            int(),
        );
        let res = ctx.transform_operators(&mut env, &ops, ());
        ctx.unify_rec(exprs.typ(), res.typ()).unwrap();
        assert_eq!(exprs.data(), res.data())
    }
}
