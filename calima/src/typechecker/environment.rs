use crate::ast::OperatorSpecification;
use crate::ir::{Val, VarRef};
use crate::symbol_names::IText;
use crate::typechecker::Context;
use crate::types::{GenericId, Scheme, Type};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::marker::PhantomData;

#[derive(Clone)]
pub struct Environment<Data: Copy + Debug> {
    values: HashMap<IText, Val>,
    operators: HashMap<IText, (VarRef, OperatorSpecification)>,
    mono_vars: HashSet<GenericId>,
    named_generics: HashMap<IText, GenericId>,
    phantom_data: PhantomData<Data>, //TODO: Remove once we have tracking again
}

impl<Data: Copy + Debug> Environment<Data> {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            operators: HashMap::new(),
            mono_vars: HashSet::new(),
            named_generics: HashMap::new(),
            phantom_data: PhantomData::default(),
        }
    }

    pub fn lookup_generic(&self, name: &IText) -> Option<GenericId> {
        self.named_generics.get(name).copied()
    }

    pub fn get_or_create_generic(
        &mut self,
        ctx: &mut Context<Data>,
        name: &IText,
        location: Data,
    ) -> GenericId {
        match self.named_generics.get(name) {
            None => {
                let gid = ctx.next_id();
                self.named_generics.insert(name.clone(), gid);
                gid
            }
            Some(gid) => *gid,
        }
    }

    pub fn add_operator(
        &mut self,
        ctx: &mut Context<Data>,
        name: IText,
        sch: Scheme,
        op: OperatorSpecification,
    ) -> VarRef {
        let v = ctx.new_var(sch, Some(name.clone()));
        self.operators.insert(name, (v, op));
        v
    }

    pub fn add(&mut self, ctx: &mut Context<Data>, name: IText, sch: Scheme) -> VarRef {
        let v = ctx.new_var(sch, Some(name.clone()));
        self.bind(name, Val::Var(v));
        v
    }

    pub fn bind(&mut self, name: IText, val: Val) {
        self.values.insert(name, val);
    }

    pub fn add_monomorphic_var(&mut self, id: GenericId) {
        self.mono_vars.insert(id);
    }

    pub fn lookup_value(&self, name: &IText) -> Option<&Val> {
        self.values.get(name)
    }

    pub fn lookup_operator(&self, name: &IText) -> Option<&(VarRef, OperatorSpecification)> {
        self.operators.get(name)
    }

    pub fn generalize(&self, tp: &Type) -> Scheme {
        fn gen_rec(
            tp: &Type,
            mono_vars: &HashSet<GenericId>,
            scheme_vars: &mut HashSet<GenericId>,
        ) {
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
        gen_rec(tp, &self.mono_vars, &mut scheme_vars);
        Scheme(scheme_vars, tp.clone())
    }
}
