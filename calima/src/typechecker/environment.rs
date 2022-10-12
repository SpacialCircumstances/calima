use crate::ast::OperatorSpecification;
use crate::ir::{Val, VarRef};
use crate::symbol_names::IText;
use crate::typechecker::Context;
use crate::types::{GenericId, Scheme, Type};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::marker::PhantomData;

#[derive(Clone)]
pub struct ScopeEnvironment<Data: Copy + Debug> {
    values: HashMap<IText, Val>,
    operators: HashMap<IText, OperatorSpecification>,
    mono_vars: HashSet<GenericId>,
    named_generics: HashMap<IText, GenericId>,
    phantom_data: PhantomData<Data>, //TODO: Remove once we have tracking again
}

impl<Data: Copy + Debug> ScopeEnvironment<Data> {
    pub fn new() -> Self {
        ScopeEnvironment {
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

    pub fn bind(&mut self, name: IText, val: Val) {
        self.values.insert(name, val);
    }

    pub fn bind_operator(&mut self, name: IText, val: Val, op_spec: OperatorSpecification) {
        self.bind(name.clone(), val);
        self.operators.insert(name, op_spec);
    }

    pub fn add_monomorphic_var(&mut self, id: GenericId) {
        self.mono_vars.insert(id);
    }

    pub fn lookup_value(&self, name: &IText) -> Option<&Val> {
        self.values.get(name)
    }

    pub fn lookup_operator(&self, name: &IText) -> Option<(Val, OperatorSpecification)> {
        self.lookup_value(name)
            .cloned()
            .and_then(|v| self.operators.get(name).map(|n| (v, *n)))
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

#[derive(Clone)]
pub struct ClosedEnvironment<Data: Copy + Debug> {
    values: HashMap<IText, Val>,
    operators: HashMap<IText, OperatorSpecification>,
    phantom_data: PhantomData<Data>, //TODO: Remove once we have tracking again
}

impl<Data: Copy + Debug> ClosedEnvironment<Data> {
    pub fn new(env: ScopeEnvironment<Data>) -> Self {
        ClosedEnvironment {
            values: env.values,
            operators: env.operators,
            phantom_data: PhantomData::default(),
        }
    }

    pub fn lookup_value(&self, name: &IText) -> Option<&Val> {
        self.values.get(name)
    }

    pub fn lookup_operator(&self, name: &IText) -> Option<(Val, OperatorSpecification)> {
        self.lookup_value(name)
            .cloned()
            .and_then(|v| self.operators.get(name).map(|n| (v, *n)))
    }
}
