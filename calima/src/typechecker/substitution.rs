use crate::types::{GenericId, Scheme, Type};
use std::collections::HashMap;
use std::ops::Index;

#[derive(Debug, Clone)]
pub struct Substitution(HashMap<GenericId, Type>);

impl Substitution {
    pub fn new() -> Self {
        Substitution(HashMap::new())
    }

    pub fn add(&mut self, gen_id: GenericId, tp: Type) {
        self.0.insert(gen_id, tp);
    }

    pub fn resolve(&self, gen_id: &GenericId) -> Option<Type> {
        self.0.get(gen_id).cloned()
    }
}

pub fn substitute(subst: &Substitution, typ: &Type) -> Type {
    match typ {
        Type::Basic(_) => typ.clone(),
        Type::Var(v) => match subst.resolve(v) {
            Some(t) => substitute(subst, &t),
            None => typ.clone(),
        },
        Type::Parameterized(t, params) => {
            Type::Parameterized(*t, params.iter().map(|t| substitute(subst, t)).collect())
        }
        Type::Error => Type::Error,
        _ => unimplemented!(),
    }
}

pub fn substitute_scheme(subst: &Substitution, schem: &Scheme) -> Scheme {
    //All variables in scheme cannot be substituted, because they should be general
    let subst_type = substitute(subst, &schem.1);
    Scheme(schem.0.clone(), subst_type)
}
