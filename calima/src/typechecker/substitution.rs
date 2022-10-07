use crate::types::{Scheme, Type};
use std::ops::Index;

#[derive(Debug, Clone)]
pub struct Substitution<T: Clone> {
    subst: Vec<Option<T>>,
}

impl<T: Clone> Substitution<T> {
    pub fn new() -> Self {
        Substitution {
            subst: (1..10).map(|_| None).collect(),
        }
    }

    pub fn add(&mut self, idx: usize, value: T) {
        if idx >= self.subst.len() {
            self.subst.resize(idx + 1, Option::None);
        }
        self.subst[idx] = Some(value);
    }
}

impl<T: Clone> Index<usize> for Substitution<T> {
    type Output = Option<T>;

    fn index(&self, index: usize) -> &Self::Output {
        self.subst.get(index).unwrap_or(&Option::None)
    }
}

pub fn substitute(subst: &Substitution<Type>, typ: &Type) -> Type {
    match typ {
        Type::Basic(_) => typ.clone(),
        Type::Var(v) => match subst[(*v).0].as_ref() {
            Some(t) => substitute(subst, t),
            None => typ.clone(),
        },
        Type::Parameterized(t, params) => {
            Type::Parameterized(*t, params.iter().map(|t| substitute(subst, t)).collect())
        }
        Type::Error => Type::Error,
        _ => unimplemented!(),
    }
}

pub fn substitute_scheme(subst: &Substitution<Type>, schem: &Scheme) -> Scheme {
    //All variables in scheme cannot be substituted, because they should be general
    let subst_type = substitute(subst, &schem.1);
    Scheme(schem.0.clone(), subst_type)
}
