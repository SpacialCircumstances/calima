use crate::ast::NumberType;
use crate::common::ModuleId;
use crate::ir::{Constant, Val, VarId, VarRef};
use crate::symbol_names::IText;
use crate::typechecker::substitution::{substitute, substitute_scheme, Substitution};
use crate::types::{PrimitiveType, Scheme, Type, TypeDef};
use std::collections::HashMap;

pub struct TypeResolution {
    name_hints: HashMap<VarRef, IText>,
    types: HashMap<VarRef, Scheme>, //TODO: Use symbol table once generic enough
    pub(crate) subst: Substitution, //TODO: Use getter
}

impl TypeResolution {
    pub fn new() -> Self {
        Self {
            name_hints: HashMap::new(),
            types: HashMap::new(),
            subst: Substitution::new(),
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

    pub fn get_type_subst(&self, val: &Val) -> Option<Scheme> {
        self.get_type(val)
            .map(|t| substitute_scheme(&self.subst, &t))
    }

    pub fn get_name_hint(&self, vr: &VarRef) -> Option<&IText> {
        self.name_hints.get(vr)
    }

    pub fn set_type(&mut self, var: VarRef, sch: Scheme, name_hint: Option<IText>) {
        self.types.insert(var, sch);

        if let Some(nh) = name_hint {
            self.name_hints.insert(var, nh);
        }
    }
}
