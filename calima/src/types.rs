use std::collections::{HashSet, HashMap};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct GenericId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    String,
    Unit,
    Char
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ParameterizedType {
    Function,
    Tuple(u32)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TypeDefinition {
    Primitive(PrimitiveType),
    Parameterized(ParameterizedType) //TODO: User-defined records, sums
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Basic(TypeDefinition),
    Parameterized(Box<Type>, Vec<Type>),
    Var(GenericId)
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scheme(pub HashSet<GenericId>, pub Type);

impl Scheme {
    pub fn simple(tp: Type) -> Self {
        Scheme(HashSet::new(), tp)
    }
}

pub fn func() -> Type {
    Type::Basic(TypeDefinition::Parameterized(ParameterizedType::Function))
}

pub fn bool() -> Type {
    Type::Basic(TypeDefinition::Primitive(PrimitiveType::Bool))
}

pub fn string() -> Type {
    Type::Basic(TypeDefinition::Primitive(PrimitiveType::String))
}

pub fn int() -> Type {
    Type::Basic(TypeDefinition::Primitive(PrimitiveType::Int))
}

pub fn float() -> Type {
    Type::Basic(TypeDefinition::Primitive(PrimitiveType::Float))
}

pub fn unit() -> Type {
    Type::Basic(TypeDefinition::Primitive(PrimitiveType::Unit))
}

pub fn build_function(params: &[Type], ret: &Type) -> Type {
    match params {
        [last] => Type::Parameterized(func().into(), vec![ last.clone(), ret.clone() ]),
        _ => {
            let c = params.first().expect("Error getting parameter");
            Type::Parameterized(func().into(), vec![c.clone(), build_function(&params[1..], ret) ])
        }
    }
}

pub fn deconstruct_function(func: &Type) -> Option<(&Type, &Type)> {
    match func {
        Type::Parameterized(p, params) => {
            match **p {
                Type::Basic(TypeDefinition::Parameterized(ParameterizedType::Function)) if params.len() == 2 => Some((&params[0], &params[1])),
                _ => None
            }
        },
        _ => None
    }
}

//All variables in an Exports map shall be fully substituted to not leave any free variables
#[derive(Debug)]
pub struct Exports<'input>(HashMap<&'input str, Scheme>);

impl<'input> Exports<'input> {
    pub fn new() -> Self {
        Exports(HashMap::new())
    }

    pub fn add_variable(&mut self, name: &'input str, sch: Scheme) {
        self.0.insert(name, sch);
    }

    pub fn iter_vars(&self) -> impl Iterator<Item=(&'input str, &Scheme)> {
        self.0.iter().map(|(a, b)| (*a, b))
    }
}