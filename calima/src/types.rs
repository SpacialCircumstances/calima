use std::collections::HashSet;

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

pub fn func() -> Type {
    Type::Basic(TypeDefinition::Parameterized(ParameterizedType::Function))
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scheme(pub HashSet<GenericId>, pub Type);

impl Scheme {
    pub fn simple(tp: Type) -> Self {
        Scheme(HashSet::new(), tp)
    }
}