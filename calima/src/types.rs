use crate::formatting::tree::{format_children, TreeFormat};
use crate::formatting::{format_iter, format_iter_end};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct RegionId(pub usize);

impl Display for RegionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct RegionInstance {
    pub id: usize,
    pub depth: usize,
}

impl RegionInstance {
    pub fn new(id: usize, depth: usize) -> Self {
        RegionInstance { id, depth }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum Region {
    Var(RegionId),
    Instance(RegionInstance),
}

impl Display for Region {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Region::Var(rid) => write!(f, "{}", rid),
            Region::Instance(ri) => write!(f, "@{}", ri.id),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct GenericId(pub usize);

impl Display for GenericId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    String,
    Unit,
    Char,
}

impl TryFrom<&str> for PrimitiveType {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "Unit" => Ok(PrimitiveType::Unit),
            "Float" => Ok(PrimitiveType::Float),
            "Int" => Ok(PrimitiveType::Int),
            "Char" => Ok(PrimitiveType::Char),
            "Bool" => Ok(PrimitiveType::Bool),
            "String" => Ok(PrimitiveType::String),
            _ => Err(()),
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Unit => write!(f, "Unit"),
            PrimitiveType::Float => write!(f, "Float"),
            PrimitiveType::Int => write!(f, "Int"),
            PrimitiveType::Char => write!(f, "Char"),
            PrimitiveType::Bool => write!(f, "Bool"),
            PrimitiveType::String => write!(f, "String"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum ComplexType {
    Function,
    Tuple(usize), //TODO: User-defined
}

impl Display for ComplexType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComplexType::Function => write!(f, "->"),
            ComplexType::Tuple(e) => write!(f, "Tuple{}", e),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum TypeDef {
    Primitive(PrimitiveType),
    Complex(ComplexType),
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDef::Primitive(pt) => write!(f, "{}", pt),
            TypeDef::Complex(pt) => write!(f, "{}", pt),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Type {
    Basic(TypeDef),
    Parameterized(ComplexType, Vec<Type>),
    Var(GenericId),
    Reference(Region, Box<Type>),
    Error,
}

impl TreeFormat for Type {
    fn get_precedence(&self) -> i32 {
        match self {
            Type::Basic(_) => 0,
            Type::Parameterized(ComplexType::Function, _) => 3,
            Type::Parameterized(ComplexType::Tuple(_), _) => 2,
            Type::Var(_) => 0,
            Type::Reference(_, _) => 4,
            Type::Error => 0,
        }
    }

    fn format(&self) -> String {
        match self {
            Type::Basic(td) => format!("{}", td),
            Type::Var(id) => format!("{}", id),
            Type::Parameterized(p, params) => match p {
                ComplexType::Function => format!(
                    "{} -> {}",
                    self.format_child(&params[0]),
                    self.format_child(&params[1])
                ),
                ComplexType::Tuple(_) => {
                    format!("({})", format_children(self, params.iter(), ", "))
                }
            },
            Type::Reference(reg, tp) => format!("@{} {}", reg, tp),
            Type::Error => "ERROR_TYPE".to_string(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Scheme(pub HashSet<RegionId>, pub HashSet<GenericId>, pub Type);

impl Scheme {
    pub fn simple(tp: Type) -> Self {
        Scheme(HashSet::new(), HashSet::new(), tp)
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            write!(f, "{}", self.2)
        } else {
            write!(
                f,
                "forall {}{}. {}",
                format_iter_end(self.0.iter(), " "),
                format_iter(self.1.iter(), " "),
                self.2
            )
        }
    }
}

pub fn bool() -> Type {
    Type::Basic(TypeDef::Primitive(PrimitiveType::Bool))
}

pub fn string() -> Type {
    Type::Basic(TypeDef::Primitive(PrimitiveType::String))
}

pub fn int() -> Type {
    Type::Basic(TypeDef::Primitive(PrimitiveType::Int))
}

pub fn float() -> Type {
    Type::Basic(TypeDef::Primitive(PrimitiveType::Float))
}

pub fn unit() -> Type {
    Type::Basic(TypeDef::Primitive(PrimitiveType::Unit))
}

pub fn build_function(params: &[Type], ret: &Type) -> Type {
    match params {
        [last] => Type::Parameterized(ComplexType::Function, vec![last.clone(), ret.clone()]),
        _ => {
            let c = params.first().expect("Error getting parameter");
            Type::Parameterized(
                ComplexType::Function,
                vec![c.clone(), build_function(&params[1..], ret)],
            )
        }
    }
}
