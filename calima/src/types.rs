use std::collections::{HashSet, HashMap};
use std::fmt::{Display, Formatter};
use crate::util::format_iter;
use crate::common::OperatorSpecification;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct GenericId(pub usize);

impl Display for GenericId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PrimitiveType {
    Bool,
    Int,
    Float,
    String,
    Unit,
    Char
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Unit => write!(f, "Unit"),
            PrimitiveType::Float => write!(f, "Float"),
            PrimitiveType::Int => write!(f, "Int"),
            PrimitiveType::Char => write!(f, "Char"),
            PrimitiveType::Bool => write!(f, "Bool"),
            PrimitiveType::String => write!(f, "String")
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ComplexType {
    Function,
    Tuple(u32) //TODO: User-defined
}

impl Display for ComplexType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ComplexType::Function => write!(f, "->"),
            ComplexType::Tuple(e) => write!(f, "Tuple{}", e)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TypeDefinition {
    Primitive(PrimitiveType),
    Complex(ComplexType)
}

impl Display for TypeDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDefinition::Primitive(pt) => write!(f, "{}", pt),
            TypeDefinition::Complex(pt) => write!(f, "{}", pt)
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Basic(TypeDefinition),
    Parameterized(ComplexType, Vec<Type>),
    Var(GenericId)
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Basic(td) => write!(f, "{}", td),
            Type::Var(id) => write!(f, "{}", id),
            Type::Parameterized(p, params) => write!(f, "({} {})", p, format_iter(params.iter(), " "))
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Scheme(pub HashSet<GenericId>, pub Type);

impl Scheme {
    pub fn simple(tp: Type) -> Self {
        Scheme(HashSet::new(), tp)
    }
}

impl Display for Scheme {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            write!(f, "{}", self.1)
        } else {
            write!(f, "forall {}. {}", format_iter(self.0.iter(), " "), self.1)
        }
    }
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
        [last] => Type::Parameterized(ComplexType::Function, vec![ last.clone(), ret.clone() ]),
        _ => {
            let c = params.first().expect("Error getting parameter");
            Type::Parameterized(ComplexType::Function, vec![c.clone(), build_function(&params[1..], ret) ])
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExportValue {
    Value(Scheme),
    Operator(OperatorSpecification, Scheme)
}

//All variables in an Exports map shall be fully substituted to not leave any free variables
#[derive(Debug, Clone)]
pub struct Exports<'input>(HashMap<&'input str, ExportValue>);

impl<'input> Exports<'input> {
    pub fn new() -> Self {
        Exports(HashMap::new())
    }

    pub fn add_operator(&mut self, name: &'input str, sch: Scheme, op: OperatorSpecification) {
        self.add(name, ExportValue::Operator(op, sch))
    }

    pub fn add_value(&mut self, name: &'input str, sch: Scheme) {
        self.add(name, ExportValue::Value(sch));
    }

    pub fn add(&mut self, name: &'input str, exp: ExportValue) {
        self.0.insert(name, exp);
    }

    pub fn iter_vars(&self) -> impl Iterator<Item=(&'input str, &ExportValue)> {
        self.0.iter().map(|(a, b)| (*a, b))
    }
}