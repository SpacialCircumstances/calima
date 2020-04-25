use std::fmt::{Display, Formatter, Debug};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumberType {
    Integer,
    Float,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Literal<'a> {
    String(&'a str),
    Number(&'a str, NumberType),
    Unit,
    Boolean(bool)
}

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(string) => write!(f, "\"{}\"", string),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
            Literal::Number(num, _) => write!(f, "{}", num)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeAnnotation<'a> {
    Name(&'a str), //First letter uppercase
    Generic(&'a str), //First letter lowercase
    Function(Box<TypeAnnotation<'a>>, Box<TypeAnnotation<'a>>),
    Parameterized(&'a str, Vec<TypeAnnotation<'a>>),
    Tuple(Vec<TypeAnnotation<'a>>)
}

impl<'a> Display for TypeAnnotation<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAnnotation::Name(name) => write!(f, "{}", name),
            TypeAnnotation::Generic(name) => write!(f, "{}", name),
            TypeAnnotation::Function(i, o) => write!(f, "({} => {})", *i, *o),
            TypeAnnotation::Parameterized(name, params) => {
                write!(f, "({} ", name)?;
                for p in params {
                    write!(f, "{} ", p)?;
                }
                write!(f, ")")
            },
            TypeAnnotation::Tuple(elements) => {
                write!(f, "(")?;
                for i in 0..elements.len()-1 {
                    write!(f, "{}, ", elements[i])?;
                }
                // A tuple always has one element
                write!(f, "{}", elements.last().unwrap())?;
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier<'a> {
    Simple(&'a str),
    Annotated(&'a str, TypeAnnotation<'a>),
}

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Simple(name) => write!(f, "{}", name),
            Identifier::Annotated(name, ta) => write!(f, "({}: {})", name, ta)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<'a> {
    Any,
    Name(Identifier<'a>),
    Tuple(Vec<Pattern<'a>>),
    Literal(Literal<'a>),
    Record(Vec<(&'a str, Pattern<'a>)>),
    UnionUnwrap(&'a str, Box<Pattern<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<'a> {
    Alias(TypeAnnotation<'a>),
    Record(Vec<(&'a str, TypeAnnotation<'a>)>),
    Union(Vec<(&'a str, TypeAnnotation<'a>)>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Modifier {
    Rec
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(Vec<Modifier>, Pattern<'a>, Expr<'a, Data>, Data),
    Do(Expr<'a, Data>, Data),
    Import(&'a str, Data), //TODO: Support complex imports
    Region(&'a str, Data),
    Type(&'a str, TypeDefinition<'a>, Data)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a, Data> {
    pub statements: Vec<Statement<'a, Data>>,
    pub result: Box<Expr<'a, Data>>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a, Data> {
    Variable(Vec<&'a str>, Data),
    FunctionCall(Box<Expr<'a, Data>>, Vec<Expr<'a, Data>>, Data),
    OperatorCall(Box<Expr<'a, Data>>, &'a str, Box<Expr<'a, Data>>, Data),
    Record(Vec<(&'a str, Expr<'a, Data>)>, Data),
    Tuple(Vec<Expr<'a, Data>>, Data),
    Literal(Literal<'a>, Data),
    Lambda(Vec<Pattern<'a>>, Block<'a, Data>, Data),
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { data: Data, value: Box<Expr<'a, Data>>, matches: Vec<(Pattern<'a>, Block<'a, Data>)> },
    List(Vec<Expr<'a, Data>>, Data)
}