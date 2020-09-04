use std::fmt::{Display, Formatter};
use crate::util::{format_tuple, format_record};

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
pub enum Identifier<'a, Data> {
    Simple(&'a str, Data),
    Operator(&'a str, Data)
}

impl<'a, Data> Identifier<'a, Data> {
    pub fn to_name(&self) -> &'a str {
        match self {
            Identifier::Simple(name, _) => name,
            Identifier::Operator(name, _) => name
        }
    }
}

impl<'a, Data> Display for Identifier<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Simple(name, _) => write!(f, "{}", name),
            Identifier::Operator(op, _) => write!(f, "({})", op)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<'a, TA: Display, Data> {
    Any(Data),
    Name(Identifier<'a, Data>, Option<TA>, Data),
    Tuple(Vec<Pattern<'a, TA, Data>>, Data),
    Literal(Literal<'a>, Data),
    Record(Vec<(&'a str, Pattern<'a, TA, Data>)>, Data),
    SumUnwrap(&'a str, Option<Box<Pattern<'a, TA, Data>>>, Data),
}

impl<'a, TA: Display, Data> Display for Pattern<'a, TA, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Any(_) => write!(f, "_"),
            Pattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta)
            },
            Pattern::Literal(lit, _) => write!(f, "{}", lit),
            Pattern::Tuple(elements, _) => format_tuple(elements, f),
            Pattern::Record(rows, _) => format_record(rows, f, ":", ", "),
            Pattern::SumUnwrap(constr, None, _) => write!(f, "{}", constr),
            Pattern::SumUnwrap(constr, Some(pat), _) => write!(f, "{} {}", constr, *pat)
        }
    }
}