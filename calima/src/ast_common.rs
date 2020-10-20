use std::fmt::{Display, Formatter};
use crate::util::{format_tuple, format_record};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum OperatorKind {
    Unary,
    Binary
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    None
}

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
pub enum BindPattern<'a, TA: Display, Data> {
    Any(Data),
    Name(&'a str, Option<TA>, Data),
    Tuple(Vec<BindPattern<'a, TA, Data>>, Data),
    Record(Vec<(&'a str, BindPattern<'a, TA, Data>)>, Data),
}

impl<'a, TA: Display, Data> Display for BindPattern<'a, TA, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BindPattern::Any(_) => write!(f, "_"),
            BindPattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta)
            },
            BindPattern::Tuple(elements, _) => format_tuple(elements, f),
            BindPattern::Record(rows, _) => format_record(rows, f, ":", ", "),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchPattern<'a, TA: Display, Data> {
    Any(Data),
    Name(&'a str, Option<TA>, Data),
    Tuple(Vec<MatchPattern<'a, TA, Data>>, Data),
    Literal(Literal<'a>, Data),
    Record(Vec<(&'a str, MatchPattern<'a, TA, Data>)>, Data),
    SumUnwrap(&'a str, Option<Box<MatchPattern<'a, TA, Data>>>, Data),
}

impl<'a, TA: Display, Data> Display for MatchPattern<'a, TA, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MatchPattern::Any(_) => write!(f, "_"),
            MatchPattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta)
            },
            MatchPattern::Literal(lit, _) => write!(f, "{}", lit),
            MatchPattern::Tuple(elements, _) => format_tuple(elements, f),
            MatchPattern::Record(rows, _) => format_record(rows, f, ":", ", "),
            MatchPattern::SumUnwrap(constr, None, _) => write!(f, "{}", constr),
            MatchPattern::SumUnwrap(constr, Some(pat), _) => write!(f, "{} {}", constr, *pat)
        }
    }
}