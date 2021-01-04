use crate::formatting::tree::{format_children, TreeFormat};
use crate::formatting::{format_record, format_tuple};
use std::fmt::{Display, Formatter};

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
    Boolean(bool),
}

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(string) => write!(f, "\"{}\"", string),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
            Literal::Number(num, _) => write!(f, "{}", num),
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
                Some(ta) => write!(f, "({}: {})", id, ta),
            },
            BindPattern::Tuple(elements, _) => format_tuple(elements, f),
            BindPattern::Record(rows, _) => write!(f, "{}", format_record(rows, ":", ", ")),
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

impl<'a, TA: Display, Data> TreeFormat for MatchPattern<'a, TA, Data> {
    fn get_precedence(&self) -> i32 {
        match self {
            Self::Any(_) => 0,
            Self::Name(_, _, _) => 0,
            Self::Tuple(_, _) => 0,
            Self::Literal(_, _) => 0,
            Self::Record(_, _) => 0,
            Self::SumUnwrap(_, _, _) => 1,
        }
    }

    fn format(&self) -> String {
        match self {
            MatchPattern::Any(_) => format!("_"),
            MatchPattern::Name(id, ta, _) => match ta {
                None => id.to_string(),
                Some(ta) => format!("({}: {})", id, ta),
            },
            MatchPattern::Literal(lit, _) => lit.to_string(),
            MatchPattern::Tuple(elements, _) => {
                format!("({})", format_children(self, elements.iter(), ", "))
            }
            MatchPattern::Record(rows, _) => format_record(rows, ":", ", "),
            MatchPattern::SumUnwrap(constr, None, _) => constr.to_string(),
            MatchPattern::SumUnwrap(constr, Some(pat), _) => {
                format!("{} {}", constr, self.format_child(&*pat))
            }
        }
    }
}

impl<'a, TA: Display, Data> Display for MatchPattern<'a, TA, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MatchPattern::Any(_) => write!(f, "_"),
            MatchPattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta),
            },
            MatchPattern::Literal(lit, _) => write!(f, "{}", lit),
            MatchPattern::Tuple(elements, _) => format_tuple(elements, f),
            MatchPattern::Record(rows, _) => write!(f, "{}", format_record(rows, ":", ", ")),
            MatchPattern::SumUnwrap(constr, None, _) => write!(f, "{}", constr),
            MatchPattern::SumUnwrap(constr, Some(pat), _) => write!(f, "{} {}", constr, *pat),
        }
    }
}
