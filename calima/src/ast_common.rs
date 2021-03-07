use crate::formatting::tree::{format_children, TreeFormat};
use crate::formatting::{format_iter, format_record, format_tuple};
use crate::names::SymbolName;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name<Data>(pub Vec<SymbolName>, pub Data);

impl<Data> Display for Name<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_iter(self.0.iter(), "."))
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumberType {
    Integer,
    Float,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    None,
}

impl Display for Associativity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Associativity::Left => write!(f, "left"),
            Associativity::Right => write!(f, "right"),
            Associativity::None => write!(f, "none"),
        }
    }
}

impl TryFrom<&str> for Associativity {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "left" => Ok(Associativity::Left),
            "right" => Ok(Associativity::Right),
            "none" => Ok(Associativity::None),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone, Eq)]
pub enum OperatorSpecification {
    Infix(u32, Associativity),
    Prefix,
}

impl Display for OperatorSpecification {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorSpecification::Infix(prec, assoc) => write!(f, "infix {} {}", prec, assoc),
            OperatorSpecification::Prefix => write!(f, "prefix"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Number(String, NumberType),
    Unit,
    Boolean(bool),
}

impl Display for Literal {
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
pub enum BindPattern<TA: Display, Data> {
    Any(Data),
    UnitLiteral(Data),
    Name(SymbolName, Option<TA>, Data),
    Tuple(Vec<BindPattern<TA, Data>>, Data),
    Record(Vec<(SymbolName, BindPattern<TA, Data>)>, Data),
}

impl<TA: Display, Data> Display for BindPattern<TA, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BindPattern::Any(_) => write!(f, "_"),
            BindPattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta),
            },
            BindPattern::Tuple(elements, _) => format_tuple(elements, f),
            BindPattern::Record(rows, _) => write!(f, "{}", format_record(rows, ":", ", ")),
            BindPattern::UnitLiteral(_) => write!(f, "()"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchPattern<Name: Display, TA: Display, Data> {
    Any(Data),
    Name(SymbolName, Option<TA>, Data),
    Tuple(Vec<MatchPattern<Name, TA, Data>>, Data),
    Literal(Literal, Data),
    Record(Vec<(SymbolName, MatchPattern<Name, TA, Data>)>, Data),
    SumUnwrap(Name, Option<Box<MatchPattern<Name, TA, Data>>>, Data),
}

impl<Name: Display, TA: Display, Data> TreeFormat for MatchPattern<Name, TA, Data> {
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
            MatchPattern::Any(_) => "_".to_string(),
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

impl<Name: Display, TA: Display, Data> Display for MatchPattern<Name, TA, Data> {
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
