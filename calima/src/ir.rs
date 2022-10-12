use crate::ast::NumberType;
use crate::formatting::{format_iter, format_iter_end};
use crate::symbol_names::IText;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constant {
    Unit,
    String(IText),
    Number(IText, NumberType),
}

impl Display for Constant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Unit => write!(f, "()"),
            Constant::String(t) => write!(f, "\"{}\"", t),
            Constant::Number(t, _) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    List(Vec<Val>),
    FunctionCall {
        func: Val,
        args: Vec<Val>,
    },
    Lambda {
        params: Vec<BindTarget>,
        block: Block,
    },
    If {
        condition: Val,
        if_true: Block,
        if_false: Block,
    },
    Generalize(Val),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::List(vals) => write!(f, "[{}]", format_iter(vals.iter(), ", ")),
            Expr::FunctionCall { func, args } => {
                write!(f, "{} {}", func, format_iter(args.iter(), " "))
            }
            Expr::Lambda { params, block } => {
                write!(f, "fun {} -> {}", format_iter(params.iter(), " "), block)
            }
            Expr::If {
                condition,
                if_true,
                if_false,
            } => write!(f, "if {} then {} else {} end", condition, if_true, if_false),
            Expr::Generalize(v) => write!(f, "$generalize {}", v),
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct VarRef(pub usize);

impl Display for VarRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Var(VarRef),
    Constant(Constant),
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Var(v) => write!(f, "{}", v),
            Val::Constant(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BindTarget {
    Var(VarRef),
    Discard,
}

impl Display for BindTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BindTarget::Var(v) => write!(f, "{}", v),
            BindTarget::Discard => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binding(pub BindTarget, pub Expr);

impl Display for Binding {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.0, self.1)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block(pub Vec<Binding>, pub Val);

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{\n{}{}\n}}",
            format_iter_end(self.0.iter(), ";\n"),
            self.1
        )
    }
}
