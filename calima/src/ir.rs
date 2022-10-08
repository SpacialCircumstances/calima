use crate::ast::NumberType;
use crate::symbol_names::IText;
use std::collections::HashSet;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constant {
    Unit,
    String(IText),
    Number(IText, NumberType),
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
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct VarRef(pub usize);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Var(VarRef),
    Constant(Constant),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BindTarget {
    Var(VarRef),
    Discard,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binding(pub BindTarget, pub Expr);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block(pub Vec<Binding>, pub Val);
