use crate::ast_common::{BindPattern, Literal, MatchPattern};
use crate::parsing::names::SymbolName;
use crate::types::{Region, Type};
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Unit(());

impl Unit {
    pub fn unit() -> Self {
        Unit(())
    }
}

impl Display for Unit {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        Result::Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TExprData {
    Variable(SymbolName),
    FunctionCall(Box<TExpression>, Vec<TExpression>),
    Record(Vec<(SymbolName, TExpression)>),
    Tuple(Vec<TExpression>),
    List(Vec<TExpression>),
    Literal(Literal),
    Lambda(Vec<BindPattern<Unit, Unit>>, TBlock),
    Case(Box<TExpression>, Vec<(MatchPattern<Unit, Unit>, TBlock)>),
    Ref(Region, Box<TExpression>),
}

#[derive(Debug, Clone)]
pub struct TExpression(TExprData, Type);

impl TExpression {
    pub fn new(data: TExprData, tp: Type) -> Self {
        TExpression(data, tp)
    }

    pub fn data(&self) -> &TExprData {
        &self.0
    }

    pub fn typ(&self) -> &Type {
        &self.1
    }
}

impl PartialEq for TExpression {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TStatement {
    Do(TExpression),
    Let(TExpression, BindPattern<Unit, Unit>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TBlock {
    pub statements: Vec<TStatement>,
    pub res: Box<TExpression>,
}
