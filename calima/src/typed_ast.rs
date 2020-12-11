use crate::ast_common::{MatchPattern, Literal, BindPattern};
use std::fmt::{Display, Formatter};
use crate::types::{Type, Region};

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
pub enum TExprData<'input> {
    Variable(&'input str),
    FunctionCall(Box<TExpression<'input>>, Vec<TExpression<'input>>),
    Record(Vec<(&'input str, TExpression<'input>)>),
    Tuple(Vec<TExpression<'input>>),
    List(Vec<TExpression<'input>>),
    Literal(Literal<'input>),
    Lambda(Vec<BindPattern<'input, Unit, Unit>>, TBlock<'input>),
    Case(Box<TExpression<'input>>, Vec<(MatchPattern<'input, Unit, Unit>, TBlock<'input>)>),
    Ref(Region, Box<TExpression<'input>>)
}

#[derive(Debug, Clone)]
pub struct TExpression<'input>(TExprData<'input>, Type);

impl<'input> TExpression<'input> {
    pub fn new(data: TExprData<'input>, tp: Type) -> Self {
        TExpression(data, tp)
    }

    pub fn data(&self) -> &TExprData {
        &self.0
    }

    pub fn typ(&self) -> &Type { &self.1 }
}

impl<'a> PartialEq for TExpression<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TStatement<'input> {
    Do(TExpression<'input>),
    Let(TExpression<'input>, BindPattern<'input, Unit, Unit>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct TBlock<'input> {
    pub statements: Vec<TStatement<'input>>,
    pub res: Box<TExpression<'input>>
}