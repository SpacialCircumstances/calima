use crate::ast_common::{Pattern, Literal};
use std::fmt::{Display, Formatter};
use crate::types::Type;

#[derive(Copy, Clone, Debug)]
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

#[derive(Debug, Clone)]
pub enum TExprData<'input> {
    Variable(Vec<&'input str>),
    FunctionCall(Box<TExpression<'input>>, Vec<TExpression<'input>>),
    Record(Vec<(&'input str, TExpression<'input>)>),
    Tuple(Vec<TExpression<'input>>),
    List(Vec<TExpression<'input>>),
    Literal(Literal<'input>),
    Lambda(Vec<Pattern<'input, Unit, Unit>>, TBlock<'input>),
    Case(Box<TExpression<'input>>, Vec<(Pattern<'input, Unit, Unit>, TBlock<'input>)>)
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

#[derive(Debug, Clone)]
pub enum TStatement<'input> {
    Do(TExpression<'input>),
    Let(TExpression<'input>, Pattern<'input, Unit, Unit>)
}

#[derive(Debug, Clone)]
pub struct TBlock<'input> {
    pub statements: Vec<TStatement<'input>>,
    pub res: Box<TExpression<'input>>
}