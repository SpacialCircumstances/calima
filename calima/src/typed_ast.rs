use crate::ast_common::{Pattern, Literal};
use crate::typechecker::Scheme;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug)]
pub struct Unit(());

impl Display for Unit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
pub struct TExpression<'input>(TExprData<'input>, Scheme);

impl<'input> TExpression<'input> {
    pub fn new(data: TExprData<'input>, scheme: Scheme) -> Self {
        TExpression(data, scheme)
    }

    pub fn data(&self) -> &TExprData {
        &self.0
    }

    pub fn scheme(&self) -> &Scheme {
        &self.1
    }
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