use std::fmt::{Display, Formatter, Debug};

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

#[derive(Debug, PartialEq, Clone)]
pub enum TypeAnnotation<'a> {
    Name(&'a str), //First letter uppercase
    Generic(&'a str), //First letter lowercase
    Function(Box<TypeAnnotation<'a>>, Box<TypeAnnotation<'a>>),
    Parameterized(&'a str, Vec<TypeAnnotation<'a>>),
    Tuple(Vec<TypeAnnotation<'a>>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier<'a> {
    Simple(&'a str),
    Annotated(&'a str, TypeAnnotation<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<'a> {
    Any,
    Name(Identifier<'a>),
    Tuple(Vec<Pattern<'a>>),
    Literal(Literal<'a>),
    Record(Vec<(&'a str, Pattern<'a>)>),
    UnionUnwrap(&'a str, Box<Pattern<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<'a> {
    Alias(TypeAnnotation<'a>),
    Record(Vec<(&'a str, TypeAnnotation<'a>)>),
    Union(Vec<(&'a str, TypeAnnotation<'a>)>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Modifier {
    Rec
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(Vec<Modifier>, Pattern<'a>, Expr<'a, Data>, Data),
    Do(Expr<'a, Data>, Data),
    Import(&'a str, Data), //TODO: Support complex imports
    Region(&'a str, Data),
    Type(&'a str, TypeDefinition<'a>, Data)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a, Data> {
    pub statements: Vec<Statement<'a, Data>>,
    pub result: Box<Expr<'a, Data>>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a, Data> {
    Variable(Vec<&'a str>, Data),
    FunctionCall(Box<Expr<'a, Data>>, Vec<Expr<'a, Data>>, Data),
    OperatorCall(Box<Expr<'a, Data>>, &'a str, Box<Expr<'a, Data>>, Data),
    Record(Vec<(&'a str, Expr<'a, Data>)>, Data),
    Tuple(Vec<Expr<'a, Data>>, Data),
    Literal(Literal<'a>, Data),
    Lambda(Vec<Pattern<'a>>, Block<'a, Data>, Data),
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { data: Data, value: Box<Expr<'a, Data>>, matches: Vec<(Pattern<'a>, Block<'a, Data>)> },
    List(Vec<Expr<'a, Data>>, Data)
}

pub trait AstFormatter {
    fn format<Data>(program: &Block<Data>) -> String where Data: Display, Data: Debug;
}

pub struct DebugFormatter;

impl AstFormatter for DebugFormatter {
    fn format<Data>(program: &Block<Data>) -> String where Data: Display, Data: Debug {
        format!("{:#?}", program)
    }
}

pub struct SourceCodeFormatter;

struct SourceCodeFormatterAstWrapper<'a, Data>(&'a Block<'a, Data>) where Data: Display, Data: Debug;

impl<'a, Data> SourceCodeFormatterAstWrapper<'a, Data> where Data: Display, Data: Debug {
    fn format_statement(statement: &Statement<Data>, formatter: &mut Formatter) -> std::fmt::Result {
        write!(formatter, "")
    }

    fn format_expression(expr: &Expr<Data>, formatter: &mut Formatter) -> std::fmt::Result {
        write!(formatter, "")
    }

    fn format_block(block: &'a Block<'a, Data>, formatter: &mut Formatter) -> std::fmt::Result {
        for statement in &block.statements {
            Self::format_statement(statement, formatter)?;
        }
        Self::format_expression(&block.result, formatter)
    }
}

impl<'a, Data> Display for SourceCodeFormatterAstWrapper<'a, Data> where Data: Display, Data: Debug {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::format_block(self.0, f)
    }
}

impl AstFormatter for SourceCodeFormatter {
    fn format<Data>(program: &Block<Data>) -> String where Data: Display, Data: Debug {
        let wrapper = SourceCodeFormatterAstWrapper(program);
        format!("{}", wrapper)
    }
}