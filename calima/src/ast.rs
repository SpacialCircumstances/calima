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
pub enum Statement<'a, Data> {
    Let(Pattern<'a>, Expr<'a, Data>, Data),
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
    RecordConstruction(Vec<(&'a str, Expr<'a, Data>)>, Data),
    Tuple(Vec<Expr<'a, Data>>),
    Literal(Literal<'a>, Data),
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { value: Box<Expr<'a, Data>>, matches: Vec<(Pattern<'a>, Block<'a, Data>)> },
    ListExpression(Vec<Expr<'a, Data>>)
}