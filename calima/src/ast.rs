pub enum NumberType {
    Integer,
    Float,
}

pub enum Literal<'a> {
    String(&'a str),
    Number(&'a str, NumberType),
    Unit
}

pub enum TypeAnnotation<'a> {
    Name(&'a str),
    //First letter uppercase
    Generic(&'a str),
    //First letter lowercase
    Function(Box<TypeAnnotation<'a>>, Box<TypeAnnotation<'a>>),
    Parameterized(Box<TypeAnnotation<'a>>, Vec<TypeAnnotation<'a>>),
}

pub enum Identifier<'a> {
    Simple(&'a str),
    Annotated(&'a str, TypeAnnotation<'a>),
}

pub enum Pattern<'a> {
    Name(Identifier<'a>),
    Tuple(Vec<Pattern<'a>>),
    Literal(Literal<'a>),
    Record(Vec<(&'a str, Pattern<'a>)>),
    Union { constr: &'a str, params: Vec<Pattern<'a>> },
}

pub enum Statement<'a, Data> {
    Let(Pattern<'a>, Expr<'a, Data>, Data),
    Do(Expr<'a, Data>, Data),
    Import(&'a str) //TODO: Support complex imports
}

pub struct Block<'a, Data> {
    pub statements: Vec<Statement<'a, Data>>,
    pub result: Box<Expr<'a, Data>>
}

pub enum Expr<'a, Data> {
    Variable(Vec<&'a str>, Data),
    FunctionCall(Box<Expr<'a, Data>>, Vec<Expr<'a, Data>>, Data),
    OperatorCall(Box<Expr<'a, Data>>, &'a str, Box<Expr<'a, Data>>, Data),
    RecordConstruction(Vec<(&'a str, Expr<'a, Data>)>, Data),
    Literal(Literal<'a>),
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { value: Box<Expr<'a, Data>>, matches: Vec<(Pattern<'a>, Block<'a, Data>)> },
    ListExpression(Vec<Expr<'a, Data>>)
}