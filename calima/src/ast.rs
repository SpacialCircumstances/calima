pub enum NumberType {
    Integer,
    Float
}

pub enum Literal<'a> {
    String(&'a str),
    Number(&'a str, NumberType)
}

pub enum TypeAnnotation<'a> {
    Name(&'a str),
    Function(Box<TypeAnnotation<'a>>, Box<TypeAnnotation<'a>>),
    Parameterized(Box<TypeAnnotation<'a>>, Vec<TypeAnnotation<'a>>)
}

pub enum Identifier<'a> {
    Simple(&'a str),
    Annotated(&'a str, TypeAnnotation<'a>)
}

pub enum Pattern<'a> {
    Name(Identifier<'a>),
    Tuple(Vec<Pattern<'a>>),
    Literal(Literal<'a>),
    Record(Vec<(&'a str, Pattern<'a>)>),
    Union { constr: &'a str, params: Vec<Pattern<'a>> }
}