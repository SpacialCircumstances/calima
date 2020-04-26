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

impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(string) => write!(f, "\"{}\"", string),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
            Literal::Number(num, _) => write!(f, "{}", num)
        }
    }
}

fn format_record<T>(elements: &Vec<(&str, T)>, f: &mut Formatter, sep: &str, element_sep: &str) -> std::fmt::Result where T: Display {
    write!(f, "{{ ")?;
    for i in 0..elements.len()-1 {
        let (n, e) = &elements[i];
        write!(f, "{}{} {}{}", n, sep, e, element_sep)?;
    }
    let (ln, le) = elements.last().unwrap();
    write!(f, "{}{} {}", ln, sep, le)?;
    write!(f, "}}")
}

fn format_tuple<T>(elements: &Vec<T>, f: &mut Formatter) -> std::fmt::Result where T: Display {
    write!(f, "(")?;
    for i in 0..elements.len()-1 {
        write!(f, "{}, ", elements[i])?;
    }
    // A tuple always has one element
    write!(f, "{}", elements.last().unwrap())?;
    write!(f, ")")
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeAnnotation<'a> {
    Name(&'a str), //First letter uppercase
    Generic(&'a str), //First letter lowercase
    Function(Box<TypeAnnotation<'a>>, Box<TypeAnnotation<'a>>),
    Parameterized(&'a str, Vec<TypeAnnotation<'a>>),
    Tuple(Vec<TypeAnnotation<'a>>)
}

impl<'a> Display for TypeAnnotation<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAnnotation::Name(name) => write!(f, "{}", name),
            TypeAnnotation::Generic(name) => write!(f, "{}", name),
            TypeAnnotation::Function(i, o) => write!(f, "({} => {})", *i, *o),
            TypeAnnotation::Parameterized(name, params) => {
                write!(f, "({} ", name)?;
                for p in params {
                    write!(f, "{} ", p)?;
                }
                write!(f, ")")
            },
            TypeAnnotation::Tuple(elements) => format_tuple(elements, f)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier<'a> {
    Simple(&'a str),
    Operator(&'a str),
    Annotated(Box<Identifier<'a>>, TypeAnnotation<'a>),
}

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Simple(name) => write!(f, "{}", name),
            Identifier::Operator(op) => write!(f, "({})", op),
            Identifier::Annotated(name, ta) => write!(f, "({}: {})", name, ta)
        }
    }
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

impl<'a> Display for Pattern<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Any => write!(f, "_"),
            Pattern::Name(id) => write!(f, "{}", id),
            Pattern::Literal(lit) => write!(f, "{}", lit),
            Pattern::Tuple(elements) => format_tuple(elements, f),
            Pattern::Record(rows) => format_record(rows, f, ":", ", "),
            Pattern::UnionUnwrap(constr, pat) => write!(f, "({} {})", constr, *pat)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<'a> {
    Alias(TypeAnnotation<'a>),
    Record(Vec<(&'a str, TypeAnnotation<'a>)>),
    Union(Vec<(&'a str, TypeAnnotation<'a>)>)
}

impl<'a> Display for TypeDefinition<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDefinition::Alias(ta) => write!(f, "{}", ta),
            TypeDefinition::Record(rows) => format_record(rows, f, ":", ", "),
            TypeDefinition::Union(rows) => format_record(rows, f, " of", " | ")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Modifier {
    Rec
}

impl Display for Modifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Modifier::Rec => write!(f, "rec")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(Vec<Modifier>, Pattern<'a>, Expr<'a, Data>, Data),
    Do(Expr<'a, Data>, Data),
    Import(&'a str, Data), //TODO: Support complex imports
    Region(&'a str, Data),
    Type(&'a str, TypeDefinition<'a>, Data)
}

impl<'a, Data: Display> Display for Statement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Import(i, _) => write!(f, "import {}", i),
            Statement::Region(reg, _) => write!(f, "region {}", reg),
            Statement::Type(tn, td, _) => write!(f, "type {} = {}", tn, td),
            Statement::Do(expr, _) => write!(f, "do {}", expr),
            Statement::Let(mods, pat, expr, _) => {
                let mods = mods.iter().map(|m| m.to_string()).collect::<Vec<String>>().join(" ");
                write!(f, "let {} {} = {}", mods, pat, expr)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a, Data> {
    pub statements: Vec<Statement<'a, Data>>,
    pub result: Box<Expr<'a, Data>>
}

impl<'a, Data: Display> Display for Block<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.statements.is_empty() {
            write!(f, "{}", self.result)
        } else {
            for st in &self.statements {
                writeln!(f, "{}", st)?;
            }
            writeln!(f, "in {}", *self.result)
        }
    }
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

impl<'a, Data: Display> Display for Expr<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(lit, _) => write!(f, "{}", lit),
            Expr::Variable(ident, _) => write!(f, "{}", ident.join(".")),
            Expr::List(exprs, _) => write!(f, "[{}]", exprs.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(",")),
            Expr::Tuple(exprs, _) => write!(f, "({})", exprs.iter().map(|e| e.to_string()).collect::<Vec<String>>().join(",")),
            _ => unimplemented!()
        }
    }
}