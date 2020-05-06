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

fn format_iter<T: Display, I: Iterator<Item=T>>(iter: I, sep: &str) -> String {
    iter.map(|e| e.to_string()).collect::<Vec<String>>().join(sep)
}

fn format_iter_end<T: Display, I: Iterator<Item=T>>(iter: I, sep: &str) -> String {
    let mut str = format_iter(iter, sep);
    if str.len() != 0 {
        str.push_str(sep);
    }
    str
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct GenericRegion<'a>(pub &'a str);

impl<'a> Display for GenericRegion<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct NamedRegion<'a>(pub &'a str);

impl<'a> Display for NamedRegion<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegionAnnotation<'a> {
    Named(NamedRegion<'a>),
    Generic(GenericRegion<'a>)
}

impl<'a> Display for RegionAnnotation<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegionAnnotation::Named(r) => write!(f, "{}", r),
            RegionAnnotation::Generic(r) => write!(f, "{}", r)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation<'a>(pub Option<RegionAnnotation<'a>>, pub TypeKind<'a>);

impl<'a> Display for TypeAnnotation<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            None => write!(f, "{}", self.1),
            Some(r) => write!(f, "{} {}", r, self.1)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind<'a> {
    Name(&'a str),
    Generic(&'a str),
    Function(Box<TypeAnnotation<'a>>, Box<TypeAnnotation<'a>>),
    Tuple(Vec<TypeAnnotation<'a>>),
    Parameterized(&'a str, Vec<TypeAnnotation<'a>>)
}

impl<'a> Display for TypeKind<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Name(name) => write!(f, "{}", name),
            TypeKind::Generic(name) => write!(f, "{}", name),
            TypeKind::Function(i, o) => write!(f, "({} => {})", *i, *o),
            TypeKind::Parameterized(name, params) => {
                write!(f, "({} ", name)?;
                for p in params {
                    write!(f, "{} ", p)?;
                }
                write!(f, ")")
            },
            TypeKind::Tuple(elements) => format_tuple(elements, f)
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
    SumUnwrap(&'a str, Option<Box<Pattern<'a>>>),
}

impl<'a> Display for Pattern<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Any => write!(f, "_"),
            Pattern::Name(id) => write!(f, "{}", id),
            Pattern::Literal(lit) => write!(f, "{}", lit),
            Pattern::Tuple(elements) => format_tuple(elements, f),
            Pattern::Record(rows) => format_record(rows, f, ":", ", "),
            Pattern::SumUnwrap(constr, None) => write!(f, "{}", constr),
            Pattern::SumUnwrap(constr, Some(pat)) => write!(f, "{} {}", constr, *pat)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<'a> {
    Alias(TypeAnnotation<'a>),
    Record(Vec<(&'a str, TypeAnnotation<'a>)>),
    Sum(Vec<(&'a str, Option<TypeAnnotation<'a>>)>)
}

impl<'a> Display for TypeDefinition<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDefinition::Alias(ta) => write!(f, "{}", ta),
            TypeDefinition::Record(rows) => format_record(rows, f, ":", ", "),
            TypeDefinition::Sum(rows) => {
                let str = rows.iter().map(|(constr, ta)| match ta {
                    None => format!("{}", constr),
                    Some(ta) => format!("{} of {}", constr, ta)
                }).collect::<Vec<String>>().join("| ");
                write!(f, "{}", str)
            }
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
pub struct ClassDefinition<'a>(pub Vec<(&'a str, TypeAnnotation<'a>)>);

impl<'a> Display for ClassDefinition<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let members = self.0.iter().map(|(n, ta)| format!("{}: {}", n, ta));
        let mut members = format_iter(members, ",\n");
        members.push('\n');
        writeln!(f, "end")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InstanceDefinition<'a, Data>(pub Vec<(&'a str, Expr<'a, Data>)>);

impl<'a, Data> Display for InstanceDefinition<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let members = self.0.iter().map(|(n, expr)| format!("{} = {}", n, expr));
        let mut members = format_iter(members, ",\n");
        members.push('\n');
        writeln!(f, "end")
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelStatement<'a, Data> {
    Import(&'a str, Data), //TODO: Support complex imports
    Type { name: &'a str, regions: Vec<GenericRegion<'a>>, params: Vec<&'a str>, type_def: TypeDefinition<'a>, data: Data },
    Class { name: &'a str, regions: Vec<GenericRegion<'a>>, params: Vec<&'a str>, class_def: ClassDefinition<'a>, data: Data },
    Instance { name: &'a str, regions: Vec<RegionAnnotation<'a>>, args: Vec<TypeAnnotation<'a>>, instance_def: InstanceDefinition<'a, Data>, data: Data }
}

impl<'a, Data> Display for TopLevelStatement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelStatement::Import(i, _) => write!(f, "import {}", i),
            TopLevelStatement::Type { name, regions, params, type_def: typedef, data: _ } => write!(f, "type {} {}{} = {}", name, format_iter_end(regions.iter(), " "), format_iter(params.iter(), " "), typedef),
            TopLevelStatement::Class { name, regions, params, class_def: classdef, data: _ } => {
                writeln!(f, "class {} {}{}=", name, format_iter_end(regions.iter(), " "), format_iter_end(params.iter(), " "))?;
                write!(f, "{}", classdef)
            },
            TopLevelStatement::Instance { name, regions, args, instance_def: instancedef, data: _ } => {
                writeln!(f, "instance {} {}{}=", name, format_iter_end(regions.iter(), " "), format_iter_end(args.iter(), " "))?;
                write!(f, "{}", instancedef)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(Vec<Modifier>, Option<RegionAnnotation<'a>>, Pattern<'a>, Expr<'a, Data>, Data),
    Do(Option<RegionAnnotation<'a>>, Expr<'a, Data>, Data),
    Region(NamedRegion<'a>, Data)
}

impl<'a, Data> Display for Statement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Region(reg, _) => write!(f, "region {}", reg),
            Statement::Do(Some(reg), expr, _) => write!(f, "do {} {}", reg, expr),
            Statement::Do(None, expr, _) => write!(f, "do {}", expr),
            Statement::Let(mods, reg, pat, expr, _) => {
                match reg {
                    None => write!(f, "let {}{} = {}", format_iter_end(mods.iter(), " "), pat, expr),
                    Some(region) => write!(f, "let {}{} {} = {}", format_iter_end(mods.iter(), " "), region, pat, expr)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TopLevelBlock<'a, Data> {
    pub top_levels: Vec<TopLevelStatement<'a, Data>>,
    pub block: Block<'a, Data>
}

impl<'a, Data> Display for TopLevelBlock<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for tls in &self.top_levels {
            writeln!(f, "{}", tls)?;
        }
        write!(f, "{}", self.block)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<'a, Data> {
    pub statements: Vec<Statement<'a, Data>>,
    pub result: Box<Expr<'a, Data>>
}

impl<'a, Data> Display for Block<'a, Data> {
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
    Lambda { regions: Vec<GenericRegion<'a>>, params: Vec<Pattern<'a>>, body: Block<'a, Data>, data: Data },
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { data: Data, value: Box<Expr<'a, Data>>, matches: Vec<(Pattern<'a>, Block<'a, Data>)> },
    List(Vec<Expr<'a, Data>>, Data)
}

impl<'a, Data> Display for Expr<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(lit, _) => write!(f, "{}", lit),
            Expr::Variable(ident, _) => write!(f, "{}", ident.join(".")),
            Expr::List(exprs, _) => write!(f, "[{}]", format_iter(exprs.iter(), ", ")),
            Expr::Tuple(exprs, _) => write!(f, "({})", format_iter(exprs.iter(), ", ")),
            Expr::Record(rows, _) => format_record(rows, f, "=", ", "),
            Expr::Lambda { regions, params, body, data: _ } => write!(f, "fun {}{} -> {}", format_iter_end(regions.iter(), " "), format_iter(params.iter(), " "), body),
            Expr::FunctionCall(func, args, _) => write!(f, "{} {}", *func, format_iter(args.iter(), " ")),
            Expr::OperatorCall(l, op, r, _) => write!(f, "{} {} {}", *l, op, *r),
            Expr::If { data: _, cond, if_true, if_false } => {
                writeln!(f, "if {} then", *cond)?;
                writeln!(f, "{}", *if_true)?;
                writeln!(f, "else")?;
                writeln!(f, "{}", *if_false)?;
                write!(f, "end")
            },
            Expr::Case { data: _, value, matches } => {
                writeln!(f, "case {} of", *value)?;

                for (pat, block) in matches {
                    writeln!(f, "| {} ->", pat)?;
                    writeln!(f, "{}", block)?;
                }

                write!(f, "end")
            }
        }
    }
}