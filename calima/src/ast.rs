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
pub struct GenericRegion<'a, Data>(pub &'a str, pub Data);

impl<'a, Data> Display for GenericRegion<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct NamedRegion<'a, Data>(pub &'a str, pub Data);

impl<'a, Data> Display for NamedRegion<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegionAnnotation<'a, Data> {
    Named(NamedRegion<'a, Data>),
    Generic(GenericRegion<'a, Data>)
}

impl<'a, Data> Display for RegionAnnotation<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegionAnnotation::Named(r) => write!(f, "{}", r),
            RegionAnnotation::Generic(r) => write!(f, "{}", r)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation<'a, Data>(pub Option<RegionAnnotation<'a, Data>>, pub TypeKind<'a, Data>, pub Data);

impl<'a, Data> Display for TypeAnnotation<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            None => write!(f, "{}", self.1),
            Some(r) => write!(f, "{} {}", r, self.1)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind<'a, Data> {
    Name(Vec<&'a str>, Data),
    Generic(&'a str, Data),
    Function(Box<TypeAnnotation<'a, Data>>, Box<TypeAnnotation<'a, Data>>),
    Tuple(Vec<TypeAnnotation<'a, Data>>),
    Parameterized(Vec<&'a str>, Vec<TypeAnnotation<'a, Data>>)
}

impl<'a, Data> Display for TypeKind<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::Name(name, _) => write!(f, "{}", format_iter(name.iter(), " and ")),
            TypeKind::Generic(name, _) => write!(f, "{}", name),
            TypeKind::Function(i, o) => write!(f, "({} -> {})", *i, *o),
            TypeKind::Parameterized(name, params) => {
                write!(f, "({} {})", format_iter(name.iter(), " and "), format_iter(params.iter(), " "))
            },
            TypeKind::Tuple(elements) => format_tuple(elements, f)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Identifier<'a> {
    Simple(&'a str),
    Operator(&'a str)
}

impl<'a> Display for Identifier<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Simple(name) => write!(f, "{}", name),
            Identifier::Operator(op) => write!(f, "({})", op)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern<'a, Data> {
    Any(Data),
    Name(Identifier<'a>, Option<TypeAnnotation<'a, Data>>, Data),
    Tuple(Vec<Pattern<'a, Data>>, Data),
    Literal(Literal<'a>, Data),
    Record(Vec<(&'a str, Pattern<'a, Data>)>, Data),
    SumUnwrap(&'a str, Option<Box<Pattern<'a, Data>>>, Data),
}

impl<'a, Data> Display for Pattern<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Any(_) => write!(f, "_"),
            Pattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta)
            },
            Pattern::Literal(lit, _) => write!(f, "{}", lit),
            Pattern::Tuple(elements, _) => format_tuple(elements, f),
            Pattern::Record(rows, _) => format_record(rows, f, ":", ", "),
            Pattern::SumUnwrap(constr, None, _) => write!(f, "{}", constr),
            Pattern::SumUnwrap(constr, Some(pat), _) => write!(f, "{} {}", constr, *pat)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<'a, Data> {
    Alias(TypeAnnotation<'a, Data>),
    Record(Vec<(&'a str, TypeAnnotation<'a, Data>)>),
    Sum(Vec<(&'a str, Option<TypeAnnotation<'a, Data>>)>)
}

impl<'a, Data> Display for TypeDefinition<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDefinition::Alias(ta) => write!(f, "{}", ta),
            TypeDefinition::Record(rows) => format_record(rows, f, ":", ", "),
            TypeDefinition::Sum(rows) => {
                let str = rows.iter().map(|(constr, ta)| match ta {
                    None => format!("{}", constr),
                    Some(ta) => format!("{} {}", constr, ta)
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
pub struct ClassDefinition<'a, Data>(pub Vec<(Identifier<'a>, TypeAnnotation<'a, Data>)>);

impl<'a, Data> Display for ClassDefinition<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let members = self.0.iter().map(|(n, ta)| format!("{}: {}", n, ta));
        let members = format_iter(members, ",\n");
        write!(f, "{}", members)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InstanceDefinition<'a, Data>(pub Vec<(Identifier<'a>, Expr<'a, Data>)>);

impl<'a, Data> Display for InstanceDefinition<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let members = self.0.iter().map(|(n, expr)| format!("{} = {}", n, expr));
        let members = format_iter(members, ",\n");
        write!(f, "{}", members)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelStatement<'a, Data> {
    Import(Vec<&'a str>, Vec<Identifier<'a>>, Data),
    Type { name: &'a str, regions: Vec<GenericRegion<'a, Data>>, params: Vec<&'a str>, type_def: TypeDefinition<'a, Data>, data: Data },
    Class { name: &'a str, regions: Vec<GenericRegion<'a, Data>>, params: Vec<&'a str>, class_def: ClassDefinition<'a, Data>, data: Data },
    Instance { name: &'a str, regions: Vec<RegionAnnotation<'a, Data>>, args: Vec<TypeAnnotation<'a, Data>>, instance_def: InstanceDefinition<'a, Data>, data: Data }
}

impl<'a, Data> Display for TopLevelStatement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelStatement::Import(module, fields, _) => {
                match fields.is_empty() {
                    true => write!(f, "import {}", format_iter(module.iter(), ".")),
                    false => write!(f, "import {}{{{}}}", format_iter(module.iter(), "."), format_iter(fields.iter(), ", "))
                }
            },
            TopLevelStatement::Type { name, regions, params, type_def: typedef, data: _ } => write!(f, "type {} {}{} = {}", name, format_iter_end(regions.iter(), " "), format_iter(params.iter(), " "), typedef),
            TopLevelStatement::Class { name, regions, params, class_def: classdef, data: _ } => {
                writeln!(f, "class {} {}{}=", name, format_iter_end(regions.iter(), " "), format_iter_end(params.iter(), " "))?;
                writeln!(f, "{}", classdef)?;
                write!(f, "end")
            },
            TopLevelStatement::Instance { name, regions, args, instance_def: instancedef, data: _ } => {
                writeln!(f, "instance {} {}{}=", name, format_iter_end(regions.iter(), " "), format_iter_end(args.iter(), " "))?;
                writeln!(f, "{}", instancedef)?;
                write!(f, "end")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(Vec<Modifier>, Option<RegionAnnotation<'a, Data>>, Pattern<'a, Data>, Expr<'a, Data>, Data),
    Do(Option<RegionAnnotation<'a, Data>>, Expr<'a, Data>, Data),
    Region(NamedRegion<'a, Data>, Data)
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
        if self.top_levels.is_empty() {
            write!(f, "{}", self.block)
        } else {
            for tls in &self.top_levels {
                writeln!(f, "{}", tls)?;
            }
            writeln!(f, "in")?;
            write!(f, "{}", self.block)
        }
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
    Lambda { regions: Vec<GenericRegion<'a, Data>>, params: Vec<Pattern<'a, Data>>, body: Block<'a, Data>, data: Data },
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { data: Data, value: Box<Expr<'a, Data>>, matches: Vec<(Pattern<'a, Data>, Block<'a, Data>)> },
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