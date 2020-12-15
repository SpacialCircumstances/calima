use std::fmt::{Display, Formatter, Debug};
use crate::util::*;
use crate::common::{ModuleIdentifier, OperatorSpecification};
use crate::ast_common::{MatchPattern, Literal, BindPattern};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct RegionVariable<'a, Data>(pub &'a str, pub Data);

impl<'a, Data> Display for RegionVariable<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegionAnnotation<'a, Data> {
    Anonymous,
    Stack,
    Named(&'a str, Data),
    Var(RegionVariable<'a, Data>)
}

impl<'a, Data> Display for RegionAnnotation<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegionAnnotation::Stack => write!(f, "@."),
            RegionAnnotation::Anonymous => write!(f, "@_"),
            RegionAnnotation::Named(name, _) => write!(f, "@{}", name),
            RegionAnnotation::Var(var) => write!(f, "@{}", var)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericTypeKind<'a, Data>(pub &'a str, pub Data);

impl<'a, Data> Display for GenericTypeKind<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation<'a, Data> {
    Name(&'a str, Data),
    Generic(GenericTypeKind<'a, Data>),
    Function(Box<TypeAnnotation<'a, Data>>, Box<TypeAnnotation<'a, Data>>),
    Tuple(Vec<TypeAnnotation<'a, Data>>),
    Parameterized(&'a str, Vec<TypeAnnotation<'a, Data>>),
    Reference(RegionAnnotation<'a, Data>, Box<TypeAnnotation<'a, Data>>)
}

impl<'a, Data> Display for TypeAnnotation<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAnnotation::Name(name, _) => write!(f, "{}", name),
            TypeAnnotation::Generic(name) => write!(f, "{}", name),
            TypeAnnotation::Function(i, o) => write!(f, "({} -> {})", *i, *o),
            TypeAnnotation::Parameterized(name, params) => {
                write!(f, "({} {})", name, format_iter(params.iter(), " "))
            },
            TypeAnnotation::Tuple(elements) => format_tuple(elements, f),
            TypeAnnotation::Reference(reg, tp) => write!(f, "{} {}", reg, tp)
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
pub enum TopLevelStatement<'a, Data> {
    Import(&'a str, Vec<&'a str>, Data),
    Type { name: &'a str, regions: Vec<RegionVariable<'a, Data>>, params: Vec<GenericTypeKind<'a, Data>>, type_def: TypeDefinition<'a, Data>, data: Data }
}

impl<'a, Data> Display for TopLevelStatement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelStatement::Import(module, fields, _) => {
                match fields.is_empty() {
                    true => write!(f, "import {}", module),
                    false => write!(f, "import {}{{{}}}", module, format_iter(fields.iter(), ", "))
                }
            },
            TopLevelStatement::Type { name, regions, params, type_def: typedef, data: _ } => write!(f, "type {} {}{} = {}", name, format_iter_end(regions.iter(), " "), format_iter(params.iter(), " "), typedef)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(Vec<Modifier>, BindPattern<'a, TypeAnnotation<'a, Data>, Data>, Expr<'a, Data>, Data),
    LetOperator(Vec<Modifier>, OperatorSpecification, &'a str, Option<TypeAnnotation<'a, Data>>, Expr<'a, Data>, Data),
    Do(Expr<'a, Data>, Data),
    Region(&'a str, Data)
}

impl<'a, Data> Display for Statement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Region(name, _) => write!(f, "region {}", name),
            Statement::Do(expr, _) => write!(f, "do {}", expr),
            Statement::Let(mods, pat, expr, _) => write!(f, "let {}{} = {}", format_iter_end(mods.iter(), " "), pat, expr),
            Statement::LetOperator(mods, op, name, ta, expr, _) => write!(f, "let {}{} {} = {}", format_iter_end(mods.iter(), " "), op, name, expr)
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
pub enum OperatorElement<'a, Data> {
    Operator(&'a str, Data),
    Expression(Expr<'a, Data>)
}

impl<'a, Data> Display for OperatorElement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorElement::Expression(expr) => write!(f, "{}", expr),
            OperatorElement::Operator(name, _) => write!(f, "{}", name)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a, Data> {
    OperatorAsFunction(&'a str, Data),
    Variable(&'a str, Data),
    FunctionCall(Box<Expr<'a, Data>>, Vec<Expr<'a, Data>>, Data),
    OperatorCall(Vec<OperatorElement<'a, Data>>, Data),
    Record(Vec<(&'a str, Expr<'a, Data>)>, Data),
    Tuple(Vec<Expr<'a, Data>>, Data),
    Literal(Literal<'a>, Data),
    Lambda { params: Vec<BindPattern<'a, TypeAnnotation<'a, Data>, Data>>, body: Block<'a, Data>, data: Data },
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { data: Data, value: Box<Expr<'a, Data>>, matches: Vec<(MatchPattern<'a, TypeAnnotation<'a, Data>, Data>, Block<'a, Data>)> },
    List(Vec<Expr<'a, Data>>, Data),
    Ref(RegionAnnotation<'a, Data>, Box<Expr<'a, Data>>, Data)
}

impl<'a, Data> Expr<'a, Data> {
    pub fn get_location(&self) -> &Data {
        match self {
            Expr::OperatorAsFunction(_, data) => data,
            Expr::Variable(_, data) => data,
            Expr::FunctionCall(_, _, data) => data,
            Expr::OperatorCall(_, data) => data,
            Expr::Record(_, data) => data,
            Expr::Tuple(_, data) => data,
            Expr::Literal(_, data) => data,
            Expr::Lambda { data, .. } => data,
            Expr::If { data, .. } => data,
            Expr::Case { data, .. } => data,
            Expr::List(_, data) => data,
            Expr::Ref(_, _, data) => data
        }
    }
}

impl<'a, Data> Display for Expr<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(lit, _) => write!(f, "{}", lit),
            Expr::OperatorAsFunction(name, _) => write!(f, "`{}`", name),
            Expr::Variable(ident, _) => write!(f, "{}", ident),
            Expr::List(exprs, _) => write!(f, "[{}]", format_iter(exprs.iter(), ", ")),
            Expr::Tuple(exprs, _) => write!(f, "({})", format_iter(exprs.iter(), ", ")),
            Expr::Record(rows, _) => format_record(rows, f, "=", ", "),
            Expr::Lambda { params, body, data: _ } => write!(f, "fun {} -> {}", format_iter(params.iter(), " "), body),
            Expr::FunctionCall(func, args, _) => write!(f, "{} {}", *func, format_iter(args.iter(), " ")),
            Expr::OperatorCall(elements, _) => write!(f, "{}", format_iter(elements.iter(), " ")),
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
            },
            Expr::Ref(reg, expr, _) => write!(f, "{} {}", reg, expr)
        }
    }
}

pub fn find_imported_modules<D: Copy>(ast: &TopLevelBlock<D>) -> Vec<(ModuleIdentifier, D)> {
    ast.top_levels.iter().fold(Vec::new(), |mut imports, statement| {
        match statement {
            TopLevelStatement::Import(module_id, _, data) => {
                imports.push((ModuleIdentifier::from_name(&[module_id]), *data));
            },
            _ => ()
        }
        imports
    })
}