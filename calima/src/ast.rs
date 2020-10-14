use std::fmt::{Display, Formatter, Debug};
use crate::util::*;
use crate::common::ModuleIdentifier;
use crate::ast_common::{Identifier, MatchPattern, Literal, BindPattern};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegionAnnotation<'a, Data> {
    Anonymous,
    Stack,
    Named(&'a str, Data)
}

impl<'a, Data> Display for RegionAnnotation<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegionAnnotation::Stack => write!(f, "@."),
            RegionAnnotation::Anonymous => write!(f, "@_"),
            RegionAnnotation::Named(name, _) => write!(f, "@{}", name)
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
    Import(Vec<&'a str>, Vec<Identifier<'a, Data>>, Data),
    Type { name: &'a str, regions: Vec<RegionAnnotation<'a, Data>>, params: Vec<TypeAnnotation<'a, Data>>, type_def: TypeDefinition<'a, Data>, data: Data }
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
            TopLevelStatement::Type { name, regions, params, type_def: typedef, data: _ } => write!(f, "type {} {}{} = {}", name, format_iter_end(regions.iter(), " "), format_iter(params.iter(), " "), typedef)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(Vec<Modifier>, Option<RegionAnnotation<'a, Data>>, BindPattern<'a, TypeAnnotation<'a, Data>, Data>, Expr<'a, Data>, Data),
    Do(Option<RegionAnnotation<'a, Data>>, Expr<'a, Data>, Data),
    Region(RegionAnnotation<'a, Data>, Data)
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
pub enum Operator<'a, Data> {
    Name(&'a str),
    FunctionAsOperator(Box<Expr<'a, Data>>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a, Data> {
    OperatorAsFunction(&'a str, Data),
    Variable(Vec<&'a str>, Data),
    FunctionCall(Box<Expr<'a, Data>>, Vec<Expr<'a, Data>>, Data),
    OperatorCall(Vec<Expr<'a, Data>>, Vec<&'a str>, Data),
    UnaryOperatorCall(&'a str, Box<Expr<'a, Data>>, Data),
    Record(Vec<(&'a str, Expr<'a, Data>)>, Data),
    Tuple(Vec<Expr<'a, Data>>, Data),
    Literal(Literal<'a>, Data),
    Lambda { regions: Vec<RegionAnnotation<'a, Data>>, params: Vec<BindPattern<'a, TypeAnnotation<'a, Data>, Data>>, body: Block<'a, Data>, data: Data },
    If { data: Data, cond: Box<Expr<'a, Data>>, if_true: Block<'a, Data>, if_false: Block<'a, Data> },
    Case { data: Data, value: Box<Expr<'a, Data>>, matches: Vec<(MatchPattern<'a, TypeAnnotation<'a, Data>, Data>, Block<'a, Data>)> },
    List(Vec<Expr<'a, Data>>, Data)
}

impl<'a, Data> Display for Expr<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(lit, _) => write!(f, "{}", lit),
            Expr::OperatorAsFunction(name, _) => write!(f, "`{}`", name),
            Expr::Variable(ident, _) => write!(f, "{}", ident.join(".")),
            Expr::List(exprs, _) => write!(f, "[{}]", format_iter(exprs.iter(), ", ")),
            Expr::Tuple(exprs, _) => write!(f, "({})", format_iter(exprs.iter(), ", ")),
            Expr::Record(rows, _) => format_record(rows, f, "=", ", "),
            Expr::Lambda { regions, params, body, data: _ } => write!(f, "fun {}{} -> {}", format_iter_end(regions.iter(), " "), format_iter(params.iter(), " "), body),
            Expr::FunctionCall(func, args, _) => write!(f, "{} {}", *func, format_iter(args.iter(), " ")),
            Expr::UnaryOperatorCall(op, expr, _) => write!(f, "{}{}", op, expr),
            Expr::OperatorCall(exprs, ops, _) => {
                for (e, op) in exprs.iter().zip(ops.iter()) {
                    write!(f, "{} {} ", e, op)?;
                }

                write!(f, "{}", exprs.last().unwrap())
            },
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

pub fn find_imported_modules<D: Copy>(ast: &TopLevelBlock<D>) -> Vec<(ModuleIdentifier, D)> {
    ast.top_levels.iter().fold(Vec::new(), |mut imports, statement| {
        match statement {
            TopLevelStatement::Import(module_id, _, data) => {
                imports.push((ModuleIdentifier::from_name(module_id), *data));
            },
            _ => ()
        }
        imports
    })
}