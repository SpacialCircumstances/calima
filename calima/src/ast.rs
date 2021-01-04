use crate::ast_common::{BindPattern, Literal, MatchPattern};
use crate::common::{ModuleIdentifier, OperatorSpecification};
use crate::formatting::tree::{format_children, TreeFormat};
use crate::formatting::*;
use std::fmt::{Debug, Display, Formatter};

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
    Var(RegionVariable<'a, Data>),
}

impl<'a, Data> Display for RegionAnnotation<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegionAnnotation::Stack => write!(f, "@."),
            RegionAnnotation::Anonymous => write!(f, "@_"),
            RegionAnnotation::Named(name, _) => write!(f, "@{}", name),
            RegionAnnotation::Var(var) => write!(f, "@{}", var),
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
    Reference(RegionAnnotation<'a, Data>, Box<TypeAnnotation<'a, Data>>),
}

impl<'a, Data> TreeFormat for TypeAnnotation<'a, Data> {
    fn get_precedence(&self) -> i32 {
        match self {
            TypeAnnotation::Name(_, _) => 0,
            TypeAnnotation::Generic(_) => 0,
            TypeAnnotation::Tuple(_) => 0,
            TypeAnnotation::Function(_, _) => 1,
            TypeAnnotation::Parameterized(_, _) => 1,
            TypeAnnotation::Reference(_, _) => 2,
        }
    }

    fn format(&self) -> String {
        match self {
            TypeAnnotation::Name(name, _) => format!("{}", name),
            TypeAnnotation::Generic(name) => format!("{}", name),
            TypeAnnotation::Function(i, o) => {
                format!("({} -> {})", self.format_child(&*i), self.format_child(&*o))
            }
            TypeAnnotation::Parameterized(name, params) => {
                format!("({} {})", name, format_children(self, params.iter(), " "))
            }
            TypeAnnotation::Tuple(elements) => {
                format!("({})", format_children(self, elements.iter(), ", "))
            }
            TypeAnnotation::Reference(reg, tp) => format!("{} {}", reg, self.format_child(&*tp)),
        }
    }
}

impl<'a, Data> Display for TypeAnnotation<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<'a, Data> {
    Alias(TypeAnnotation<'a, Data>),
    Record(Vec<(&'a str, TypeAnnotation<'a, Data>)>),
    Sum(Vec<(&'a str, Option<TypeAnnotation<'a, Data>>)>),
}

impl<'a, Data> Display for TypeDefinition<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDefinition::Alias(ta) => write!(f, "{}", ta),
            TypeDefinition::Record(rows) => write!(f, "{}", format_record(rows, ":", ", ")),
            TypeDefinition::Sum(rows) => {
                let str = rows
                    .iter()
                    .map(|(constr, ta)| match ta {
                        None => format!("{}", constr),
                        Some(ta) => format!("{} {}", constr, ta),
                    })
                    .collect::<Vec<String>>()
                    .join("| ");
                write!(f, "{}", str)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Modifier {
    Rec,
}

impl Display for Modifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Modifier::Rec => write!(f, "rec"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelStatement<'a, Data> {
    Import(&'a str, Vec<&'a str>, Data),
    Type {
        name: &'a str,
        regions: Vec<RegionVariable<'a, Data>>,
        params: Vec<GenericTypeKind<'a, Data>>,
        type_def: TypeDefinition<'a, Data>,
        data: Data,
    },
}

impl<'a, Data> Display for TopLevelStatement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelStatement::Import(module, fields, _) => match fields.is_empty() {
                true => write!(f, "import {}", module),
                false => write!(
                    f,
                    "import {}{{{}}}",
                    module,
                    format_iter(fields.iter(), ", ")
                ),
            },
            TopLevelStatement::Type {
                name,
                regions,
                params,
                type_def: typedef,
                data: _,
            } => write!(
                f,
                "type {} {}{} = {}",
                name,
                format_iter_end(regions.iter(), " "),
                format_iter(params.iter(), " "),
                typedef
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a, Data> {
    Let(
        Vec<Modifier>,
        BindPattern<'a, TypeAnnotation<'a, Data>, Data>,
        Expr<'a, Data>,
        Data,
    ),
    LetOperator(
        Vec<Modifier>,
        OperatorSpecification,
        &'a str,
        Option<TypeAnnotation<'a, Data>>,
        Expr<'a, Data>,
        Data,
    ),
    Do(Expr<'a, Data>, Data),
    Region(&'a str, Data),
}

impl<'a, Data> Display for Statement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Region(name, _) => write!(f, "region {}", name),
            Statement::Do(expr, _) => write!(f, "do {}", expr),
            Statement::Let(mods, pat, expr, _) => write!(
                f,
                "let {}{} = {}",
                format_iter_end(mods.iter(), " "),
                pat,
                expr
            ),
            Statement::LetOperator(mods, op, name, ta, expr, _) => write!(
                f,
                "let {}{} {} = {}",
                format_iter_end(mods.iter(), " "),
                op,
                name,
                expr
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TopLevelBlock<'a, Data> {
    pub top_levels: Vec<TopLevelStatement<'a, Data>>,
    pub block: Block<'a, Data>,
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
    pub result: Box<Expr<'a, Data>>,
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
    Expression(Expr<'a, Data>),
}

impl<'a, Data> Display for OperatorElement<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorElement::Expression(expr) => write!(f, "{}", expr),
            OperatorElement::Operator(name, _) => write!(f, "{}", name),
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
    Lambda {
        params: Vec<BindPattern<'a, TypeAnnotation<'a, Data>, Data>>,
        body: Block<'a, Data>,
        data: Data,
    },
    If {
        data: Data,
        cond: Box<Expr<'a, Data>>,
        if_true: Block<'a, Data>,
        if_false: Block<'a, Data>,
    },
    Case {
        data: Data,
        value: Box<Expr<'a, Data>>,
        matches: Vec<(
            MatchPattern<'a, TypeAnnotation<'a, Data>, Data>,
            Block<'a, Data>,
        )>,
    },
    List(Vec<Expr<'a, Data>>, Data),
    Ref(RegionAnnotation<'a, Data>, Box<Expr<'a, Data>>, Data),
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
            Expr::Ref(_, _, data) => data,
        }
    }
}

impl<'a, Data> TreeFormat for Expr<'a, Data> {
    fn get_precedence(&self) -> i32 {
        match self {
            Expr::Literal(_, _) => 0,
            Expr::Variable(_, _) => 0,
            Expr::OperatorAsFunction(_, _) => 0,
            Expr::Tuple(_, _) => 0,
            Expr::List(_, _) => 0,
            Expr::Record(_, _) => 0,
            Expr::FunctionCall(_, _, _) => 1,
            Expr::OperatorCall(_, _) => 2,
            Expr::Lambda { .. } => 3,
            Expr::Ref(_, _, _) => 3,
            Expr::If { .. } => 4,
            Expr::Case { .. } => 4,
        }
    }

    fn format(&self) -> String {
        match self {
            Expr::Literal(lit, _) => format!("{}", lit),
            Expr::OperatorAsFunction(name, _) => format!("`{}`", name),
            Expr::Variable(ident, _) => format!("{}", ident),
            Expr::List(exprs, _) => format!("[{}]", format_children(self, exprs.iter(), ", ")),
            Expr::Tuple(exprs, _) => format!("({})", format_children(self, exprs.iter(), ", ")),
            Expr::Record(rows, _) => format_record(rows, "=", ", "),
            Expr::Lambda {
                params,
                body,
                data: _,
            } => format!("fun {} -> {}", format_iter(params.iter(), " "), body),
            Expr::FunctionCall(func, args, _) => {
                format!("{} {}", *func, format_children(self, args.iter(), " "))
            }
            Expr::OperatorCall(elements, _) => format!(
                "{}",
                format_iter(
                    elements.iter().map(|el| {
                        match el {
                            OperatorElement::Operator(op, _) => op.to_string(),
                            OperatorElement::Expression(expr) => self.format_child(expr),
                        }
                    }),
                    " "
                )
            ),
            Expr::If {
                data: _,
                cond,
                if_true,
                if_false,
            } => {
                format!(
                    "if {} then\n{}\nelse\n{}\nend",
                    self.format_child(&*cond),
                    *if_true,
                    *if_false
                )
            }
            Expr::Case {
                data: _,
                value,
                matches,
            } => {
                let mut res = String::new();
                res.push_str(&*format!("case {} of\n", self.format_child(&*value)));

                for (pat, block) in matches {
                    res.push_str(&*format!("| {} ->\n", pat));
                    res.push_str(&*format!("{}\n", block));
                }

                res.push_str("end");
                res
            }
            Expr::Ref(reg, expr, _) => format!("{} {}", reg, self.format_child(expr)),
        }
    }
}

impl<'a, Data> Display for Expr<'a, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

pub fn find_imported_modules<D: Copy>(ast: &TopLevelBlock<D>) -> Vec<(ModuleIdentifier, D)> {
    ast.top_levels
        .iter()
        .fold(Vec::new(), |mut imports, statement| {
            match statement {
                TopLevelStatement::Import(module_id, _, data) => {
                    imports.push((ModuleIdentifier::from_name(&[module_id]), *data));
                }
                _ => (),
            }
            imports
        })
}
