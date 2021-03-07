use crate::ast_common::{BindPattern, Literal, MatchPattern, Name, OperatorSpecification};
use crate::common::ModuleIdentifier;
use crate::formatting::tree::{format_children, TreeFormat};
use crate::formatting::*;
use crate::names::SymbolName;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct RegionVariable<Data>(pub SymbolName, pub Data);

impl<Data> Display for RegionVariable<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegionAnnotation<Data> {
    Anonymous,
    Stack,
    Named(SymbolName, Data),
    Var(RegionVariable<Data>),
}

impl<Data> Display for RegionAnnotation<Data> {
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
pub struct GenericTypeKind<Data>(pub SymbolName, pub Data);

impl<Data> Display for GenericTypeKind<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation<Data> {
    Name(Name<Data>),
    Generic(GenericTypeKind<Data>),
    Function(Box<TypeAnnotation<Data>>, Box<TypeAnnotation<Data>>),
    Tuple(Vec<TypeAnnotation<Data>>),
    Parameterized(Name<Data>, Vec<TypeAnnotation<Data>>),
    Reference(RegionAnnotation<Data>, Box<TypeAnnotation<Data>>),
}

impl<Data> TreeFormat for TypeAnnotation<Data> {
    fn get_precedence(&self) -> i32 {
        match self {
            TypeAnnotation::Name(_) => 0,
            TypeAnnotation::Generic(_) => 0,
            TypeAnnotation::Tuple(_) => 0,
            TypeAnnotation::Function(_, _) => 1,
            TypeAnnotation::Parameterized(_, _) => 1,
            TypeAnnotation::Reference(_, _) => 2,
        }
    }

    fn format(&self) -> String {
        match self {
            TypeAnnotation::Name(name) => name.to_string(),
            TypeAnnotation::Generic(name) => name.to_string(),
            TypeAnnotation::Function(i, o) => {
                format!("{} -> {}", self.format_child(&*i), self.format_child(&*o))
            }
            TypeAnnotation::Parameterized(name, params) => {
                format!("{} {}", name, format_children(self, params.iter(), " "))
            }
            TypeAnnotation::Tuple(elements) => {
                format!("({})", format_children(self, elements.iter(), ", "))
            }
            TypeAnnotation::Reference(reg, tp) => format!("{} {}", reg, self.format_child(&*tp)),
        }
    }
}

impl<Data> Display for TypeAnnotation<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<Data> {
    Alias(TypeAnnotation<Data>),
    Record(Vec<(SymbolName, TypeAnnotation<Data>)>),
    Sum(Vec<(SymbolName, Option<TypeAnnotation<Data>>)>),
}

impl<Data> Display for TypeDefinition<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDefinition::Alias(ta) => write!(f, "{}", ta),
            TypeDefinition::Record(rows) => write!(f, "{}", format_record(rows, ":", ", ")),
            TypeDefinition::Sum(rows) => {
                let str = rows
                    .iter()
                    .map(|(constr, ta)| match ta {
                        None => constr.to_string(),
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
pub struct Let<Data> {
    pub mods: Vec<Modifier>,
    pub pattern: BindPattern<TypeAnnotation<Data>, Data>,
    pub value: Expr<Data>,
    pub data: Data,
}

impl<Data> Display for Let<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "let {}{} = {}",
            format_iter_end(self.mods.iter(), " "),
            self.pattern,
            self.value
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetOperator<Data> {
    pub mods: Vec<Modifier>,
    pub op: OperatorSpecification,
    pub name: SymbolName,
    pub ta: Option<TypeAnnotation<Data>>,
    pub value: Expr<Data>,
    pub data: Data,
}

impl<Data> Display for LetOperator<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "let {}{} {} = {}",
            format_iter_end(self.mods.iter(), " "),
            self.op,
            self.name,
            self.value
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Visibility {
    Public,
}

impl Display for Visibility {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => write!(f, "public"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TopLevelStatement<Data> {
    Import {
        module: Name<Data>,
        opens: Vec<(SymbolName, Data)>,
        data: Data,
    },
    Type {
        name: SymbolName,
        regions: Vec<RegionVariable<Data>>,
        params: Vec<GenericTypeKind<Data>>,
        type_def: TypeDefinition<Data>,
        data: Data,
    },
    Let(Option<Visibility>, Let<Data>),
    LetOperator(Option<Visibility>, LetOperator<Data>),
}

impl<Data> Display for TopLevelStatement<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelStatement::Import { module, .. } => write!(f, "import {}", module),
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
            TopLevelStatement::Let(vis, lets) => match vis {
                None => write!(f, "{}", lets),
                Some(vis) => write!(f, "{}{}", vis, lets),
            },
            TopLevelStatement::LetOperator(vis, lets) => match vis {
                None => write!(f, "{}", lets),
                Some(vis) => write!(f, "{}{}", vis, lets),
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<Data> {
    Let(Let<Data>),
    LetOperator(LetOperator<Data>),
    Do(Expr<Data>, Data),
    Region(SymbolName, Data),
}

impl<Data> Display for Statement<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Region(name, _) => write!(f, "region {}", name),
            Statement::Do(expr, _) => write!(f, "do {}", expr),
            Statement::Let(lets) => write!(f, "{}", lets),
            Statement::LetOperator(lets) => write!(f, "{}", lets),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TopLevelBlock<Data>(pub(crate) Vec<TopLevelStatement<Data>>);

impl<Data> Display for TopLevelBlock<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for tls in &self.0 {
            writeln!(f, "{}", tls)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<Data> {
    pub statements: Vec<Statement<Data>>,
    pub result: Box<Expr<Data>>,
}

impl<Data> Display for Block<Data> {
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
pub enum OperatorElement<Data> {
    Operator(SymbolName, Data),
    Expression(Expr<Data>),
}

impl<Data> Display for OperatorElement<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorElement::Expression(expr) => write!(f, "{}", expr),
            OperatorElement::Operator(name, _) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<Data> {
    OperatorAsFunction(SymbolName, Data),
    Variable(Name<Data>),
    FunctionCall(Box<Expr<Data>>, Vec<Expr<Data>>, Data),
    OperatorCall(Vec<OperatorElement<Data>>, Data),
    Record(Vec<(SymbolName, Expr<Data>)>, Data),
    Tuple(Vec<Expr<Data>>, Data),
    Literal(Literal, Data),
    Lambda {
        params: Vec<BindPattern<TypeAnnotation<Data>, Data>>,
        body: Block<Data>,
        data: Data,
    },
    If {
        data: Data,
        cond: Box<Expr<Data>>,
        if_true: Block<Data>,
        if_false: Block<Data>,
    },
    Case {
        data: Data,
        value: Box<Expr<Data>>,
        matches: Vec<(
            MatchPattern<Name<Data>, TypeAnnotation<Data>, Data>,
            Block<Data>,
        )>,
    },
    List(Vec<Expr<Data>>, Data),
    Ref(RegionAnnotation<Data>, Box<Expr<Data>>, Data),
}

impl<Data> Expr<Data> {
    pub fn get_location(&self) -> &Data {
        match self {
            Expr::OperatorAsFunction(_, data) => data,
            Expr::Variable(name) => &name.1,
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

impl<Data> TreeFormat for Expr<Data> {
    fn get_precedence(&self) -> i32 {
        match self {
            Expr::Literal(_, _) => 0,
            Expr::Variable(_) => 0,
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
            Expr::Literal(lit, _) => lit.to_string(),
            Expr::OperatorAsFunction(name, _) => format!("`{}`", name),
            Expr::Variable(ident) => ident.to_string(),
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
            Expr::OperatorCall(elements, _) => format_iter(
                elements.iter().map(|el| match el {
                    OperatorElement::Operator(op, _) => op.to_string(),
                    OperatorElement::Expression(expr) => self.format_child(expr),
                }),
                " ",
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

impl<Data> Display for Expr<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

pub fn find_imported_modules<D: Copy>(ast: &TopLevelBlock<D>) -> Vec<(ModuleIdentifier, D)> {
    ast.0.iter().fold(Vec::new(), |mut imports, statement| {
        match statement {
            TopLevelStatement::Import { module, data, .. } => {}
            _ => (),
        }
        imports
    })
}
