use crate::ast_common::{BindPattern, Literal, MatchPattern, Name, OperatorSpecification};
use crate::common::ModuleIdentifier;
use crate::formatting::tree::{format_children, TreeFormat};
use crate::formatting::*;
use crate::parsing::token::Span;
use crate::symbol_names::SymbolName;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct RegionVariable<Symbol: Display, Data>(pub Symbol, pub Data);

impl<Symbol: Display, Data> Display for RegionVariable<Symbol, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegionAnnotation<Symbol: Display, Data> {
    Anonymous,
    Stack,
    Named(Symbol, Data),
    Var(RegionVariable<Symbol, Data>),
}

impl<Symbol: Display, Data> Display for RegionAnnotation<Symbol, Data> {
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
pub struct GenericTypeKind<Symbol: Display, Data>(pub Symbol, pub Data);

impl<Symbol: Display, Data> Display for GenericTypeKind<Symbol, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation<Name: Display, Symbol: Display, Data> {
    Name(Name),
    Generic(GenericTypeKind<Symbol, Data>),
    Function(
        Box<TypeAnnotation<Name, Symbol, Data>>,
        Box<TypeAnnotation<Name, Symbol, Data>>,
    ),
    Tuple(Vec<TypeAnnotation<Name, Symbol, Data>>),
    Parameterized(Name, Vec<TypeAnnotation<Name, Symbol, Data>>),
    Reference(
        RegionAnnotation<Symbol, Data>,
        Box<TypeAnnotation<Name, Symbol, Data>>,
    ),
}

impl<Name: Display, Symbol: Display, Data> TreeFormat for TypeAnnotation<Name, Symbol, Data> {
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

impl<Name: Display, Symbol: Display, Data> Display for TypeAnnotation<Name, Symbol, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeDefinition<Name: Display, Symbol: Display, Data> {
    Alias(TypeAnnotation<Name, Symbol, Data>),
    Record(Vec<(Symbol, TypeAnnotation<Name, Symbol, Data>)>),
    Sum(Vec<(Symbol, Option<TypeAnnotation<Name, Symbol, Data>>)>),
}

impl<Name: Display, Symbol: Display, Data> Display for TypeDefinition<Name, Symbol, Data> {
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
pub struct Let<Name: Display, Symbol: Display, Data> {
    pub mods: Vec<Modifier>,
    pub pattern: BindPattern<Symbol, TypeAnnotation<Name, Symbol, Data>, Data>,
    pub value: Expr<Name, Symbol, Data>,
    pub data: Data,
}

impl<Name: Display, Symbol: Display, Data> Display for Let<Name, Symbol, Data> {
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
pub struct LetOperator<Name: Display, Symbol: Display, Data> {
    pub mods: Vec<Modifier>,
    pub op: OperatorSpecification,
    pub name: Symbol,
    pub ta: Option<TypeAnnotation<Name, Symbol, Data>>,
    pub value: Expr<Name, Symbol, Data>,
    pub data: Data,
}

impl<Name: Display, Symbol: Display, Data> Display for LetOperator<Name, Symbol, Data> {
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
pub enum TopLevelStatement<Name: Display, Symbol: Display, Data> {
    Import {
        module: Name,
        opens: Vec<(Symbol, Data)>,
        data: Data,
    },
    Type {
        name: Symbol,
        regions: Vec<RegionVariable<Symbol, Data>>,
        params: Vec<GenericTypeKind<Symbol, Data>>,
        type_def: TypeDefinition<Name, Symbol, Data>,
        data: Data,
    },
    Let(Option<Visibility>, Let<Name, Symbol, Data>),
    LetOperator(Option<Visibility>, LetOperator<Name, Symbol, Data>),
}

impl<Name: Display, Symbol: Display, Data> Display for TopLevelStatement<Name, Symbol, Data> {
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
pub enum Statement<Name: Display, Symbol: Display, Data> {
    Let(Let<Name, Symbol, Data>),
    LetOperator(LetOperator<Name, Symbol, Data>),
    Do(Expr<Name, Symbol, Data>, Data),
    Region(Symbol, Data),
}

impl<Name: Display, Symbol: Display, Data> Display for Statement<Name, Symbol, Data> {
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
pub struct TopLevelBlock<Name: Display, Symbol: Display, Data>(
    pub(crate) Vec<TopLevelStatement<Name, Symbol, Data>>,
);

impl<Name: Display, Symbol: Display, Data> Display for TopLevelBlock<Name, Symbol, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for tls in &self.0 {
            writeln!(f, "{}", tls)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<Name: Display, Symbol: Display, Data> {
    pub statements: Vec<Statement<Name, Symbol, Data>>,
    pub result: Box<Expr<Name, Symbol, Data>>,
}

impl<Name: Display, Symbol: Display, Data> Display for Block<Name, Symbol, Data> {
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
pub enum OperatorElement<Name: Display, Symbol: Display, Data> {
    Operator(Symbol, Data),
    Expression(Expr<Name, Symbol, Data>, Data),
}

impl<Name: Display, Symbol: Display, Data> Display for OperatorElement<Name, Symbol, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorElement::Expression(expr, _) => write!(f, "{}", expr),
            OperatorElement::Operator(name, _) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<Name: Display, Symbol: Display, Data> {
    OperatorAsFunction(Symbol, Data),
    Variable(Name),
    FunctionCall(
        Box<Expr<Name, Symbol, Data>>,
        Vec<Expr<Name, Symbol, Data>>,
        Data,
    ),
    OperatorCall(Vec<OperatorElement<Name, Symbol, Data>>, Data),
    Record(Vec<(Symbol, Expr<Name, Symbol, Data>)>, Data),
    Tuple(Vec<Expr<Name, Symbol, Data>>, Data),
    Literal(Literal, Data),
    Lambda {
        params: Vec<BindPattern<Symbol, TypeAnnotation<Name, Symbol, Data>, Data>>,
        body: Block<Name, Symbol, Data>,
        data: Data,
    },
    If {
        data: Data,
        cond: Box<Expr<Name, Symbol, Data>>,
        if_true: Block<Name, Symbol, Data>,
        if_false: Block<Name, Symbol, Data>,
    },
    Case {
        data: Data,
        value: Box<Expr<Name, Symbol, Data>>,
        matches: Vec<(
            MatchPattern<Name, Symbol, TypeAnnotation<Name, Symbol, Data>, Data>,
            Block<Name, Symbol, Data>,
        )>,
    },
    List(Vec<Expr<Name, Symbol, Data>>, Data),
    Ref(
        RegionAnnotation<Symbol, Data>,
        Box<Expr<Name, Symbol, Data>>,
        Data,
    ),
}

impl<Name: Display, Symbol: Display, Data> TreeFormat for Expr<Name, Symbol, Data> {
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
                    OperatorElement::Expression(expr, _) => self.format_child(expr),
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

impl<Name: Display, Symbol: Display, Data> Display for Expr<Name, Symbol, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

pub fn find_imported_modules<Name: Display, Symbol: Display, D: Copy>(
    ast: &TopLevelBlock<Name, Symbol, D>,
) -> Vec<(ModuleIdentifier, D)> {
    ast.0.iter().fold(Vec::new(), |mut imports, statement| {
        match statement {
            TopLevelStatement::Import { module, data, .. } => {}
            _ => (),
        }
        imports
    })
}
