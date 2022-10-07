use crate::common::ModuleIdentifier;
use crate::formatting::tree::{format_children, TreeFormat};
use crate::formatting::*;
use crate::parsing::token::Span;
use crate::symbol_names::IText;
use std::convert::TryFrom;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name<Data>(pub Vec<IText>, pub Data);

impl<Data> Display for Name<Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_iter(self.0.iter(), "."))
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum NumberType {
    Integer,
    Float,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    None,
}

impl Display for Associativity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Associativity::Left => write!(f, "left"),
            Associativity::Right => write!(f, "right"),
            Associativity::None => write!(f, "none"),
        }
    }
}

impl TryFrom<&str> for Associativity {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "left" => Ok(Associativity::Left),
            "right" => Ok(Associativity::Right),
            "none" => Ok(Associativity::None),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone, Eq)]
pub enum OperatorSpecification {
    Infix(u32, Associativity),
    Prefix,
}

impl Display for OperatorSpecification {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorSpecification::Infix(prec, assoc) => write!(f, "infix {} {}", prec, assoc),
            OperatorSpecification::Prefix => write!(f, "prefix"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(IText),
    Number(IText, NumberType),
    Unit,
    Boolean(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(string) => write!(f, "\"{}\"", string),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
            Literal::Number(num, _) => write!(f, "{}", num),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BindPattern<Symbol: Display, TA: Display, Data> {
    Any(Data),
    UnitLiteral(Data),
    Name(Symbol, Option<TA>, Data),
    Tuple(Vec<BindPattern<Symbol, TA, Data>>, Data),
    Record(Vec<(Symbol, BindPattern<Symbol, TA, Data>)>, Data),
}

impl<Symbol: Display, TA: Display, Data> Display for BindPattern<Symbol, TA, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BindPattern::Any(_) => write!(f, "_"),
            BindPattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta),
            },
            BindPattern::Tuple(elements, _) => format_tuple(elements, f),
            BindPattern::Record(rows, _) => write!(f, "{}", format_record(rows, ":", ", ")),
            BindPattern::UnitLiteral(_) => write!(f, "()"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MatchPattern<Name: Display, Symbol: Display, TA: Display, Data> {
    Any(Data),
    Name(Symbol, Option<TA>, Data),
    Tuple(Vec<MatchPattern<Name, Symbol, TA, Data>>, Data),
    Literal(Literal, Data),
    Record(Vec<(Symbol, MatchPattern<Name, Symbol, TA, Data>)>, Data),
    SumUnwrap(
        Name,
        Option<Box<MatchPattern<Name, Symbol, TA, Data>>>,
        Data,
    ),
}

impl<Name: Display, Symbol: Display, TA: Display, Data> TreeFormat
    for MatchPattern<Name, Symbol, TA, Data>
{
    fn get_precedence(&self) -> i32 {
        match self {
            Self::Any(_) => 0,
            Self::Name(_, _, _) => 0,
            Self::Tuple(_, _) => 0,
            Self::Literal(_, _) => 0,
            Self::Record(_, _) => 0,
            Self::SumUnwrap(_, _, _) => 1,
        }
    }

    fn format(&self) -> String {
        match self {
            MatchPattern::Any(_) => "_".to_string(),
            MatchPattern::Name(id, ta, _) => match ta {
                None => id.to_string(),
                Some(ta) => format!("({}: {})", id, ta),
            },
            MatchPattern::Literal(lit, _) => lit.to_string(),
            MatchPattern::Tuple(elements, _) => {
                format!("({})", format_children(self, elements.iter(), ", "))
            }
            MatchPattern::Record(rows, _) => format_record(rows, ":", ", "),
            MatchPattern::SumUnwrap(constr, None, _) => constr.to_string(),
            MatchPattern::SumUnwrap(constr, Some(pat), _) => {
                format!("{} {}", constr, self.format_child(&*pat))
            }
        }
    }
}

impl<Name: Display, Symbol: Display, TA: Display, Data> Display
    for MatchPattern<Name, Symbol, TA, Data>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MatchPattern::Any(_) => write!(f, "_"),
            MatchPattern::Name(id, ta, _) => match ta {
                None => write!(f, "{}", id),
                Some(ta) => write!(f, "({}: {})", id, ta),
            },
            MatchPattern::Literal(lit, _) => write!(f, "{}", lit),
            MatchPattern::Tuple(elements, _) => format_tuple(elements, f),
            MatchPattern::Record(rows, _) => write!(f, "{}", format_record(rows, ":", ", ")),
            MatchPattern::SumUnwrap(constr, None, _) => write!(f, "{}", constr),
            MatchPattern::SumUnwrap(constr, Some(pat), _) => write!(f, "{} {}", constr, *pat),
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
    Reference(Box<TypeAnnotation<Name, Symbol, Data>>),
}

impl<Name: Display, Symbol: Display, Data> TreeFormat for TypeAnnotation<Name, Symbol, Data> {
    fn get_precedence(&self) -> i32 {
        match self {
            TypeAnnotation::Name(_) => 0,
            TypeAnnotation::Generic(_) => 0,
            TypeAnnotation::Tuple(_) => 0,
            TypeAnnotation::Function(_, _) => 1,
            TypeAnnotation::Parameterized(_, _) => 1,
            TypeAnnotation::Reference(_) => 2,
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
            TypeAnnotation::Reference(tp) => format!("{}", self.format_child(&*tp)),
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
                params,
                type_def: typedef,
                data: _,
            } => write!(
                f,
                "type {} {} = {}",
                name,
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
}

impl<Name: Display, Symbol: Display, Data> Display for Statement<Name, Symbol, Data> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
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
    Ref(Box<Expr<Name, Symbol, Data>>, Data),
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
            Expr::Ref(_, _) => 3,
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
            Expr::Ref(expr, _) => format!("{}", self.format_child(expr)),
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
