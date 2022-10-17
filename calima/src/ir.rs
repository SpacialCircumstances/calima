use crate::ast::NumberType;
use crate::formatting::{format_iter, format_iter_end};
use crate::modules::TypedModuleData;
use crate::symbol_names::IText;
use crate::typechecker::ValueTypeContext;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

pub trait FormatWithValueTypeContext {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result;
}

fn format_ctx_iter<'a, T: FormatWithValueTypeContext + 'a, I: Iterator<Item = &'a T>>(
    vtc: &ValueTypeContext,
    f: &mut Formatter<'_>,
    mut iter: I,
    sep: &str,
) -> std::fmt::Result {
    let mut next = iter.next();

    while let Some(ni) = next.take() {
        ni.format(vtc, f)?;
        next = iter.next();

        if let Some(_) = &next {
            write!(f, "{}", sep)?;
        }
    }

    Ok(())
}

fn format_ctx_iter_end<'a, T: FormatWithValueTypeContext + 'a, I: Iterator<Item = &'a T>>(
    vtc: &ValueTypeContext,
    f: &mut Formatter<'_>,
    mut iter: I,
    sep: &str,
) -> std::fmt::Result {
    for it in iter {
        it.format(vtc, f)?;
        write!(f, "{}", sep)?;
    }

    Ok(())
}

struct WithVTC<'a, T: FormatWithValueTypeContext>(&'a T, &'a ValueTypeContext);

impl<'a, T: FormatWithValueTypeContext> Display for WithVTC<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.format(&self.1, f)
    }
}

pub fn format_to_string<T: FormatWithValueTypeContext>(t: &T, vtc: &ValueTypeContext) -> String {
    WithVTC(t, vtc).to_string()
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constant {
    Unit,
    String(IText),
    Number(IText, NumberType),
    Boolean(bool),
}

impl FormatWithValueTypeContext for Constant {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Unit => write!(f, "()"),
            Constant::String(t) => write!(f, "\"{}\"", t),
            Constant::Number(t, _) => write!(f, "{}", t),
            Constant::Boolean(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    List(Vec<Val>),
    FunctionCall {
        func: Val,
        args: Vec<Val>,
    },
    Lambda {
        params: Vec<BindTarget>,
        block: Block,
    },
    If {
        condition: Val,
        if_true: Block,
        if_false: Block,
    },
    Generalize(Val),
}

impl FormatWithValueTypeContext for Expr {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::List(vals) => {
                write!(f, "[")?;
                format_ctx_iter(vtc, f, vals.iter(), ", ")?;
                write!(f, "]")
            }
            Expr::FunctionCall { func, args } => {
                func.format(vtc, f)?;
                write!(f, " ")?;
                format_ctx_iter(vtc, f, args.iter(), " ")
            }
            Expr::Lambda { params, block } => {
                write!(f, "fun ")?;
                format_ctx_iter(vtc, f, params.iter(), " ")?;
                write!(f, " -> ")?;
                block.format(vtc, f)
            }
            Expr::If {
                condition,
                if_true,
                if_false,
            } => {
                write!(f, "if ")?;
                condition.format(vtc, f)?;
                write!(f, " then ")?;
                if_true.format(vtc, f)?;
                write!(f, " else ")?;
                if_false.format(vtc, f)?;
                write!(f, " end")
            }
            Expr::Generalize(v) => {
                write!(f, "$generalize ")?;
                v.format(vtc, f)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct VarRef(pub usize);

impl FormatWithValueTypeContext for VarRef {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        match vtc.get_name_hint(self) {
            None => write!(f, "v{}", self.0),
            Some(nh) => write!(f, "v{}({})", self.0, nh),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Var(VarRef),
    Constant(Constant),
}

impl FormatWithValueTypeContext for Val {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Var(v) => v.format(vtc, f),
            Val::Constant(c) => c.format(vtc, f),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BindTarget {
    Var(VarRef),
    Discard,
}

impl FormatWithValueTypeContext for BindTarget {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BindTarget::Var(v) => v.format(vtc, f),
            BindTarget::Discard => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binding(pub BindTarget, pub Expr);

impl FormatWithValueTypeContext for Binding {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.format(vtc, f)?;
        write!(f, " = ")?;
        self.1.format(vtc, f)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block(pub Vec<Binding>, pub Val);

impl FormatWithValueTypeContext for Block {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        format_ctx_iter_end(vtc, f, self.0.iter(), ";\n")?;
        self.1.format(vtc, f)?;
        write!(f, "\n}}")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Module {
    pub(crate) externs: Vec<Val>,
    pub(crate) main_block: Block,
    pub(crate) export: Vec<Val>,
}

impl FormatWithValueTypeContext for Module {
    fn format(&self, vtc: &ValueTypeContext, f: &mut Formatter<'_>) -> std::fmt::Result {
        for ext in &self.externs {
            write!(f, "extern ")?;
            ext.format(vtc, f)?;
            write!(f, "\n")?;
        }

        self.main_block.format(vtc, f)?;
        write!(f, "\n")?;

        for exp in &self.export {
            write!(f, "export ")?;
            exp.format(vtc, f)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}
