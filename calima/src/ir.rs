use crate::ast::NumberType;
use crate::formatting::context::{format_ctx_iter, format_ctx_iter_end, FormatWithContext};
use crate::formatting::{format_iter, format_iter_end};
use crate::modules::TypedModuleData;
use crate::symbol_names::IText;
use crate::typechecker::ValueTypeContext;
use crate::types::{Scheme, Type};
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

pub struct FormattingContext<'a> {
    vtc: &'a ValueTypeContext,
}

impl<'a> FormattingContext<'a> {
    pub fn new(vtc: &'a ValueTypeContext) -> Self {
        Self { vtc }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constant {
    Unit,
    String(IText),
    Number(IText, NumberType),
    Boolean(bool),
}

impl<'a> FormatWithContext<'a> for Constant {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
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

impl<'a> FormatWithContext<'a> for Expr {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::List(vals) => {
                write!(f, "[")?;
                format_ctx_iter(ctx, f, vals.iter(), ", ")?;
                write!(f, "]")
            }
            Expr::FunctionCall { func, args } => {
                func.format(ctx, f)?;
                write!(f, " ")?;
                format_ctx_iter(ctx, f, args.iter(), " ")
            }
            Expr::Lambda { params, block } => {
                write!(f, "fun ")?;
                format_ctx_iter(ctx, f, params.iter(), " ")?;
                write!(f, " -> ")?;
                block.format(ctx, f)
            }
            Expr::If {
                condition,
                if_true,
                if_false,
            } => {
                write!(f, "if ")?;
                condition.format(ctx, f)?;
                write!(f, " then ")?;
                if_true.format(ctx, f)?;
                write!(f, " else ")?;
                if_false.format(ctx, f)?;
                write!(f, " end")
            }
            Expr::Generalize(v) => {
                write!(f, "$generalize ")?;
                v.format(ctx, f)
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct VarRef(pub usize);

impl<'a> FormatWithContext<'a> for VarRef {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
        let sch = ctx
            .vtc
            .get_type(&Val::Var(*self))
            .unwrap_or_else(|| Scheme::simple(Type::Error));
        match ctx.vtc.get_name_hint(self) {
            None => write!(f, "v{}<{}>", self.0, sch),
            Some(nh) => write!(f, "v{}({})<{}>", self.0, nh, sch),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val {
    Var(VarRef),
    Constant(Constant),
}

impl<'a> FormatWithContext<'a> for Val {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Var(v) => v.format(ctx, f),
            Val::Constant(c) => c.format(ctx, f),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BindTarget {
    Var(VarRef),
    Discard,
}

impl<'a> FormatWithContext<'a> for BindTarget {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BindTarget::Var(v) => v.format(ctx, f),
            BindTarget::Discard => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binding(pub BindTarget, pub Expr);

impl<'a> FormatWithContext<'a> for Binding {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.format(ctx, f)?;
        write!(f, " = ")?;
        self.1.format(ctx, f)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Block(pub Vec<Binding>, pub Val);

impl<'a> FormatWithContext<'a> for Block {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        format_ctx_iter_end(ctx, f, self.0.iter(), ";\n")?;
        self.1.format(ctx, f)?;
        write!(f, "\n}}")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Module {
    pub(crate) externs: Vec<Val>,
    pub(crate) main_block: Block,
    pub(crate) export: Vec<Val>,
}

impl<'a> FormatWithContext<'a> for Module {
    type Context = FormattingContext<'a>;

    fn format(&'a self, ctx: &mut Self::Context, f: &mut Formatter<'_>) -> std::fmt::Result {
        for ext in &self.externs {
            write!(f, "extern ")?;
            ext.format(ctx, f)?;
            write!(f, "\n")?;
        }

        self.main_block.format(ctx, f)?;
        write!(f, "\n")?;

        for exp in &self.export {
            write!(f, "export ")?;
            exp.format(ctx, f)?;
            write!(f, "\n")?;
        }

        Ok(())
    }
}
