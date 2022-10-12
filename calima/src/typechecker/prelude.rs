use crate::ast::Associativity;
use crate::ast::Associativity::Left;
use crate::ast::OperatorSpecification::{Infix, Prefix};
use crate::symbol_names::StringInterner;
use crate::typechecker::environment::ScopeEnvironment;
use crate::typechecker::Context;
use crate::types::{bool, build_function, float, int, string, unit, GenericId, Scheme, Type};
use quetta::Text;
use std::collections::HashSet;
use std::fmt::Debug;
use std::rc::Rc;

fn int_op() -> Scheme {
    Scheme::simple(build_function(&[int(), int()], &int()))
}

fn int_unary_op() -> Scheme {
    Scheme::simple(build_function(&[int()], &int()))
}

fn float_op() -> Scheme {
    Scheme::simple(build_function(&[float(), float()], &float()))
}

fn float_unary_op() -> Scheme {
    Scheme::simple(build_function(&[float()], &float()))
}

fn scheme(gen: &[GenericId], tp: Type) -> Scheme {
    let mut gens = HashSet::new();
    gens.extend(gen.iter());
    Scheme(gens, tp)
}

fn eq_type() -> Scheme {
    let mut eq_set = HashSet::new();
    let id = GenericId(1);
    eq_set.insert(id);
    Scheme(
        eq_set,
        build_function(&[Type::Var(id), Type::Var(id)], &bool()),
    )
}

pub fn prelude<Data: Copy + Debug>(
    ctx: &mut Context<Data>,
    env: &mut ScopeEnvironment<Data>,
    interner: &StringInterner,
) {
    ctx.add_operator(
        env,
        interner.intern(Text::new("+")),
        int_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new(".+")),
        float_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new("-")),
        int_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new(".-")),
        float_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new("*")),
        int_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new(".*")),
        float_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new("/")),
        int_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new("./")),
        float_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new("..")),
        Scheme::simple(build_function(&[string(), string()], &string())),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        interner.intern(Text::new("==")),
        eq_type(),
        Infix(50, Associativity::None),
    );
    ctx.add_operator(env, interner.intern(Text::new("~")), int_unary_op(), Prefix);
    ctx.add_operator(
        env,
        interner.intern(Text::new(".~")),
        float_unary_op(),
        Prefix,
    );
    ctx.add(
        env,
        interner.intern(Text::new("println")),
        scheme(
            &[GenericId(1)],
            build_function(&[Type::Var(GenericId(1))], &unit()),
        ),
    );
}
