use crate::ast::Associativity;
use crate::ast::Associativity::Left;
use crate::ast::OperatorSpecification::{Infix, Prefix};
use crate::common::ModuleId;
use crate::symbol_names::StringInterner;
use crate::typechecker::environment::ScopeEnvironment;
use crate::typechecker::type_resolution::TypeResolution;
use crate::typechecker::ModuleTypecheckContext;
use crate::types::{bool, build_function, float, int, string, unit, GenericId, Scheme, Type};
use quetta::Text;
use std::collections::HashSet;
use std::fmt::Debug;

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
    let id = GenericId {
        id: 0,
        mod_id: ModuleId::PRELUDE,
    };
    eq_set.insert(id);
    Scheme(
        eq_set,
        build_function(&[Type::Var(id), Type::Var(id)], &bool()),
    )
}

pub fn prelude<Data: Copy + Debug>(
    ctx: &mut ModuleTypecheckContext<Data>,
    env: &mut ScopeEnvironment<Data>,
    vtc: &mut TypeResolution,
    interner: &StringInterner,
) {
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("+")),
        int_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new(".+")),
        float_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("-")),
        int_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new(".-")),
        float_op(),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("*")),
        int_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new(".*")),
        float_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("/")),
        int_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("./")),
        float_op(),
        Infix(80, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("..")),
        Scheme::simple(build_function(&[string(), string()], &string())),
        Infix(60, Left),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("==")),
        eq_type(),
        Infix(50, Associativity::None),
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new("~")),
        int_unary_op(),
        Prefix,
    );
    ctx.add_operator(
        env,
        vtc,
        interner.intern(Text::new(".~")),
        float_unary_op(),
        Prefix,
    );
    ctx.add(
        env,
        vtc,
        interner.intern(Text::new("println")),
        scheme(
            &[GenericId {
                mod_id: ModuleId::PRELUDE,
                id: 2,
            }],
            build_function(
                &[Type::Var(GenericId {
                    mod_id: ModuleId::PRELUDE,
                    id: 2,
                })],
                &unit(),
            ),
        ),
    );
}
