use crate::ast_common::Associativity;
use crate::ast_common::Associativity::Left;
use crate::ast_common::OperatorSpecification::{Infix, Prefix};
use crate::parsing::names::SymbolNameInterner;
use crate::typechecker::env::ModuleEnvironment;
use crate::types::{bool, build_function, float, int, string, unit, GenericId, Scheme, Type};
use std::collections::HashSet;
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
    Scheme(HashSet::new(), gens, tp)
}

fn eq_type() -> Scheme {
    let mut eq_set = HashSet::new();
    let id = GenericId(1);
    eq_set.insert(id);
    Scheme(
        HashSet::new(),
        eq_set,
        build_function(&[Type::Var(id), Type::Var(id)], &bool()),
    )
}

pub fn prelude(interner: &SymbolNameInterner) -> Rc<ModuleEnvironment> {
    let mut env = ModuleEnvironment::new();

    env.add_operator(interner.intern("+"), int_op(), Infix(60, Left));
    env.add_operator(interner.intern(".+"), float_op(), Infix(60, Left));
    env.add_operator(interner.intern("-"), int_op(), Infix(60, Left));
    env.add_operator(interner.intern(".-"), float_op(), Infix(60, Left));
    env.add_operator(interner.intern("*"), int_op(), Infix(80, Left));
    env.add_operator(interner.intern(".*"), float_op(), Infix(80, Left));
    env.add_operator(interner.intern("/"), int_op(), Infix(80, Left));
    env.add_operator(interner.intern("./"), float_op(), Infix(80, Left));
    env.add_operator(
        interner.intern(".."),
        Scheme::simple(build_function(&[string(), string()], &string())),
        Infix(60, Left),
    );
    env.add_operator(
        interner.intern("=="),
        eq_type(),
        Infix(50, Associativity::None),
    );
    env.add_operator(interner.intern("~"), int_unary_op(), Prefix);
    env.add_operator(interner.intern(".~"), float_unary_op(), Prefix);
    env.add_value(
        interner.intern("println"),
        scheme(
            &[GenericId(1)],
            build_function(&[Type::Var(GenericId(1))], &unit()),
        ),
    );
    Rc::new(env)
}
