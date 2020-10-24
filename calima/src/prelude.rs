use crate::types::{Exports, Scheme, build_function, int, float, bool, string, unit, GenericId, Type};
use std::collections::HashSet;
use crate::common::{OperatorSpecification, Associativity};
use crate::common::Associativity::Left;
use crate::common::OperatorSpecification::Infix;

fn int_op() -> Scheme {
    Scheme::simple(build_function(&[ int(), int() ], &int()))
}

fn float_op() -> Scheme {
    Scheme::simple(build_function(&[ float(), float() ], &float()))
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
    Scheme(eq_set, build_function(& [ Type::Var(id), Type::Var(id) ], &bool()))
}

pub fn prelude() -> Exports<'static> {
    let mut ex = Exports::new();
    ex.add_operator("+", int_op(), Infix(60, Left));
    ex.add_operator(".+", float_op(), Infix(60, Left));
    ex.add_operator("-", int_op(), Infix(60, Left));
    ex.add_operator(".-", float_op(), Infix(60, Left));
    ex.add_operator("*", int_op(), Infix(80, Left));
    ex.add_operator(".*", float_op(), Infix(80, Left));
    ex.add_operator("/", int_op(), Infix(80, Left));
    ex.add_operator("./", float_op(), Infix(80, Left));
    ex.add_operator("..", Scheme::simple(build_function(&[ string(), string() ], &string())), Infix(60, Left));
    ex.add_operator("==", eq_type(), Infix(50, Associativity::None));
    ex.add_value("println", scheme(&[ GenericId(1) ], build_function(&[ Type::Var(GenericId(1)) ], &unit())));
    ex
}