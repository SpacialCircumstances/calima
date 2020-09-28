use crate::types::{Exports, Scheme, build_function, int, float, bool, string, unit, GenericId, Type};
use std::collections::HashSet;

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
    ex.add_variable("+", int_op());
    ex.add_variable(".+", float_op());
    ex.add_variable("-", int_op());
    ex.add_variable(".-", float_op());
    ex.add_variable("*", int_op());
    ex.add_variable(".*", float_op());
    ex.add_variable("/", int_op());
    ex.add_variable("./", float_op());
    ex.add_variable("..", Scheme::simple(build_function(&[ string(), string() ], &string())));
    ex.add_variable("println", scheme(&[ GenericId(1) ], build_function(&[ Type::Var(GenericId(1)) ], &unit())));
    ex.add_variable("==", eq_type());
    ex
}