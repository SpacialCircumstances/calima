use crate::types::{Exports, Scheme, build_function, int, float};

fn int_op() -> Scheme {
    Scheme::simple(build_function(&[ int(), int() ], &int()))
}

fn float_op() -> Scheme {
    Scheme::simple(build_function(&[ float(), float() ], &float()))
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
    ex
}