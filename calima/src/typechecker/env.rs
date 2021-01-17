use crate::ast_common::OperatorSpecification;
use crate::types::{Exports, Scheme};
use std::collections::HashMap;

pub trait Environment {
    fn lookup_name(&self, name: &str) -> Option<&Scheme>;
    fn lookup_operator(&self, name: &str) -> Option<&(Scheme, OperatorSpecification)>;
    fn lookup_module(&self, name: &str) -> Option<&Box<dyn Environment>>;
}

pub struct ModuleEnvironment<'a> {
    names: HashMap<&'a str, Scheme>,
    modules: HashMap<&'a str, Box<dyn Environment<'a>>>,
    operators: HashMap<&'a str, (Scheme, OperatorSpecification)>,
}

impl<'a> ModuleEnvironment<'a> {
    pub fn from_exports(exports: Exports<'a>) -> Self {
        //TODO
        Self {
            names: HashMap::new(),
            modules: HashMap::new(),
            operators: HashMap::new(),
        }
    }
}

impl<'a> Environment for ModuleEnvironment<'a> {
    fn lookup_name(&self, name: &str) -> Option<&Scheme> {
        self.names.get(name)
    }

    fn lookup_operator(&self, name: &str) -> Option<&(Scheme, OperatorSpecification)> {
        self.operators.get(name)
    }

    fn lookup_module(&self, name: &str) -> Option<&Box<dyn Environment<'a>>> {
        self.modules.get(name)
    }
}
