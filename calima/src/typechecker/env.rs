use crate::ast_common::OperatorSpecification;
use crate::types::Scheme;
use std::collections::HashMap;

pub trait Environment {
    fn lookup_value(&self, name: &str) -> Option<&Scheme>;
    fn lookup_operator(&self, name: &str) -> Option<&(Scheme, OperatorSpecification)>;
    fn lookup_module(&self, name: &str) -> Option<&Box<dyn Environment>>;
}

pub struct ModuleEnvironment<'a> {
    values: HashMap<&'a str, Scheme>,
    modules: HashMap<&'a str, Box<dyn Environment>>,
    operators: HashMap<&'a str, (Scheme, OperatorSpecification)>,
}

impl<'a> ModuleEnvironment<'a> {
    pub fn new() -> Self {
        Self {
            values: Default::default(),
            modules: Default::default(),
            operators: Default::default(),
        }
    }
}

impl<'a> ModuleEnvironment<'a> {
    pub fn add_value(&mut self, name: &'a str, sch: Scheme) {
        self.values.insert(name, sch);
    }

    pub fn add_operator(&mut self, name: &'a str, sch: Scheme, ops: OperatorSpecification) {
        self.operators.insert(name, (sch, ops));
    }

    pub fn add_module(&mut self, name: &'a str, module: Box<dyn Environment>) {
        self.modules.insert(name, module);
    }
}

impl<'a> Environment for ModuleEnvironment<'a> {
    fn lookup_value(&self, name: &str) -> Option<&Scheme> {
        self.values.get(name)
    }

    fn lookup_operator(&self, name: &str) -> Option<&(Scheme, OperatorSpecification)> {
        self.operators.get(name)
    }

    fn lookup_module(&self, name: &str) -> Option<&Box<dyn Environment>> {
        self.modules.get(name)
    }
}
