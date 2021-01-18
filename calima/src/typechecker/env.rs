use crate::ast_common::OperatorSpecification;
use crate::types::Scheme;
use std::collections::HashMap;

#[derive(Clone, Eq, PartialEq)]
pub enum Opening<'a> {
    All,
    Identifiers(Vec<&'a str>),
}

impl<'a> Opening<'a> {
    fn contains(&self, name: &str) -> bool {
        match self {
            Self::All => true,
            Self::Identifiers(idents) => idents.contains(&name),
        }
    }
}

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

    pub fn opened_values(&self, opening: &Opening<'_>) -> Vec<(&str, Scheme)> {
        self.values
            .iter()
            .filter(|(k, sch)| opening.contains(k))
            .map(|(name, scheme)| (*name, scheme.clone()))
            .collect()
    }

    pub fn opened_operators(
        &self,
        opening: &Opening<'_>,
    ) -> Vec<(&str, Scheme, OperatorSpecification)> {
        self.operators
            .iter()
            .filter(|(k, op)| opening.contains(k))
            .map(|(name, (scheme, op))| (*name, scheme.clone(), *op))
            .collect()
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
