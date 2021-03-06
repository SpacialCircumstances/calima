use crate::ast_common::OperatorSpecification;
use crate::parsing::names::SymbolName;
use crate::types::Scheme;
use std::collections::HashMap;

#[derive(Clone, Eq, PartialEq)]
pub enum Opening {
    All,
    Identifiers(Vec<SymbolName>),
}

impl Opening {
    fn contains(&self, name: &SymbolName) -> bool {
        match self {
            Self::All => true,
            Self::Identifiers(idents) => idents.contains(&name),
        }
    }
}

pub trait Environment {
    fn lookup_value(&self, name: &SymbolName) -> Option<&Scheme>;
    fn lookup_operator(&self, name: &SymbolName) -> Option<&(Scheme, OperatorSpecification)>;
    fn lookup_module(&self, name: &SymbolName) -> Option<&Box<dyn Environment>>;
}

pub struct ModuleEnvironment {
    values: HashMap<SymbolName, Scheme>,
    modules: HashMap<SymbolName, Box<dyn Environment>>,
    operators: HashMap<SymbolName, (Scheme, OperatorSpecification)>,
}

impl ModuleEnvironment {
    pub fn new() -> Self {
        Self {
            values: Default::default(),
            modules: Default::default(),
            operators: Default::default(),
        }
    }

    pub fn opened_values(&self, opening: &Opening) -> Vec<(SymbolName, Scheme)> {
        self.values
            .iter()
            .filter(|(k, sch)| opening.contains(k))
            .map(|(name, scheme)| (name.clone(), scheme.clone()))
            .collect()
    }

    pub fn opened_operators(
        &self,
        opening: &Opening,
    ) -> Vec<(SymbolName, Scheme, OperatorSpecification)> {
        self.operators
            .iter()
            .filter(|(k, op)| opening.contains(k))
            .map(|(name, (scheme, op))| (name.clone(), scheme.clone(), *op))
            .collect()
    }
}

impl ModuleEnvironment {
    pub fn add_value(&mut self, name: SymbolName, sch: Scheme) {
        self.values.insert(name, sch);
    }

    pub fn add_operator(&mut self, name: SymbolName, sch: Scheme, ops: OperatorSpecification) {
        self.operators.insert(name, (sch, ops));
    }

    pub fn add_module(&mut self, name: SymbolName, module: Box<dyn Environment>) {
        self.modules.insert(name, module);
    }
}

impl<'a> Environment for ModuleEnvironment {
    fn lookup_value(&self, name: &SymbolName) -> Option<&Scheme> {
        self.values.get(name)
    }

    fn lookup_operator(&self, name: &SymbolName) -> Option<&(Scheme, OperatorSpecification)> {
        self.operators.get(name)
    }

    fn lookup_module(&self, name: &SymbolName) -> Option<&Box<dyn Environment>> {
        self.modules.get(name)
    }
}
