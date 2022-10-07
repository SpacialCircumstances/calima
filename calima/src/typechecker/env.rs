use crate::ast_common::OperatorSpecification;
use crate::symbol_names::IText;
use crate::types::Scheme;
use std::collections::HashMap;

#[derive(Clone, Eq, PartialEq)]
pub enum Opening {
    All,
    Identifiers(Vec<IText>),
}

impl Opening {
    fn contains(&self, name: &IText) -> bool {
        match self {
            Self::All => true,
            Self::Identifiers(idents) => idents.contains(&name),
        }
    }
}

pub trait Environment {
    fn lookup_value(&self, name: &IText) -> Option<&Scheme>;
    fn lookup_operator(&self, name: &IText) -> Option<&(Scheme, OperatorSpecification)>;
    fn lookup_module(&self, name: &IText) -> Option<&Box<dyn Environment>>;
}

pub struct ModuleEnvironment {
    values: HashMap<IText, Scheme>,
    modules: HashMap<IText, Box<dyn Environment>>,
    operators: HashMap<IText, (Scheme, OperatorSpecification)>,
}

impl ModuleEnvironment {
    pub fn new() -> Self {
        Self {
            values: Default::default(),
            modules: Default::default(),
            operators: Default::default(),
        }
    }

    pub fn opened_values(&self, opening: &Opening) -> Vec<(IText, Scheme)> {
        self.values
            .iter()
            .filter(|(k, sch)| opening.contains(k))
            .map(|(name, scheme)| (name.clone(), scheme.clone()))
            .collect()
    }

    pub fn opened_operators(
        &self,
        opening: &Opening,
    ) -> Vec<(IText, Scheme, OperatorSpecification)> {
        self.operators
            .iter()
            .filter(|(k, op)| opening.contains(k))
            .map(|(name, (scheme, op))| (name.clone(), scheme.clone(), *op))
            .collect()
    }
}

impl ModuleEnvironment {
    pub fn add_value(&mut self, name: IText, sch: Scheme) {
        self.values.insert(name, sch);
    }

    pub fn add_operator(&mut self, name: IText, sch: Scheme, ops: OperatorSpecification) {
        self.operators.insert(name, (sch, ops));
    }

    pub fn add_module(&mut self, name: IText, module: Box<dyn Environment>) {
        self.modules.insert(name, module);
    }
}

impl<'a> Environment for ModuleEnvironment {
    fn lookup_value(&self, name: &IText) -> Option<&Scheme> {
        self.values.get(name)
    }

    fn lookup_operator(&self, name: &IText) -> Option<&(Scheme, OperatorSpecification)> {
        self.operators.get(name)
    }

    fn lookup_module(&self, name: &IText) -> Option<&Box<dyn Environment>> {
        self.modules.get(name)
    }
}
