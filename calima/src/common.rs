use crate::ast::Name;
use crate::formatting::format_iter;
use crate::symbol_names::IText;
use quetta::Text;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName(Vec<Text>);

impl ModuleName {
    pub fn new(id: Vec<Text>) -> Self {
        Self(id)
    }

    pub fn from_name(name: Text) -> Self {
        Self::new(name.lift_many(|s| s.split(".")).collect())
    }

    pub fn identifier(&self) -> &Vec<Text> {
        &self.0
    }
}

impl Display for ModuleName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format_iter(self.0.iter(), "."))
    }
}

impl<Data: Copy> From<&Name<Data>> for ModuleName {
    fn from(name: &Name<Data>) -> Self {
        ModuleName::new(name.0.iter().map(|n| n.text().clone()).collect())
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct ModuleId(usize);

impl ModuleId {
    pub(crate) fn new(id: usize) -> Self {
        Self(id)
    }
}
