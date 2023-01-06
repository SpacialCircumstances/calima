use crate::ast::Name;
use crate::formatting::format_iter;
use crate::symbol_names::IText;
use quetta::Text;
use serde::{Deserialize, Serialize};
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

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct ModuleId(usize);

impl ModuleId {
    pub const PRELUDE: ModuleId = Self(0);
    pub const FIRST_ID: usize = 1;

    pub(crate) fn new(id: usize) -> Self {
        Self(id)
    }
}

impl Display for ModuleId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{}", self.0)
    }
}
