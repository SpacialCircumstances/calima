use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleIdentifier(String);

impl ModuleIdentifier {
    pub fn from_name(name: &[&str]) -> Self {
        Self(name.join("."))
    }

    pub fn from_filename(name: String) -> Self {
        Self(name)
    }

    pub fn components(&self) -> impl Iterator<Item = &str> {
        self.0.split('.')
    }

    pub fn path_relative_to(&self, path: &Path) -> PathBuf {
        let mut path = PathBuf::from(path);
        for el in self.components() {
            path.push(el.to_lowercase())
        }
        path.set_extension("ca");
        path
    }
}

impl Display for ModuleIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
