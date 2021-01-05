use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleIdentifier {
    full_name: String,
}

impl ModuleIdentifier {
    pub fn from_name(name: &[&str]) -> Self {
        ModuleIdentifier {
            full_name: name.join("."),
        }
    }

    pub fn from_filename(name: String) -> Self {
        ModuleIdentifier { full_name: name }
    }

    pub fn components(&self) -> impl Iterator<Item = &str> {
        self.full_name.split('.')
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
        write!(f, "{}", self.full_name)
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    None,
}

impl Display for Associativity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Associativity::Left => write!(f, "left"),
            Associativity::Right => write!(f, "right"),
            Associativity::None => write!(f, "none"),
        }
    }
}

impl TryFrom<&str> for Associativity {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "left" => Ok(Associativity::Left),
            "right" => Ok(Associativity::Right),
            "none" => Ok(Associativity::None),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone, Eq)]
pub enum OperatorSpecification {
    Infix(u32, Associativity),
    Prefix,
}

impl Display for OperatorSpecification {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperatorSpecification::Infix(prec, assoc) => write!(f, "infix {} {}", prec, assoc),
            OperatorSpecification::Prefix => write!(f, "prefix"),
        }
    }
}
