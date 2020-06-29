use std::path::{Path, PathBuf};
use std::fmt::{Display, Formatter};
use crate::ast::TopLevelBlock;
use crate::token::Span;

//Adapted from https://gist.github.com/babygau/41d80225c4bb2acd6c6f
pub trait OwnedFunctor< A, B, F>
    where F: Fn(A) -> B {
    type Output;
    fn fmap(self, f: &F) -> Self::Output;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleIdentifier {
    full_name: String
}

impl ModuleIdentifier {
    pub fn from_name(name: &[&str]) -> Self {
        ModuleIdentifier {
            full_name: name.join(".")
        }
    }

    pub fn from_filename(name: String) -> Self {
        ModuleIdentifier {
            full_name: name
        }
    }

    pub fn components(&self) -> impl Iterator<Item=&str> {
        self.full_name.split(".")
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

#[derive(Debug)]
pub struct Module<'input, Data> {
    pub ast: TopLevelBlock<'input, Data>,
    pub name: ModuleIdentifier,
    pub path: PathBuf,
    pub depth: u32,
    pub deps: Vec<(ModuleIdentifier, Span)>,
}

impl<'input, Data, New, M: Fn(Data) -> New> OwnedFunctor<Data, New, M> for Module<'input, Data> {
    type Output = Module<'input, New>;

    fn fmap(self, f: &M) -> Self::Output {
        Module {
            ast: self.ast.fmap(f),
            name: self.name,
            path: self.path,
            depth: self.depth,
            deps: self.deps
        }
    }
}