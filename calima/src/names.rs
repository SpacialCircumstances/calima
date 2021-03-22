use crate::symbol_names::SymbolName;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SymbolSource {
    Local,
    External,
}

#[derive(Debug, Eq, PartialEq)]
pub struct SymbolData(Vec<SymbolName>, SymbolSource);

#[derive(Debug, Eq, PartialEq)]
pub struct Symbol(Rc<SymbolData>);

impl Clone for Symbol {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
