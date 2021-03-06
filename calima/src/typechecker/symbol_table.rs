use crate::parsing::names::SymbolName;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone)]
pub enum Location<Data: Copy> {
    External, //TODO: Remove
    Local(Data),
}

#[derive(Debug, Clone)]
struct Element<T, Data: Copy> {
    data: T,
    definition_location: Location<Data>,
}

impl<T, Data: Copy> Element<T, Data> {
    fn new(data: T, definition_location: Location<Data>) -> Self {
        Element {
            data,
            definition_location,
        }
    }
}

#[derive(Clone)]
pub struct SymbolTable<T, Data: Copy>(HashMap<SymbolName, Element<T, Data>>);

impl<T, Data: Copy> SymbolTable<T, Data> {
    pub fn new() -> Self {
        SymbolTable(HashMap::new())
    }

    pub fn add(&mut self, key: SymbolName, value: T, def_loc: Location<Data>) {
        self.0.insert(key, Element::new(value, def_loc));
    }

    pub fn get(&self, key: &SymbolName) -> Option<&T> {
        self.0.get(key).map(|e| &e.data)
    }
}
