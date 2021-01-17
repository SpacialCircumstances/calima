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
pub struct SymbolTable<'a, T, Data: Copy>(HashMap<&'a str, Element<T, Data>>);

impl<'a, T, Data: Copy> SymbolTable<'a, T, Data> {
    pub fn new() -> Self {
        SymbolTable(HashMap::new())
    }

    pub fn add(&mut self, key: &'a str, value: T, def_loc: Location<Data>) {
        self.0.insert(key, Element::new(value, def_loc));
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        self.0.get(key).map(|e| &e.data)
    }
}
