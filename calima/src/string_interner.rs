use std::collections::HashSet;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct StringInterner {
    strings: RefCell<HashSet<String>>
}

impl StringInterner {
    pub fn new() -> Self {
        StringInterner {
            strings: RefCell::new(HashSet::new())
        }
    }

    pub fn intern<'a, 'b>(&'b self, string: &'a str) -> &'b str {
        let mut strings = self.strings.borrow_mut();
        if !strings.contains(string) {
            strings.insert(string.to_string());
        }
        let res = strings.get(string).unwrap().as_str();
        //This should be safe because we never remove values from the map
        unsafe {
            std::mem::transmute(res)
        }
    }
}