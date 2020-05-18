use std::collections::HashSet;

#[derive(Debug, Clone)]
struct StringInterner {
    strings: HashSet<String>
}

impl StringInterner {
    pub fn new() -> Self {
        StringInterner {
            strings: HashSet::new()
        }
    }

    pub fn intern(&mut self, string: &str) -> &str {
        if !self.strings.contains(string) {
            self.strings.insert(string.to_string());
        }
        self.strings.get(string).unwrap()
    }
}