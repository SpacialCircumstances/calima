use std::cell::RefCell;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::process::exit;
use std::rc::Rc;

use quetta::Text;
use std::sync::{Arc, Mutex, RwLock};

//TODO: If we can access the underlying textdata directly, we can optimize partialeq/hash impls. Maybe move string interning feature into quetta?

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IText(Text);

impl IText {
    pub fn text(&self) -> &Text {
        &self.0
    }
}

impl Display for IText {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

struct InternedStringStore(HashMap<Text, IText>);

impl InternedStringStore {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn intern(&mut self, text: Text) -> IText {
        match self.0.entry(text.clone()) {
            Entry::Occupied(ex) => ex.get().clone(),
            Entry::Vacant(empty) => {
                let itext = IText(text);
                empty.insert(itext.clone());
                itext
            }
        }
    }
}

pub struct StringInterner(Arc<Mutex<InternedStringStore>>);

impl StringInterner {
    pub fn new() -> Self {
        StringInterner(Arc::new(Mutex::new(InternedStringStore::new())))
    }

    pub fn intern(&self, text: Text) -> IText {
        let mut l = self.0.lock().expect("Error acquiring lock");
        l.intern(text)
    }
}

#[cfg(test)]
mod tests {
    use crate::symbol_names::StringInterner;
    use std::rc::Rc;

    #[test]
    fn test_names() {
        let names = StringInterner::new();
        let n1 = names.intern("Test");
        let n2 = names.intern("Test2");
        assert_ne!(n1, n2);
        let n3 = names.intern("Test");
        assert_eq!(n1, n3);
        assert_eq!(Rc::strong_count(&n1.0 .0), 3);
    }
}
