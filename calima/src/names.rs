use std::cell::RefCell;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::process::exit;
use std::rc::Rc;

struct TRc<T: Eq + Debug>(Rc<T>);

impl<T: Eq + Debug> Clone for TRc<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: Hash + Eq + Debug> Hash for TRc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let inner: &T = self.0.as_ref();
        inner.hash(state)
    }
}

impl<T: Eq + Debug> PartialEq for TRc<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0) || self.0 == other.0
    }
}

impl<T: Eq + Debug> Eq for TRc<T> {}

impl<T: Eq + Debug> Debug for TRc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0.as_ref())
    }
}

#[derive(Debug, Clone)]
pub struct SymbolName(TRc<String>, u64);

impl SymbolName {
    fn new(value: String) -> Self {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);
        let hash = hasher.finish();
        Self(TRc(Rc::new(value)), hash)
    }

    pub fn as_str(&self) -> &str {
        self.0 .0.as_ref()
    }
}

impl PartialEq for SymbolName {
    fn eq(&self, other: &Self) -> bool {
        if self.1 != other.1 {
            false
        } else {
            self.0 == other.0
        }
    }
}

impl Eq for SymbolName {}

impl PartialEq<str> for &SymbolName {
    fn eq(&self, other: &str) -> bool {
        let self_str = self.0 .0.as_ref();
        self_str == other
    }
}

impl Hash for SymbolName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.1)
    }
}

impl Display for SymbolName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0 .0.as_str())
    }
}

#[derive(Debug)]
pub struct SymbolNameInterner {
    string_hash_cache: RefCell<HashMap<u64, Vec<SymbolName>>>,
}

impl SymbolNameInterner {
    pub fn new() -> Self {
        Self {
            string_hash_cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn intern<S: AsRef<str> + Into<String>>(&self, name: S) -> SymbolName {
        let str_slice = name.as_ref();
        let mut hasher1 = DefaultHasher::new();
        str_slice.hash(&mut hasher1);
        let str_hash = hasher1.finish();
        let mut cache = self.string_hash_cache.borrow_mut();
        match cache.entry(str_hash) {
            Entry::Occupied(mut bucket) => {
                match bucket.get().iter().find(|item| item.eq(str_slice)) {
                    Some(existing) => existing.clone(),
                    None => {
                        let name = SymbolName::new(name.into());
                        bucket.get_mut().push(name.clone());
                        name
                    }
                }
            }
            Entry::Vacant(empty) => {
                let new_name = SymbolName::new(name.into());
                empty.insert(vec![new_name.clone()]);
                new_name
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::names::SymbolNameInterner;
    use std::rc::Rc;

    #[test]
    fn test_names() {
        let names = SymbolNameInterner::new();
        let n1 = names.intern("Test");
        let n2 = names.intern("Test2");
        assert_ne!(n1, n2);
        let n3 = names.intern("Test");
        assert_eq!(n1, n3);
        assert_eq!(Rc::strong_count(&n1.0 .0), 3);
    }
}
