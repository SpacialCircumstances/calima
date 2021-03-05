use std::cell::RefCell;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Name(TRc<String>);

impl Name {
    fn new(value: String) -> Self {
        Self(TRc(Rc::new(value)))
    }
}

impl PartialEq<str> for &Name {
    fn eq(&self, other: &str) -> bool {
        let self_str = self.0 .0.as_ref();
        self_str == other
    }
}

#[derive(Debug)]
pub struct NameInterner {
    string_hash_cache: RefCell<HashMap<u64, Vec<Name>>>,
}

impl NameInterner {
    pub fn new() -> Self {
        Self {
            string_hash_cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn intern<S: AsRef<str> + Into<String>>(&self, name: S) -> Name {
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
                        let name = Name::new(name.into());
                        bucket.get_mut().push(name.clone());
                        name
                    }
                }
            }
            Entry::Vacant(empty) => {
                let new_name = Name::new(name.into());
                empty.insert(vec![new_name.clone()]);
                new_name
            }
        }
    }
}
