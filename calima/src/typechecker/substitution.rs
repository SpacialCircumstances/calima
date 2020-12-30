use std::ops::Index;

#[derive(Debug, Clone)]
pub struct Substitution<T: Clone> {
    subst: Vec<Option<T>>,
}

impl<T: Clone> Substitution<T> {
    pub fn new() -> Self {
        Substitution {
            subst: (1..10).map(|_| None).collect(),
        }
    }

    pub fn add(&mut self, idx: usize, value: T) {
        if idx >= self.subst.len() {
            self.subst.resize(idx + 1, Option::None);
        }
        self.subst[idx] = Some(value);
    }
}

impl<T: Clone> Index<usize> for Substitution<T> {
    type Output = Option<T>;

    fn index(&self, index: usize) -> &Self::Output {
        self.subst.get(index).unwrap_or(&Option::None)
    }
}
