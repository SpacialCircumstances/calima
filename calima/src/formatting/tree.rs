pub trait TreeFormat {
    fn get_precedence(&self) -> i32;
    fn format(&self) -> String;
    fn format_child<C: TreeFormat>(&self, child: C) -> String {
        if child.get_precedence() >= self.get_precedence() {
            format!("({})", child.format())
        } else {
            child.format()
        }
    }
}

impl<C> TreeFormat for &C {
    fn get_precedence(&self) -> i32 {
        self.get_precedence()
    }

    fn format(&self) -> String {
        self.format()
    }
}

pub fn format_children<R: TreeFormat, C: TreeFormat, I: Iterator<Item = C>>(
    root: &R,
    iter: I,
    sep: &str,
) -> String {
    iter.map(|e| root.format_child(e))
        .collect::<Vec<String>>()
        .join(sep)
}
