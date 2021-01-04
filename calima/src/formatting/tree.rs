pub trait TreeFormat {
    fn get_precedence(&self) -> i32;
    fn format(&self) -> String;
    fn format_child(&self, child: &Self) -> String {
        if self.get_precedence() < child.get_precedence() {
            format!("({})", child.format())
        } else {
            child.format()
        }
    }
}

pub fn format_children<'a, R: TreeFormat + 'a, I: Iterator<Item = &'a R>>(
    root: &R,
    iter: I,
    sep: &str,
) -> String {
    iter.map(|e| root.format_child(e))
        .collect::<Vec<String>>()
        .join(sep)
}
