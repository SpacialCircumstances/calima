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
