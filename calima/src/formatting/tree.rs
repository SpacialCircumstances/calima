pub trait TreeFormat {
    fn get_precedence(&self) -> i32;
    fn format(&self) -> String;
    fn format_child<C: TreeFormat>(&self, child: C) -> String;
}
