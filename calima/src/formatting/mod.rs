use crate::symbol_names::IText;
use std::fmt::{Display, Formatter};

pub mod context;
pub mod tree;

pub fn format_iter<T: Display, I: Iterator<Item = T>>(iter: I, sep: &str) -> String {
    iter.map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(sep)
}

pub fn format_iter_end<T: Display, I: Iterator<Item = T>>(iter: I, sep: &str) -> String {
    let mut str = format_iter(iter, sep);
    if !str.is_empty() {
        str.push_str(sep);
    }
    str
}

pub fn format_record<T, Symbol>(elements: &Vec<(Symbol, T)>, sep: &str, element_sep: &str) -> String
where
    T: Display,
    Symbol: Display,
{
    let rows = format_iter(
        elements.iter().map(|(n, e)| format!("{}{} {}", n, sep, e)),
        element_sep,
    );
    format!("{{ {} }}", rows)
}

pub fn format_tuple<T>(elements: &Vec<T>, f: &mut Formatter) -> std::fmt::Result
where
    T: Display,
{
    write!(f, "(")?;
    for i in 0..elements.len() - 1 {
        write!(f, "{}, ", elements[i])?;
    }
    // A tuple always has one element
    write!(f, "{}", elements.last().unwrap())?;
    write!(f, ")")
}
