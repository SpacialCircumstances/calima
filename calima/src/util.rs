use std::fmt::{Display, Formatter};
use std::collections::BTreeMap;

pub fn group_by<K: Ord, V, I: Iterator<Item=(K, V)>>(iter: I) -> BTreeMap<K, Vec<V>> {
    let mut map = BTreeMap::new();
    iter.for_each(|(k, v)| map.entry(k).or_insert_with(|| Vec::new()).push(v));
    map
}

pub fn format_iter<T: Display, I: Iterator<Item=T>>(iter: I, sep: &str) -> String {
    iter.map(|e| e.to_string()).collect::<Vec<String>>().join(sep)
}

pub fn format_iter_end<T: Display, I: Iterator<Item=T>>(iter: I, sep: &str) -> String {
    let mut str = format_iter(iter, sep);
    if str.len() != 0 {
        str.push_str(sep);
    }
    str
}

pub fn format_record<T>(elements: &Vec<(&str, T)>, f: &mut Formatter, sep: &str, element_sep: &str) -> std::fmt::Result where T: Display {
    let rows = format_iter(elements.iter().map(|(n, e)| format!("{}{} {}", n, sep, e)), element_sep);
    write!(f, "{{ {} }}", rows)
}

pub fn format_tuple<T>(elements: &Vec<T>, f: &mut Formatter) -> std::fmt::Result where T: Display {
    write!(f, "(")?;
    for i in 0..elements.len()-1 {
        write!(f, "{}, ", elements[i])?;
    }
    // A tuple always has one element
    write!(f, "{}", elements.last().unwrap())?;
    write!(f, ")")
}