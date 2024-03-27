use std::backtrace::Backtrace;
use std::env;
use std::fmt::Display;

// Just a little sugar around having to write 'num % 2 == 0'
pub trait IsEven {
    fn is_even(&self) -> bool;
}
impl IsEven for usize {
    fn is_even(&self) -> bool {
        *self % 2 == 0
    }
}

pub trait IsOdd {
    fn is_odd(&self) -> bool;
}
impl IsOdd for usize {
    fn is_odd(&self) -> bool {
        *self % 2 == 1
    }
}

macro_rules! print_env_mappings {
    ($env:expr) => {{
        let curr_ns = $env.get_current_namespace().unwrap();
        let hm =
            crate::values::Value::HashMap(std::rc::Rc::new(curr_ns.get_mappings_as_values()), None);
        println!("NS: {}\n{}", curr_ns, hm);
    }};
}

macro_rules! print_ns_mappings {
    ($ns:expr) => {{
        let hm =
            crate::values::Value::HashMap(std::rc::Rc::new($ns.get_mappings_as_values()), None);
        println!("NS: {}\n{}", $ns, hm);
    }};
}

pub fn is_debug_env() -> bool {
    match env::var("ENV") {
        Ok(s) => s == "debug",
        Err(_) => false,
    }
}

macro_rules! if_debug {
    ($body:block) => {
        if crate::utils::is_debug_env() {
            $body
        }
    };
}

macro_rules! backtrace {
    () => {
        println!("Backtrace:\n{}", std::backtrace::Backtrace::force_capture());
    };
}

/// Returns the first character of `s` as String
pub fn str_first(s: &str) -> String {
    s.chars().next().unwrap().to_string()
}

/// Returns the last character of `s` as String
pub fn str_last(s: &str) -> String {
    s.chars().rev().next().unwrap().to_string()
}

pub fn count_char_occurrences(s: &str, c: char) -> usize {
    s.chars().filter(|&char| char == c).count()
}

pub fn count_leading_whitespace(s: &str) -> usize {
    let mut count = 0;
    for char in s.chars() {
        if char.is_whitespace() {
            count += 1;
        } else {
            break;
        }
    }
    count
}

pub fn count_newlines(s: &str) -> usize {
    s.chars().filter(|c| *c == '\n').count()
}

pub fn remove_last_char(s: String) -> String {
    if s.is_empty() {
        return s;
    }
    let len = s.len();
    s[..len - 1].to_string()
}

pub fn print_vec<T: Display>(v: Vec<T>) -> String {
    format!(
        "[{}]",
        v.iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    )
}
