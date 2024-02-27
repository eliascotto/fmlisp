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
