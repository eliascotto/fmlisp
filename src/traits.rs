use crate::values::Value;
use std::collections::HashMap;
use std::fmt::Debug;

pub trait IMeta: Debug {
    // Meta is implemented as an optional HashMap. If not set, returns None and the REPL func will print Value::Nil
    fn meta(&self) -> Option<HashMap<Value, Value>>;
}

pub trait IObj: IMeta + Debug {
    fn with_meta(&self, meta: HashMap<Value, Value>) -> Self;
}
