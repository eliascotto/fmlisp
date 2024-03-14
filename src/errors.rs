use crate::values::{self, Value};
use std::fmt::{self, Debug, Display};
use std::hash::{Hash, Hasher};

pub trait Throwable: Debug {}

#[derive(Debug, Clone)]
pub struct Error {
    message: Box<Value>,
    trace: Vec<Box<Value>>,
    kind: Option<String>,    // type
    details: Option<String>, // extra error message details
}

impl Error {
    pub fn new(msg: Value) -> Error {
        Error {
            message: Box::new(msg),
            trace: vec![],
            kind: None,
            details: None,
        }
    }

    pub fn new_with_type(msg: String, kind: &str) -> Error {
        Error {
            message: Box::new(Value::Str(msg)),
            trace: vec![],
            kind: Some(String::from(kind)),
            details: None,
        }
    }

    pub fn new_from_str(msg: String) -> Error {
        Error::new(Value::Str(msg))
    }

    pub fn message(&self) -> String {
        if let Value::Str(ref s) = *self.message {
            s.clone()
        } else {
            panic!("Value is not of type Value::Str");
        }
    }

    pub fn set_message(&mut self, msg: &str) {
        let val = Value::Str(String::from(msg));
        self.message = Box::new(val);
    }

    pub fn details(&self) -> Option<String> {
        self.details.clone()
    }

    pub fn set_details(&mut self, msg: &str) {
        self.details = Some(String::from(msg));
    }

    pub fn has_type(&self) -> bool {
        self.kind.is_some()
    }

    pub fn set_type(&mut self, s: &str) {
        self.kind = Some(String::from(s));
    }

    pub fn get_type(&self) -> String {
        format!(
            "{}Error",
            self.kind.clone().unwrap_or_else(|| String::new())
        )
    }

    pub fn to_lisp_error(&self) -> values::LispErr {
        values::LispErr::Error(self.clone())
    }

    pub fn trace_to_vec(&self) -> Vec<String> {
        self.trace
            .iter()
            .map(|t| {
                if let Value::Str(ref s) = **t {
                    s.clone() // Extract the String value
                } else {
                    panic!("Value is not of type Value::Str");
                }
            })
            .collect::<Vec<String>>()
    }

    pub fn add_trace(&mut self, t: Value) {
        self.trace.push(Box::new(t));
    }

    pub fn has_trace(&self) -> bool {
        !self.trace.is_empty()
    }

    pub fn trace_len(&self) -> usize {
        self.trace.len()
    }

    pub fn fmt_trace(&self, enumerate: bool) -> String {
        let v = self
            .trace
            .iter()
            .enumerate()
            .map(|(i, t)| {
                if enumerate {
                    format!("  {}: {}", i, t.pr_str())
                } else {
                    format!("{}", t.pr_str())
                }
            })
            .collect::<Vec<String>>();
        v.join("\n")
    }
}

impl Throwable for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "#error {{\n :message {}\n :trace\n {:?}}}",
            self.message.pr_str(),
            self.trace_to_vec()
        )
    }
}

impl Hash for Error {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.message.hash(state);
        self.trace.hash(state);
    }
}
