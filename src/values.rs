use itertools::Itertools;
use std::backtrace::Backtrace;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::core;
use crate::env::Environment;
use crate::errors;
use crate::keyword::Keyword;
use crate::namespaces::Namespace;
use crate::symbol::Symbol;
use crate::var::Var;

#[derive(Debug, Clone, Default)]
pub enum Value {
    #[default]
    Nil,
    Integer(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    Symbol(Symbol),
    Keyword(Keyword),
    Var(Var),

    // STRUCTURES
    List(Rc<Vec<Value>>, Option<HashMap<Value, Value>>),
    Vector(Rc<Vec<Value>>, Option<HashMap<Value, Value>>),
    HashMap(Rc<HashMap<Value, Value>>, Option<HashMap<Value, Value>>),
    Set(Rc<HashSet<Value>>, Option<HashMap<Value, Value>>),

    // FUNCTIONS
    // Internal fmlisp functions defined into /lang
    //
    // TODO refactoring with fields: func, meta, is_macro, name
    Func(
        fn(ExprArgs, Rc<Environment>) -> ValueRes,
        Option<HashMap<Value, Value>>,
    ),
    // Internal Macros functions
    Macro(
        fn(ExprArgs, Rc<Environment>) -> ValueRes,
        Option<HashMap<Value, Value>>,
    ),
    // Lambda functions
    Lambda {
        ast: Vec<Rc<Value>>,
        env: Rc<Environment>,
        params: Vec<Rc<Value>>,
        is_macro: bool,
        meta: Option<HashMap<Value, Value>>,
    },

    // LISP
    Atom(Rc<RefCell<Value>>),
    Namespace(RefCell<Rc<Namespace>>),

    // Error
    Error(LispErr),
}

#[derive(Debug, Clone)]
pub enum LispErr {
    ErrString(String),
    ErrValue(Box<Value>),
    Error(errors::Error),
}

impl Hash for LispErr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LispErr::ErrString(s) => s.hash(state),
            LispErr::ErrValue(v) => v.hash(state),
            LispErr::Error(e) => e.hash(state),
        }
    }
}

/// Return a LispError object formatted to String
pub fn format_error(e: LispErr) -> String {
    match e {
        LispErr::ErrString(s) => s.clone(),
        LispErr::ErrValue(mv) => mv.pr_str(),
        LispErr::Error(e) => e.message(),
    }
}

pub type ExprArgs = Vec<Value>;
pub type ValueRes = Result<Value, LispErr>;

/// Returns a new error from a &str
pub fn error(s: &str) -> ValueRes {
    Err(LispErr::ErrString(s.to_string()))
}

/// /// Returns a new ArgumentError from a &str
pub fn arg_error(s: &str) -> ValueRes {
    let mut err = errors::Error::new_from_str(String::from(s));
    err.set_type("Argument");
    Err(LispErr::Error(err))
}

/// Return a new Error from string received as params.
///
/// ```
/// error!("Unexpected error")
/// ```
macro_rules! error {
    ($msg:expr) => {
        Err(LispErr::ErrString($msg.to_string()))
    };
}

macro_rules! argument_error {
    ($($arg:tt)*) => {{
        let msg = format!($($arg)*);
        let err = errors::Error::new_with_type(msg, "Argument");
        Err(LispErr::Error(err))
    }};
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(ref a), Value::Bool(ref b)) => a == b,
            (Value::Integer(ref a), Value::Integer(ref b)) => a == b,
            (Value::Float(ref a), Value::Float(ref b)) => a == b,
            (Value::Char(ref a), Value::Char(ref b)) => a == b,
            (Value::Str(ref a), Value::Str(ref b)) => a == b,
            (Value::Symbol(ref a), Value::Symbol(ref b)) => a == b,
            (Value::Keyword(ref a), Value::Keyword(ref b)) => a == b,
            (Value::List(ref a, _), Value::List(ref b, _))
            | (Value::Vector(ref a, _), Value::Vector(ref b, _))
            | (Value::List(ref a, _), Value::Vector(ref b, _))
            | (Value::Vector(ref a, _), Value::List(ref b, _)) => a == b,
            (Value::HashMap(ref a, _), Value::HashMap(ref b, _)) => a == b,
            (Value::Set(ref a, _), Value::Set(ref b, _)) => a == b,
            (Value::Func(ref fa, _), Value::Func(ref fb, _)) => fa == fb,
            (Value::Macro(ref ma, _), Value::Macro(ref mb, _)) => ma == mb,
            (Value::Lambda { .. }, Value::Lambda { .. }) => false,
            (Value::Namespace(ref na), Value::Namespace(ref nb)) => na == nb,
            _ => false,
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => "Nil".hash(state),
            Value::Integer(i) => i.hash(state),
            Value::Float(f) => {
                let mut buf = [0u8; 8];
                let bytes = f.to_ne_bytes();
                buf.copy_from_slice(&bytes);
                buf.hash(state);
            }
            Value::Bool(b) => b.hash(state),
            Value::Char(c) => c.hash(state),
            Value::Str(s) => s.hash(state),
            Value::Symbol(s) => s.hash(state),
            Value::Keyword(k) => k.hash(state),
            Value::Var(v) => v.hash(state),
            Value::List(list, _) => {
                list.hash(state);
            }
            Value::Vector(vec, _) => {
                vec.hash(state);
            }
            Value::HashMap(hash_map, _) => {
                for (key, value) in hash_map.iter() {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Set(set, _) => {
                for v in set.iter() {
                    v.hash(state);
                }
            }
            Value::Func(_, _) => {
                // Not hashing function pointers
            }
            Value::Macro(_, _) => {
                // Not hashing function pointers
            }
            Value::Lambda {
                ast,
                env: _,
                params,
                is_macro,
                meta: _,
            } => {
                // Not hashing function pointers and environment
                (ast, params, is_macro).hash(state);
            }
            Value::Namespace(ns) => {
                ns.borrow().hash(state);
            }
            Value::Atom(atom) => {
                // Not hashing atom content
                atom.borrow().hash(state);
            }
            Value::Error(err) => {
                err.hash(state);
            }
        }
    }
}

pub fn pr_seq(seq: &Vec<Value>, start: &str, end: &str, join: &str) -> String {
    let strs: Vec<String> = seq.iter().map(|x| x.to_string()).collect();
    format!("{}{}{}", start, strs.join(join), end)
}

// Implement trait Display for Value to get .to_string()
// The problem is that we don't have the possibility to differentiate to print a String as "str"
// or as str . Value::String("str").to_string() should return a String::from("str")
// but to print in the REPL I would like to have > "str" . To add this behaviour I added a function
// pr_str below
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Value::Nil => String::from("nil"),
            Value::Bool(val) => val.to_string(),
            Value::Integer(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Char(c) => format!("\\{}", c),
            Value::Str(s) => s.clone(),
            Value::Symbol(sym) => sym.to_string(),
            Value::Var(var) => var.to_string(),
            Value::Keyword(kw) => kw.to_string(),
            Value::List(l, _) => pr_seq(&l, "(", ")", " "),
            Value::Vector(v, _) => pr_seq(&v, "[", "]", " "),
            Value::HashMap(hm, _) => {
                let l = hm
                    .iter()
                    .flat_map(|(k, v)| vec![k.clone(), v.clone()])
                    .collect();
                pr_seq(&l, "{", "}", " ")
            }
            Value::Set(set, _) => {
                let strs: Vec<String> = set.iter().map(|x| x.to_string()).collect();
                format!("{{{}}}", strs.join(" "))
            }
            Value::Func(f, _) => format!("#<fn {:?}>", f),
            Value::Macro(f, _) => format!("#<macro {:?}>", f),
            Value::Lambda { ast, params, .. } => {
                let mut overloads = vec![];
                for idx in 0..params.len() {
                    overloads.push(format!(
                        "({} {})",
                        params[idx].to_string(),
                        ast[idx].to_string()
                    ))
                }
                format!("(fn {})", overloads.join("\n"))
            }
            Value::Namespace(ns) => ns.borrow().to_string(),
            Value::Atom(a) => format!("#atom {{:val {}}}", a.borrow().to_string()),
            Value::Error(err) => format_error(err.clone()),
        };
        write!(f, "{}", s)
    }
}

fn escape_str(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\\' => "\\\\".to_string(),
            _ => c.to_string(),
        })
        .collect::<Vec<String>>()
        .join("")
}

pub fn pr_seq_readability(seq: &Vec<Value>, start: &str, end: &str, join: &str) -> String {
    let strs: Vec<String> = seq.iter().map(|x| x.pr_str()).collect();
    format!("{}{}{}", start, strs.join(join), end)
}

impl Value {
    pub fn pr_str(&self) -> String {
        match self {
            Value::Nil => String::from("nil"),
            Value::Bool(true) => String::from("true"),
            Value::Bool(false) => String::from("false"),
            Value::Integer(i) => format!("{}", i),
            Value::Float(f) => format!("{}", f),
            Value::Char(c) => format!("\\{}", c),
            Value::Str(s) => format!("\"{}\"", escape_str(s)),
            Value::Symbol(sym) => sym.to_string(),
            Value::Keyword(k) => k.to_string(),
            Value::Var(v) => v.to_string(),
            Value::List(l, _) => pr_seq_readability(&**l, "(", ")", " "),
            Value::Vector(l, _) => pr_seq_readability(&**l, "[", "]", " "),
            Value::HashMap(hm, _) => {
                let l = hm
                    .iter()
                    .flat_map(|(k, v)| vec![k.clone(), v.clone()])
                    .collect();
                pr_seq_readability(&l, "{", "}", " ")
            }
            Value::Set(set, _) => {
                let strs: Vec<String> = set.iter().map(|x| x.pr_str()).collect();
                format!("#{{{}}}", strs.join(" "))
            }
            Value::Func(f, _) => format!("#<fn {:?}>", f),
            Value::Macro(f, _) => format!("#<macro {:?}>", f),
            Value::Lambda { ast, params, .. } => {
                let mut overloads = vec![];
                for idx in 0..params.len() {
                    overloads.push(format!("({} {})", params[idx].pr_str(), ast[idx].pr_str()))
                }
                format!("(fn {})", overloads.join("\n"))
            }
            Value::Namespace(ns) => ns.borrow().to_string(),
            Value::Atom(a) => format!("#atom {{:val {}}}", a.borrow().pr_str()),
            Value::Error(err) => format_error(err.clone()),
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Value::Nil => "Nil",
            Value::Integer(_) => "Integer",
            Value::Float(_) => "Float",
            Value::Bool(_) => "Boolean",
            Value::Char(_) => "Character",
            Value::Str(_) => "String",
            Value::Symbol(_) => "Symbol",
            Value::Keyword(_) => "Keyword",
            Value::Var(_) => "Var",
            Value::List(_, _) => "List",
            Value::Vector(_, _) => "Vector",
            Value::HashMap(_, _) => "HashMap",
            Value::Set(_, _) => "Set",
            Value::Func(_, _) => "Function",
            Value::Macro(_, _) => "Function",
            Value::Lambda { .. } => "Function",
            Value::Namespace(_) => "Namespace",
            Value::Atom(_) => "Atom",
            Value::Error(_) => "Error",
        }
    }

    pub fn apply(&self, args: ExprArgs, env: Rc<Environment>) -> ValueRes {
        match *self {
            Value::Func(f, _) | Value::Macro(f, _) => f(args, env.clone()),
            Value::Lambda {
                ref ast,
                ref env,
                ref params,
                ..
            } => {
                let ast_a = &**ast;
                let arity = args.len();
                if let Some((a, p)) = core::find_ast_and_params_by_arity(ast_a, params, arity) {
                    let fn_env = env.bind(p, args.clone())?;
                    return Ok(core::eval((*a).clone(), fn_env)?);
                } // else
                argument_error!(
                    "Wrong number of arguments ({}) passed to function {}",
                    arity,
                    self
                )
            }
            _ => error("Attempt to call a non-function"),
        }
    }

    pub fn count(&self) -> ValueRes {
        match self {
            Value::List(l, _) | Value::Vector(l, _) => Ok(Value::Integer(l.len() as i64)),
            Value::Str(s) => Ok(Value::Integer(s.len() as i64)),
            Value::Nil => Ok(Value::Integer(0)),
            _ => error(&format!(
                "count not supported on this type: {}",
                self.as_str()
            )),
        }
    }

    pub fn empty_q(&self) -> ValueRes {
        match self {
            Value::List(l, _) | Value::Vector(l, _) => Ok(Value::Bool(l.len() == 0)),
            Value::Str(s) => Ok(Value::Bool(s.len() == 0)),
            Value::HashMap(hm, _) => Ok(Value::Bool(hm.is_empty())),
            Value::Set(s, _) => Ok(Value::Bool(s.is_empty())),
            Value::Nil => Ok(Value::Bool(true)),
            _ => error(&format!(
                "empty? not supported on this type: {}",
                self.as_str()
            )),
        }
    }

    pub fn set(&self, val: Value) -> ValueRes {
        match self {
            Value::Var(var) => {
                var.bind_value(Rc::new(val.clone()));
                Ok(val)
            }
            _ => error(&format!(
                "set not supported on this type: {}",
                self.as_str()
            )),
        }
    }

    pub fn deref(&self) -> ValueRes {
        match self {
            Value::Atom(a) => Ok(a.borrow().clone()),
            _ => error(&format!(
                "deref not supported on this type: {}",
                self.as_str()
            )),
        }
    }

    pub fn reset_bang(&self, v: &Value) -> ValueRes {
        match self {
            Value::Atom(a) => {
                *a.borrow_mut() = v.clone();
                Ok(v.clone())
            }
            _ => error("Attempt to reset! a non-Atom"),
        }
    }

    pub fn swap_bang(&self, args: &ExprArgs, env: Rc<Environment>) -> ValueRes {
        match self {
            Value::Atom(a) => {
                let f = &args[0];
                let mut fargs = args[1..].to_vec();
                fargs.insert(0, a.borrow().clone());
                *a.borrow_mut() = f.apply(fargs, env)?;
                Ok(a.borrow().clone())
            }
            _ => error("Attempt to swap! a non-Atom"),
        }
    }

    /// Returns metadata as hashmap
    fn meta(&self) -> Option<HashMap<Value, Value>> {
        match self {
            Value::List(_, meta)
            | Value::Vector(_, meta)
            | Value::Set(_, meta)
            | Value::HashMap(_, meta)
            | Value::Func(_, meta)
            | Value::Lambda { meta, .. } => match meta {
                Some(m) => Some((*m).clone()),
                None => None,
            },
            Value::Var(var) => match var.meta() {
                Some(m) => Some((*m).clone()),
                None => None,
            },
            Value::Namespace(ns) => match ns.borrow().meta() {
                Some(m) => Some(m.clone()),
                None => None,
            },
            Value::Symbol(sym) => Some(sym.meta().clone()),
            _ => None,
        }
    }

    /// Returns Value metadata
    pub fn get_meta(&self) -> Option<Value> {
        match self.meta() {
            Some(meta) => Some(meta.to_value()),
            None => None,
        }
    }

    /// Set Value metadata. Only symbols and sequences are supported.
    /// Var metadata are set in the definition.
    pub fn with_meta(&mut self, new_meta: &Value) -> ValueRes {
        // Function to transform metadata before setting it
        fn process_meta(new_meta: &Value) -> Option<HashMap<Value, Value>> {
            match new_meta {
                // Symbols and Strings, get inserted into :tags
                Value::Symbol(_) | Value::Str(_) => {
                    let mut map: HashMap<Value, Value> = HashMap::default();
                    map.insert(key!("tag").to_value(), new_meta.clone());
                    Some(map)
                }
                // A keyword becomes a key with value true
                Value::Keyword(_) => {
                    let mut map: HashMap<Value, Value> = HashMap::default();
                    map.insert(new_meta.clone(), Value::Bool(true));
                    Some(map)
                }
                // HashMap is directly cloned (or merged?)
                Value::HashMap(hm, _) => Some((**hm).clone()),
                _ => None,
            }
        }

        match self {
            Value::List(_, ref mut meta)
            | Value::Vector(_, ref mut meta)
            | Value::Set(_, ref mut meta)
            | Value::HashMap(_, ref mut meta)
            | Value::Func(_, ref mut meta)
            | Value::Lambda { ref mut meta, .. } => {
                let m = match process_meta(new_meta) {
                    Some(meta) => meta,
                    _ => {
                        return error!(format!(
                            "Expected HashMap for setting meta on value: {} {:?}",
                            self.to_string(),
                            new_meta
                        ))
                    }
                };
                *meta = Some(m);
            }
            Value::Symbol(ref mut sym) => {
                let m = match process_meta(new_meta) {
                    Some(meta) => meta,
                    _ => {
                        return error!(format!(
                            "Expected HashMap for setting meta on value: {} {:?}",
                            self.to_string(),
                            new_meta
                        ))
                    }
                };
                *sym = sym.with_meta(m);
            }
            _ => {
                return error(&format!(
                    "with-meta not supported on this type: {}",
                    self.as_str()
                ))
            }
        }
        Ok(self.clone())
    }

    /// Returns true if the Symbol is currently set to private
    pub fn is_private(&self) -> bool {
        match self.meta() {
            Some(meta) => {
                if let Some(Value::Bool(true)) = meta.get(&Value::Keyword(key!("private"))) {
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    pub fn char_at(&self, idx: usize) -> ValueRes {
        match self {
            Value::Str(ref s) => {
                if idx < s.len() {
                    Ok(Value::Char(s.chars().nth(idx).unwrap() as char))
                } else {
                    error("Index out of bound")
                }
            }
            _ => error(&format!(
                "char_at not supported on this type: {}",
                self.as_str()
            )),
        }
    }

    pub fn to_chars_list(&self) -> ValueRes {
        match self {
            Value::Str(ref s) => {
                let chars = s.chars().map(Value::Char).collect();
                Ok(list_from_vec(chars))
            }
            _ => error(&format!(
                "to_chars_list not supported on this type: {}",
                self.as_str()
            )),
        }
    }
}

pub trait ToValue {
    fn to_value(&self) -> Value;
    fn to_rc_value(&self) -> Rc<Value> {
        Rc::new(self.to_value())
    }
}

impl ToValue for Value {
    fn to_value(&self) -> Value {
        self.clone()
    }
}

impl ToValue for Rc<Value> {
    fn to_value(&self) -> Value {
        (**self).clone()
    }
}

impl ToValue for i64 {
    fn to_value(&self) -> Value {
        Value::Integer(*self)
    }
}

impl ToValue for f64 {
    fn to_value(&self) -> Value {
        Value::Float(*self)
    }
}

impl ToValue for bool {
    fn to_value(&self) -> Value {
        Value::Bool(*self)
    }
}

impl ToValue for std::string::String {
    fn to_value(&self) -> Value {
        Value::Str(self.clone())
    }
}

impl ToValue for &str {
    fn to_value(&self) -> Value {
        Value::Str(std::string::String::from(self.clone()))
    }
}

impl ToValue for Symbol {
    fn to_value(&self) -> Value {
        Value::Symbol(self.clone())
    }
}

impl ToValue for Keyword {
    fn to_value(&self) -> Value {
        Value::Keyword(self.clone())
    }
}

impl ToValue for HashMap<Value, Value> {
    fn to_value(&self) -> Value {
        Value::HashMap(Rc::new((*self).clone()), None)
    }
}

impl ToValue for Var {
    fn to_value(&self) -> Value {
        Value::Var(self.clone())
    }
}

impl ToValue for Namespace {
    fn to_value(&self) -> Value {
        Value::Namespace(RefCell::new(Rc::new((*self).clone())))
    }
}

// type utility macros

macro_rules! list {
    () => (
        Value::List(Rc::new(vec![]), None)
    );
    ($($args:expr),*) => {{
        let v: Vec<Value> = vec![$($args),*];
        Value::List(Rc::new(v), None)
    }};
}

macro_rules! vector {
    () => (
        Value::Vector(Rc::new(vec![]), None)
    );
    ($($args:expr),*) => {{
        let v: Vec<Value> = vec![$($args),*];
        Value::Vector(Rc::new(v), None)
    }};
}

macro_rules! set {
    () => {
        Value::Set(Rc::new(HashSet::default()), None)
    };
    ($($args:expr),*) => {{
        let s = HashSet::from([$($args),*]);
        Value::Set(Rc::new(s), None)
    }};
}

// Create a list from a Vec<Value>
pub fn list_from_vec(v: Vec<Value>) -> Value {
    Value::List(Rc::new(v), None)
}

// Create a vector from a Vec<Value>
pub fn vector_from_vec(v: Vec<Value>) -> Value {
    Value::Vector(Rc::new(v), None)
}

pub fn set_from_vec(v: Vec<Value>) -> Value {
    let hashset: HashSet<Value> = v.into_iter().collect(); // Vec to Array
    Value::Set(Rc::new(hashset), None)
}

/// Create a HashMap from a vec of Values
pub fn hash_map_from_vec(kvs: Vec<Value>) -> ValueRes {
    let hm: HashMap<Value, Value> = HashMap::default();
    _assoc(hm, None, kvs)
}

/// Creates a new Value::HashMap from key-value
pub fn hash_map_from_kv(key: Value, value: Value) -> Value {
    let mut hm: HashMap<Value, Value> = HashMap::default();
    hm.insert(key, value);
    Value::HashMap(Rc::new(hm), None)
}

// Create an atom from a Value
pub fn atom(mv: &Value) -> Value {
    Value::Atom(Rc::new(RefCell::new(mv.clone())))
}

// Create a Str from a String
pub fn string(s: &str) -> Value {
    Value::Str(s.to_string())
}

// Creates a keyword from a string
pub fn keyword(s: &str) -> Value {
    Value::Keyword(key!(s))
}

// Creates a symbol from a string
pub fn symbol(s: &str) -> Value {
    Value::Symbol(sym!(s))
}

pub fn _assoc(
    mut hm: HashMap<Value, Value>,
    meta: Option<HashMap<Value, Value>>,
    kvs: ExprArgs,
) -> ValueRes {
    if kvs.len() % 2 != 0 {
        return error("odd number of elements");
    }
    for (k, v) in kvs.iter().tuples() {
        match k {
            Value::Str(_) | Value::Keyword(_) => hm.insert(k.clone(), v.clone()),
            _ => return error(&format!("Key type not supported: {}", k.as_str())),
        };
    }
    Ok(Value::HashMap(Rc::new(hm), meta.clone()))
}

pub fn _dissoc(
    mut hm: HashMap<Value, Value>,
    meta: Option<HashMap<Value, Value>>,
    ks: ExprArgs,
) -> ValueRes {
    for k in ks.iter() {
        match k {
            Value::Str(_) | Value::Keyword(_) => {
                hm.remove(k);
            }
            _ => return error(&format!("Key type not supported: {}", k.as_str())),
        }
    }
    Ok(Value::HashMap(Rc::new(hm), meta.clone()))
}

pub fn func(f: fn(ExprArgs, Rc<Environment>) -> ValueRes) -> Value {
    Value::Func(f, None)
}

pub fn macro_fn(m: fn(ExprArgs, Rc<Environment>) -> ValueRes) -> Value {
    Value::Macro(m, None)
}

pub fn var(v: Var) -> Value {
    Value::Var(v)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Print

    #[test]
    fn test_escape_str() {
        assert_eq!(escape_str("Hello\nWorld"), "Hello\\nWorld");
        assert_eq!(escape_str("Test \"quote\""), "Test \\\"quote\\\"");
        assert_eq!(escape_str("\\"), "\\\\");
        assert_eq!(escape_str(""), "");
    }

    #[test]
    fn test_pr_str_nil() {
        assert_eq!(Value::Nil.to_string(), "nil");
    }

    #[test]
    fn test_pr_str_true() {
        assert_eq!(Value::Bool(true).to_string(), "true");
    }

    #[test]
    fn test_pr_str_false() {
        assert_eq!(Value::Bool(false).to_string(), "false");
    }

    #[test]
    fn test_pr_str_integer() {
        assert_eq!(Value::Integer(42).to_string(), "42");
    }

    #[test]
    fn test_pr_str_float() {
        assert_eq!(Value::Float(3.14).to_string(), "3.14");
    }

    #[test]
    fn test_pr_str_char() {
        assert_eq!(Value::Char('c').to_string(), "\\c");
    }

    #[test]
    fn test_pr_str_str_readable() {
        assert_eq!(Value::Str("Hello".to_string()).pr_str(), "\"Hello\"");
    }

    #[test]
    fn test_pr_str_str_not_readable() {
        assert_eq!(Value::Str("Hello".to_string()).to_string(), "Hello");
    }

    #[test]
    fn test_pr_str_sym() {
        assert_eq!(Value::Symbol(sym!("abc")).to_string(), "abc");
    }

    #[test]
    fn test_pr_str_keyword() {
        assert_eq!(Value::Keyword(key!("def")).to_string(), ":def");
    }

    #[test]
    fn test_pr_str_list() {
        let list = list![Value::Integer(1), string("two"), Value::Bool(true)];
        assert_eq!(list.to_string(), "(1 two true)");
    }

    #[test]
    fn test_pr_str_vector() {
        let vector = vector![Value::Integer(1), string("two"), Value::Bool(true)];
        assert_eq!(vector.to_string(), "[1 two true]");
    }

    #[test]
    fn test_pr_str_hash() {
        let hashmap = hash_map_from_vec(vec![
            string("a"),
            Value::Integer(1),
            string("b"),
            string("two"),
            string("c"),
            Value::Bool(true),
        ])
        .unwrap();
        let result = hashmap.to_string();
        assert!(result.contains("a 1"));
        assert!(result.contains("b two"));
        assert!(result.contains("c true"));
    }

    #[test]
    fn test_pr_str_function() {
        let function = func(|_, _| Ok(Value::Nil));
        assert_eq!(function.to_string().starts_with("#<fn "), true);
        assert_eq!(function.to_string().ends_with(">"), true);
    }

    #[test]
    fn test_pr_str_atom() {
        let atom = atom(&Value::Integer(42));
        assert_eq!(atom.to_string(), "#atom {:val 42}");
    }

    // eq

    #[test]
    fn test_malvalue_equality() {
        let nil1 = Value::Nil;
        let nil2 = Value::Nil;

        let bool_true = Value::Bool(true);
        let bool_false = Value::Bool(false);

        let integer1 = Value::Integer(42);
        let integer2 = Value::Integer(42);
        let integer3 = Value::Integer(43);

        let float1 = Value::Float(3.14);
        let float2 = Value::Float(3.14);
        let float3 = Value::Float(2.71);

        let str1 = Value::Str("hello".to_string());
        let str2 = Value::Str("hello".to_string());
        let str3 = Value::Str("world".to_string());

        let sym1 = Value::Symbol(sym!("sym"));
        let sym2 = Value::Symbol(sym!("sym"));
        let sym3 = Value::Symbol(sym!("other_sym"));

        assert_eq!(nil1, nil2);
        assert_eq!(bool_true, bool_true);
        assert_eq!(integer1, integer2);
        assert_eq!(float1, float2);
        assert_eq!(str1, str2);
        assert_eq!(sym1, sym2);

        assert_ne!(bool_true, bool_false);
        assert_ne!(integer1, integer3);
        assert_ne!(float1, float3);
        assert_ne!(str1, str3);
        assert_ne!(sym1, sym3);
    }

    // format_error

    #[test]
    fn test_format_error_string() {
        let err = LispErr::ErrString("Some error message".to_string());
        assert_eq!(format_error(err), "Some error message");
    }

    #[test]
    fn test_format_error_malvalue() {
        let err_value = Value::Integer(42);
        let err = LispErr::ErrValue(Box::new(err_value));
        // Assuming pr_str() returns a string representation of the Value
        assert_eq!(format_error(err), "42"); // Check the string representation
    }

    // apply

    #[test]
    fn test_apply() {
        let env = Environment::default();
        let rc_env = Rc::new(env);

        fn sum_fn(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
            match (a[0].clone(), a[1].clone()) {
                (Value::Integer(a0), Value::Integer(a1)) => Ok(Value::Integer(a0 + a1)),
                _ => error("wrong params"),
            }
        }

        let value = Value::Func(sum_fn, None);

        assert_eq!(
            value
                .apply(vec![Value::Integer(1), Value::Integer(3)], rc_env)
                .unwrap(),
            Value::Integer(4)
        );
    }

    #[test]
    #[should_panic(expected = "Attempt to call a non-function")]
    fn test_apply_with_errors() {
        let env = Environment::default();
        let rc_env = Rc::new(env);

        // Apply should only be used on functions
        assert_eq!(
            Value::Str("test".to_string())
                .apply(vec![Value::Integer(1)], rc_env)
                .unwrap(),
            Value::Integer(4)
        );
    }

    // count

    #[test]
    fn test_count() {
        let l = list![Value::Integer(1), Value::Integer(3)];
        let v = vector![Value::Integer(1), Value::Integer(3)];
        let s = Value::Str("not a string".to_string());
        assert_eq!(l.count().unwrap(), Value::Integer(2));
        assert_eq!(v.count().unwrap(), Value::Integer(2));
        assert_eq!(s.count().unwrap(), Value::Integer(12));
    }

    #[test]
    #[should_panic(expected = "count not supported on this type: Integer")]
    fn test_count_with_errors() {
        assert_eq!(Value::Integer(2).count().unwrap(), Value::Integer(2));
    }

    // empty_q

    #[test]
    fn test_empty_q_list_empty() {
        // Test when a list is empty
        let list = list![];
        let vector = vector![];
        let string = Value::Str("".to_string());
        let nil = Value::Nil;

        assert_eq!(list.empty_q().unwrap(), Value::Bool(true));
        assert_eq!(vector.empty_q().unwrap(), Value::Bool(true));
        assert_eq!(string.empty_q().unwrap(), Value::Bool(true));
        assert_eq!(nil.empty_q().unwrap(), Value::Bool(true));
    }

    #[test]
    #[should_panic(expected = "empty? not supported on this type: Integer")]
    fn test_empty_q_panic_other_types() {
        // Test when unsupported types are provided, expecting a panic
        let unsupported = Value::Integer(42);
        let _ = unsupported.empty_q().unwrap(); // This test should panic
    }

    // deref

    #[test]
    fn test_deref() {
        let list = list![];
        let vector = vector![];
        let string = Value::Str("".to_string());
        let nil = Value::Nil;
        // Test when a list is empty
        let list_a = atom(&list);
        let vector_a = atom(&vector);
        let string_a = atom(&string);
        let nil_a = atom(&nil);

        assert_eq!(list_a.deref().unwrap(), list);
        assert_eq!(vector_a.deref().unwrap(), vector);
        assert_eq!(string_a.deref().unwrap(), string);
        assert_eq!(nil_a.deref().unwrap(), nil);
    }

    // reset!

    #[test]
    fn test_reset_bang_working() {
        // Test the function for normal operation
        let atom_value = Value::Integer(42);
        let atom = atom(&atom_value);

        let new_value = Value::Integer(100);
        assert_eq!(atom.reset_bang(&new_value).unwrap(), new_value);

        // Check if the value of the atom has been successfully updated
        let atom_cell = match atom {
            Value::Atom(ref a) => a.borrow(),
            _ => panic!("Expected an Atom"),
        };
        assert_eq!(*atom_cell, new_value);
    }

    #[test]
    #[should_panic(expected = "Attempt to reset! a non-Atom")]
    fn test_reset_bang_panic() {
        // Test for causing panic when attempting to reset a non-Atom
        let non_atom = Value::Integer(42);
        let new_value = Value::Integer(100);

        // This should panic since we are attempting to reset a non-Atom
        let _ = non_atom.reset_bang(&new_value).unwrap();
    }

    // swap!

    #[test]
    fn test_swap_bang_working() {
        // Test the function for normal operation
        // Creating an atom with initial value 42
        let atom_value = Value::Integer(42);
        let atom = atom(&atom_value);
        let env = Environment::default();
        let rc_env = Rc::new(env);

        // Define a function that increments the value by 1
        let inc_fn = Value::Func(
            |args, _| {
                let value = &args[0];
                match value {
                    Value::Integer(i) => Ok(Value::Integer(i + 1)),
                    _ => error("Expected an integer"),
                }
            },
            None,
        );

        // Call swap_bang with the increment function
        let args = vec![inc_fn];
        assert_eq!(atom.swap_bang(&args, rc_env).unwrap(), Value::Integer(43));

        // Check if the value of the atom has been successfully updated
        let atom_cell = match atom {
            Value::Atom(ref a) => a.borrow(),
            _ => panic!("Expected an Atom"),
        };
        assert_eq!(*atom_cell, Value::Integer(43));
    }

    #[test]
    #[should_panic(expected = "Attempt to swap! a non-Atom")]
    fn test_swap_bang_panic() {
        // Test for causing panic when attempting to swap a non-Atom
        let non_atom = Value::Integer(42);
        let args = vec![];
        let env = Environment::default();
        let rc_env = Rc::new(env);

        // This should panic since we are attempting to swap a non-Atom
        let _ = non_atom.swap_bang(&args, rc_env).unwrap();
    }

    // with-meta

    #[test]
    fn test_meta() {
        let mut list = list![Value::Integer(1), Value::Integer(2)];
        let params = vec![
            string("a"),
            Value::Integer(1),
            string("b"),
            Value::Integer(2),
        ];
        let hm_meta = hash_map_from_vec(params).unwrap();
        let list_meta = list.with_meta(&hm_meta).unwrap();

        assert_eq!(list_meta.get_meta().unwrap(), hm_meta);
    }

    // list!()

    #[test]
    fn test_list_macro() {
        let list1 = list![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        let expected_list1 = Value::List(
            Rc::new(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ]),
            None,
        );
        assert_eq!(list1, expected_list1);

        let list2 = list![];
        let expected_list2 = Value::List(Rc::new(vec![]), None);
        assert_eq!(list2, expected_list2);

        let list3 = list!(Value::Integer(1));
        let expected_list3 = Value::List(Rc::new(vec![Value::Integer(1)]), None);
        assert_eq!(list3, expected_list3);
    }

    // vector!

    #[test]
    fn test_vector_macro() {
        let list1 = vector![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        let expected_list1 = Value::Vector(
            Rc::new(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ]),
            None,
        );
        assert_eq!(list1, expected_list1);

        let list2 = vector![];
        let expected_list2 = Value::Vector(Rc::new(vec![]), None);
        assert_eq!(list2, expected_list2);

        let list3 = vector!(Value::Integer(1));
        let expected_list3 = Value::Vector(Rc::new(vec![Value::Integer(1)]), None);
        assert_eq!(list3, expected_list3);
    }

    // set!

    #[test]
    fn test_set_macro() {
        let set1 = set![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        let expected_set1 = Value::Set(
            Rc::new(HashSet::from([
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ])),
            None,
        );
        assert_eq!(set1, expected_set1);

        let set2 = set![];
        let expected_set2 = Value::Set(Rc::new(HashSet::new()), None);
        assert_eq!(set2, expected_set2);

        let set3 = set!(Value::Integer(1));
        let expected_set3 = Value::Set(Rc::new(HashSet::from([Value::Integer(1)])), None);
        assert_eq!(set3, expected_set3);
    }

    // list_from_vec

    #[test]
    fn test_list_from_vec() {
        let list = list_from_vec(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let expected_list = Value::List(
            Rc::new(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ]),
            None,
        );
        assert_eq!(list, expected_list);
    }

    // vector_from_vec

    #[test]
    fn test_vector_from_vec() {
        let vector = vector_from_vec(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        let expected_vector = Value::Vector(
            Rc::new(vec![
                Value::Integer(1),
                Value::Integer(2),
                Value::Integer(3),
            ]),
            None,
        );
        assert_eq!(vector, expected_vector);
    }

    // string

    #[test]
    fn test_string_fn() {
        let s = string("test");
        let expected_s = Value::Str("test".to_string());
        assert_eq!(s, expected_s);
    }

    // func

    #[test]
    fn test_func_fn() {
        fn test_this(_a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
            Ok(Value::Integer(1))
        }

        let f = func(test_this);
        let expected_f = Value::Func(test_this, None);
        assert_eq!(f, expected_f);
    }

    // char_at

    #[test]
    fn test_char_at_working() {
        let string = string("hello");

        assert_eq!(string.char_at(0).unwrap(), Value::Char('h'));
        assert_eq!(string.char_at(1).unwrap(), Value::Char('e'));
        assert_eq!(string.char_at(4).unwrap(), Value::Char('o'));
    }

    #[test]
    #[should_panic(expected = "char_at not supported")]
    fn test_char_at_panic() {
        let non_string = Value::Integer(42);
        let _ = non_string.char_at(0).unwrap();
    }

    #[test]
    #[should_panic(expected = "Index out of bound")]
    fn test_char_at_out_of_bounds() {
        let string = string("hello");
        let _ = string.char_at(10).unwrap();
    }

    // to_chars_list

    #[test]
    fn test_to_chars_list() {
        let string = string("hello");
        let expected_lst = list![
            Value::Char('h'),
            Value::Char('e'),
            Value::Char('l'),
            Value::Char('l'),
            Value::Char('o')
        ];
        assert_eq!(string.to_chars_list().unwrap(), expected_lst);
    }

    #[test]
    #[should_panic(expected = "to_chars_list not supported")]
    fn test_to_chars_list_panic() {
        Value::Integer(1).to_chars_list().unwrap();
    }
}
