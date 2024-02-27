use crate::keyword::Keyword;
use crate::symbol::Symbol;
use crate::values::{ToValue, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

// Var
// Bring together a namespace, a symbol and a value.
// Clojure Environment, keeps a map of Symbol-Var/Classes,
// so every single definition loaded inside the evironment is effectively
// a Var. Fmlisp will keep the same decision.
#[derive(Clone, Debug)]
pub struct Var {
    // Namespace
    pub ns: Symbol,
    // Symbol
    pub sym: Symbol,
    // Value assigned the symbol
    pub val: RefCell<Rc<Value>>,
    // Reference to Obj meta
    meta: RefCell<Option<Rc<HashMap<Value, Value>>>>,
}

impl Var {
    pub fn new(ns: Symbol, sym: Symbol, value: Rc<Value>) -> Var {
        Var {
            ns,
            sym,
            val: RefCell::new(value),
            meta: RefCell::new(None),
        }
    }

    pub fn new_without_val(ns: Symbol, sym: Symbol) -> Var {
        Var {
            ns,
            sym,
            val: RefCell::new(Value::Nil.to_rc_value()),
            meta: RefCell::new(None),
        }
    }

    /// Returns the value of the Var
    pub fn deref(&self) -> Rc<Value> {
        self.val.borrow().clone()
    }

    /// Set the value of Var to `val`
    pub fn bind_value(&self, val: Rc<Value>) {
        if let Value::Var(_) = &*val {
            panic!("Assigning a Var to a Var");
        }
        self.val.replace(val);
    }

    /// Returns the Var meta
    pub fn meta(&self) -> Option<Rc<HashMap<Value, Value>>> {
        self.meta.borrow().clone()
    }

    /// Assign meta to Var
    pub fn with_meta(&self, meta: Rc<HashMap<Value, Value>>) {
        self.meta.borrow_mut().replace(meta);
    }

    pub fn is_private(&self) -> bool {
        match self.meta() {
            Some(meta) => {
                if let Some(v) = meta.get(&key!("private").to_value()) {
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }
}

impl PartialEq for Var {
    // Remember; meta doesn't factor into equality
    fn eq(&self, other: &Self) -> bool {
        self.ns == other.ns && self.sym == other.sym
    }
}

impl Hash for Var {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&self.ns, &self.sym).hash(state);
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#'{}/{}", self.ns, self.sym)
    }
}
