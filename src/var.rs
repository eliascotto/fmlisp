use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::keyword::Keyword;
use crate::symbol::Symbol;
use crate::values::{ToValue, Value};

// Var
// Bring together a namespace, a symbol and a value.
// Clojure Environment, keeps a map of Symbol-Var/Classes,
// so every single definition loaded inside the evironment is effectively
// a Var. Fmlisp will keep the same decision.
#[derive(Clone, Debug, Eq)]
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

    /// Assign key-val to meta
    pub fn set_meta(&self, key: Value, val: Value) {
        let mut meta_map = match self.meta.borrow_mut().as_mut() {
            Some(map) => Rc::get_mut(map).unwrap().to_owned(),
            None => HashMap::new(),
        };

        meta_map.insert(key, val);
        self.meta.borrow_mut().replace(Rc::new(meta_map));
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

impl Ord for Var {
    fn cmp(&self, other: &Self) -> Ordering {
        // First, compare the namespaces
        let ns_ord = self.ns.cmp(&other.ns);
        if ns_ord != Ordering::Equal {
            return ns_ord;
        }

        // If namespaces are equal, compare the symbols
        let sym_ord = self.sym.cmp(&other.sym);
        if sym_ord != Ordering::Equal {
            return sym_ord;
        }

        // If namespaces and symbols are equal, compare the values
        match (self.val.borrow().as_ref(), other.val.borrow().as_ref()) {
            (Value::Nil, Value::Nil) => Ordering::Equal,
            (Value::Nil, _) => Ordering::Less,
            (_, Value::Nil) => Ordering::Greater,
            _ => self.val.borrow().as_ref().cmp(&other.val.borrow().as_ref()),
        }
    }
}

impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
