use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::values::Value;

#[derive(Eq, Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub ns: Option<String>,
    pub meta: HashMap<Value, Value>,
}

// Create a Symbol from a String
macro_rules! sym {
    ($x:expr) => {
        crate::symbol::Symbol::new($x)
    };
}

impl Symbol {
    pub fn new(name: &str) -> Symbol {
        let mut name = name;
        let mut ns = None;

        // Symbol name can be {ns/name} so we need to split it
        if let Some(ind) = name.chars().position(|c| c == '/') {
            // Support interning of the symbol '/' for division
            if ind > 0 || name.len() > 1 {
                ns = Some(&name[..ind]);
                name = &name[ind + 1..];
            }
        }
        Symbol::new_with_ns(name, ns)
    }

    pub fn new_with_ns(name: &str, ns: Option<&str>) -> Symbol {
        let ns_string: Option<String> = match ns {
            Some(str) => Some(str.to_string()),
            None => None,
        };
        Symbol {
            name: String::from(name),
            ns: ns_string,
            meta: HashMap::new(),
        }
    }

    pub fn has_ns(&self) -> bool {
        self.ns != None
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns a new Symbol cloned, without namespace. Meta are cloned as well
    pub fn unqualified(&self) -> Symbol {
        let mut retval = self.clone();
        retval.ns = None;
        if self.has_meta() {
            retval.with_meta(self.meta());
        }
        retval
    }

    /// Returns the Symbol meta
    pub fn meta(&self) -> HashMap<Value, Value> {
        self.meta.clone()
    }

    pub fn has_meta(&self) -> bool {
        self.meta.is_empty() == false
    }

    /// Assign meta to Symbol
    pub fn with_meta(&self, meta: HashMap<Value, Value>) -> Symbol {
        Symbol {
            name: self.name.clone(),
            ns: self.ns.clone(),
            meta,
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref ns) = self.ns {
            write!(f, "{}/{}", ns, self.name)?;
        } else {
            write!(f, "{}", self.name)?;
        }
        Ok(())
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&self.name, &self.ns).hash(state);
    }
}

impl PartialEq for Symbol {
    // meta doesn't factor into equality
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ns == other.ns
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> Ordering {
        // First, compare the namespaces
        match (&self.ns, &other.ns) {
            (Some(ns1), Some(ns2)) => match ns1.cmp(ns2) {
                Ordering::Equal => (),
                ord => return ord,
            },
            (Some(_), None) => return Ordering::Less,
            (None, Some(_)) => return Ordering::Greater,
            (None, None) => (),
        }

        // If namespaces are equal or both None, compare the names
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
