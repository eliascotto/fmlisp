use crate::symbol::Symbol;
use std::cmp::Ordering;
use std::fmt;
use std::hash::Hash;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct Keyword {
    // In Clojure proper,  a Keyword wraps a Symbol to share their ..symbolic functionality
    pub sym: Symbol,
}

// Create a Symbol from a String
macro_rules! key {
    ($x:expr) => {
        Keyword::new($x)
    };
}

impl Keyword {
    pub fn new(name: &str) -> Keyword {
        Keyword {
            sym: Symbol::new(name),
        }
    }

    pub fn new_with_ns(ns: &str, name: &str) -> Keyword {
        Keyword {
            sym: Symbol::new_with_ns(name, Some(ns)),
        }
    }

    pub fn name(&self) -> &str {
        self.sym.name()
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.sym.ns != None {
            write!(f, ":{:?}/{}", self.sym.ns, self.sym.name)
        } else {
            write!(f, ":{}", self.sym.name)
        }
    }
}

impl Ord for Keyword {
    fn cmp(&self, other: &Self) -> Ordering {
        self.sym.cmp(&other.sym)
    }
}

impl PartialOrd for Keyword {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
