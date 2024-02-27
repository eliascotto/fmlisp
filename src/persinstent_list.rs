use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};

use crate::traits;
use crate::values::Value;

#[derive(Debug, Clone)]
pub struct PersistentList {
    pub list: VecDeque<Value>,
    pub meta: Option<HashMap<Value, Value>>,
}

impl PersistentList {
    pub fn new() -> Self {
        PersistentList {
            list: VecDeque::new(),
            meta: None,
        }
    }

    /// Returns the optional meta of the List
    pub fn meta(&self) -> Option<HashMap<Value, Value>> {
        self.meta.clone()
    }

    /// Returns a new PersistentList instance with associated meta
    pub fn with_meta(&self, meta: HashMap<Value, Value>) -> Self {
        PersistentList {
            list: self.list.clone(),
            meta: Some(meta),
        }
    }

    /// Returns the PersistentList into a Vec
    pub fn to_vec(&self) -> Vec<Value> {
        self.list.into_iter().collect()
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }
}

impl PartialEq for PersistentList {
    fn eq(&self, other: &PersistentList) -> bool {
        self.list == other.list
    }
}

impl Hash for PersistentList {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.list.hash(state)
    }
}

impl traits::IMeta for PersistentList {
    fn meta(&self) -> Option<HashMap<Value, Value>> {
        self.meta()
    }
}

impl traits::IObj for PersistentList {
    fn with_meta(&self, meta: HashMap<Value, Value>) -> Self {
        self.with_meta(meta)
    }
}
