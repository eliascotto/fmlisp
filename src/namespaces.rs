use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::symbol::Symbol;
use crate::values::{LispError, Value};
use crate::values::{ToValue, ValueRes};
use crate::var::Var;

/// Struct to save information on a single namespace.
/// A ns requires a specific and unique name, the refers (other ns imported)
/// and the mapping of symbols.
///
/// TODO
/// - Store also aliases defined in the namespace
/// - Find a way to save ns dockstring.
#[derive(Debug, Clone)]
pub struct Namespace {
    pub name: Symbol,
    // A hastable of symbols defined into the namespace.
    mappings: RefCell<HashMap<Symbol, Var>>,
    // Namespace aliases to other namespaces
    aliases: HashMap<Symbol, Symbol>,
    // Namespace contains meta
    meta: RefCell<Option<HashMap<Value, Value>>>,
}

impl Namespace {
    pub fn new(name: &Symbol, mappings: HashMap<Symbol, Var>) -> Namespace {
        Namespace {
            name: name.unqualified(),
            mappings: RefCell::new(mappings),
            aliases: HashMap::default(),
            meta: RefCell::new(None),
        }
    }

    /// Create a namespace from a symbol
    pub fn from_sym(name: &Symbol) -> Namespace {
        let ns = Namespace::new(name, HashMap::default());
        if !name.meta().is_empty() {
            ns.with_meta(name.meta());
        }
        ns
    }

    /// Mappings contains a certain key
    fn contains(&self, sym: &Symbol) -> bool {
        self.mappings.borrow().contains_key(sym)
    }

    /// Insert or update a Value (Var) into the namespace mapping
    pub fn insert(&self, sym: &Symbol, val: Rc<Value>) {
        if sym.has_ns() {
            panic!("Can't insert namespace-qualified symbol, {}", sym);
        }

        if !self.contains(sym) {
            match &*val {
                Value::Var(ref var) => {
                    // Insert a copy of the Var into the current namespace
                    self.mappings
                        .borrow_mut()
                        .insert(sym.unqualified(), var.clone());
                }
                _ => panic!("Expected Var for symbol {}", sym),
            }
        } else {
            let mappings = self.mappings.borrow_mut();
            let var = mappings.get(&sym.unqualified()).unwrap();
            let new_val = match &*val {
                // If insert receives a Var, the update should just
                // update the content of the Var already in the namespace
                Value::Var(ref var) => var.val.borrow().clone(),
                // Otherwise, is just a normal Value
                _ => val,
            };
            var.bind_value(new_val);
            var.with_meta(Rc::new(sym.meta()));
        }
    }

    pub fn remove(&self, sym: &Symbol) {
        if self.contains(sym) {
            self.mappings.borrow_mut().remove(&sym.unqualified());
        }
    }

    /// Try to perform a get into the namespace mappings, returns Option
    pub fn try_get(&self, sym: &Symbol) -> Option<Rc<Value>> {
        match self.mappings.borrow_mut().get(&sym.unqualified()) {
            // Warning: a new Rc is returned, so a new value is saved on the heap.
            // Modify the var with `set` will not update the value in the mapping hashtable
            Some(var) => Some(var.to_rc_value()),
            None => None,
        }
    }

    // Return a Value or raise an error
    pub fn get(&self, sym: &Symbol) -> Result<Rc<Value>, LispError> {
        match self.try_get(sym) {
            Some(val) => Ok(val),
            None => Err(LispError::ErrString(format!(
                "Undefined symbol {}",
                sym.name
            ))),
        }
    }

    /// Returns namespaces mappings
    pub fn get_mappings(&self) -> RefCell<HashMap<Symbol, Var>> {
        self.mappings.clone()
    }

    /// Returns mappings as HashMap of Values
    pub fn get_mappings_as_values(&self) -> HashMap<Value, Value> {
        self.mappings
            .borrow()
            .iter()
            .map(|(k, v)| (k.to_value(), v.to_value()))
            .collect()
    }

    /// Returns an hashmap of public symbols-var
    fn get_public_mappings(&self) -> HashMap<Symbol, Var> {
        self.mappings
            .borrow()
            .iter()
            .filter(|(_k, v)| !v.is_private())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }

    /// Returns the Var meta
    pub fn meta(&self) -> Option<HashMap<Value, Value>> {
        self.meta.borrow().clone()
    }

    /// Assign meta to Var
    pub fn with_meta(&self, meta: HashMap<Value, Value>) {
        self.meta.borrow_mut().replace(meta);
    }

    /// Add the symbols references to the namespace mapping
    pub fn add_referred_syms(&self, ref_ns: &Namespace, syms: Vec<Symbol>) -> ValueRes {
        for sym in syms {
            match ref_ns.get(&sym) {
                Ok(var) => {
                    if let Some(v) = self.mappings.borrow().get(&sym) {
                        return error!(format!(
                            "Symbol {} already refers to {} in namespace {}",
                            sym, v, self
                        ));
                    }
                    self.insert(&sym, var);
                }
                _ => {
                    return error!(format!(
                        "{} symbol not found into referred namespace {}",
                        sym, ref_ns
                    ))
                }
            }
        }
        Ok(Value::Nil)
    }

    /// Add `ref_ns` symbols to the namespace mapping.
    /// Symbols are cloned, so indipendently managed from other namespaces.
    pub fn add_referred_namespace(&self, ref_ns: &Namespace) -> ValueRes {
        for (sym, var) in ref_ns.get_public_mappings().iter() {
            // Throws an exception if name is already mapped to something else
            //  in the current namespace.
            if let Some(var) = self.mappings.borrow().get(sym) {
                return error!(format!(
                    "Symbol {} already refers to {} in namespace {}",
                    sym, var, self
                ));
            }
            self.insert(sym, var.to_rc_value());
        }
        Ok(Value::Nil)
    }

    pub fn add_alias(&mut self, alias_ns: &Symbol, source_ns: &Symbol) {
        self.aliases.insert(alias_ns.clone(), source_ns.clone());
    }

    pub fn get_alias(&self, alias_ns: &Symbol) -> Option<&Symbol> {
        self.aliases.get(alias_ns)
    }
}

impl fmt::Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#Namespace<{}>", self.name.to_string())
    }
}

impl PartialEq for Namespace {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Namespace {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (&self.name).hash(state);
    }
}

/// Struct with a HashMap containing ns Symbol-Namespace
#[derive(Debug, Clone)]
pub struct Namespaces(RefCell<HashMap<Symbol, Rc<Namespace>>>);

impl Namespaces {
    pub fn new() -> Namespaces {
        Namespaces(RefCell::new(HashMap::new()))
    }

    fn insert(&self, ns: Namespace) {
        // Use unqualified for storage
        self.0
            .borrow_mut()
            .insert(ns.name.unqualified(), Rc::new(ns));
    }

    /// Add a new namespace into the internal HashMap using symbol,
    /// but doesn't return the new namespace.
    pub fn create_namespace(&self, sym: &Symbol) {
        let ns = Namespace::from_sym(sym);

        if sym.name != "fmlisp.core" {
            if let Some(core_ns) = self.get_namespace(&sym!("fmlisp.core")) {
                let _ = ns.add_referred_namespace(&core_ns);
            }
        }

        self.insert(ns);
    }

    /// Returns `true` if namespaces containta a namespace with name `ns_sym`.
    pub fn has_namespace(&self, ns_sym: &Symbol) -> bool {
        let ns_sym = ns_sym.unqualified();
        let namespaces = self.0.borrow();
        match namespaces.get(&ns_sym) {
            Some(_) => true,
            None => false,
        }
    }

    /// Insert a binding (sym = val) *into* namespace (namespace)
    /// If namespace doesn't exist, create it
    pub fn insert_into_namespace(&self, ns_sym: &Symbol, sym: &Symbol, val: Rc<Value>) {
        let v: Symbol;
        let ns_sym = match &sym.ns {
            Some(s) => {
                v = Symbol::new(s.as_str());
                &v
            }
            None => {
                v = ns_sym.unqualified();
                &v
            }
        };

        let namespaces = self.0.borrow();
        let namespace = namespaces.get(ns_sym);
        match namespace {
            Some(namespace) => {
                namespace.insert(sym, val);
            }
            None => {
                drop(namespaces); // drop every old namespaces references
                let namespace = Namespace::from_sym(ns_sym);
                namespace.insert(sym, val);
                self.insert(namespace);
            }
        }
    }

    /// Like get, but slightly lower level; returns a None on failure rather than a
    /// Value::Condition. See docs for get
    fn try_get(&self, namespace_sym: &Symbol, sym: &Symbol) -> Option<Rc<Value>> {
        // When storing / retrieving from namespaces, we want
        // namespace_sym unqualified keys
        let mut namespace_sym = namespace_sym.unqualified();
        // Ie, a scenario like get(.. , 'clojure.core/+) or get(.., 'shortcut/+)
        let namespaces = self.0.borrow();

        // If our sym is namespace qualified, use that as our namespace
        if sym.has_ns() {
            let symbols_ns = Symbol::new(&sym.ns.as_ref().unwrap());
            let ns = namespaces.get(&namespace_sym)?;

            // If the namespace has an alias like `sym.ns`, use the source ns instead
            namespace_sym = if let Some(source_ns) = ns.get_alias(&symbols_ns) {
                source_ns.clone()
            } else {
                symbols_ns
            };
        }

        let sym = sym.unqualified();
        let namespace = namespaces.get(&namespace_sym)?;

        // If we cannot find the symbol, and its not a direct grab from a specific namespace,
        // we should see if we can find it in one of our referred namespaces or symbols
        namespace.try_get(&sym)
    }

    /// Get value of sym in namespace
    /// Note;
    /// ```
    ///  get('clojure.core,'+)
    /// ```
    /// Will be asking what '+ means in 'clojure.core, so
    /// this will only return a value if there is a 'clojure.core/+
    /// But
    /// ```
    /// get('clojure.core, 'clojure.other/+)
    /// ```
    /// Will always mean the same thing, no matter what namespace we're in; it will mean
    /// the value '+ belonging to clojure.other,  the namespace you're in is irrelevant
    ///
    /// Finally,
    /// ```
    /// get('clojure.core, 'shortcut/+)
    /// ```
    /// Will depend on what shortcut expands to in clojure.core (assuming shortcut is not an actual namespace here)
    ///
    /// As we can see, this is a relatively high level function meant to be getting _whatever_
    /// a user has typed in for a symbol while inside a namespace
    pub fn get(&self, namespace_sym: &Symbol, sym: &Symbol) -> Rc<Value> {
        match self.try_get(namespace_sym, sym) {
            Some(val) => val,
            None => Rc::new(Value::Nil),
        }
    }

    /// Returns the namespace with symbol `namespace_sym`.
    pub fn get_namespace(&self, namespace_sym: &Symbol) -> Option<Rc<Namespace>> {
        match self.0.borrow().get(namespace_sym) {
            Some(ns) => Some(ns.clone()),
            None => None,
        }
    }

    /// Returns all namespaces
    pub fn get_all_namespaces(&self) -> Vec<Rc<Namespace>> {
        let mut namespaces = Vec::new();
        for value in self.0.borrow().values() {
            namespaces.push(value.clone());
        }
        namespaces
    }

    pub fn namespace_with_meta(&self, namespace_sym: &Symbol, meta: HashMap<Value, Value>) {
        match self.get_namespace(namespace_sym) {
            Some(ns) => {
                ns.with_meta(meta);
            }
            None => {}
        }
    }

    pub fn add_referred_syms(
        &self,
        namespace_sym: &Symbol,
        referred_namespace_sym: &Symbol,
        syms: Vec<Symbol>,
    ) -> ValueRes {
        if let Some(ref_ns) = self.get_namespace(&referred_namespace_sym.unqualified()) {
            match self.0.borrow().get(namespace_sym) {
                Some(curr_ns) => curr_ns.add_referred_syms(&ref_ns, syms),
                None => panic!("Namespace {} not found!", namespace_sym),
            }
        } else {
            error!(format!("No namespace named {}", namespace_sym))
        }
    }

    pub fn add_referred_namespace(
        &self,
        namespace_sym: &Symbol,
        referred_namespace_sym: &Symbol,
    ) -> ValueRes {
        if let Some(ref_ns) = self.get_namespace(&referred_namespace_sym.unqualified()) {
            match self.0.borrow().get(namespace_sym) {
                Some(curr_ns) => curr_ns.add_referred_namespace(&ref_ns),
                None => panic!("Namespace {} not found!", namespace_sym),
            }
        } else {
            error!(format!("No namespace named {}", referred_namespace_sym))
        }
    }
}
