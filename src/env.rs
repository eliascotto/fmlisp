use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error_fmt;
use crate::namespaces::{Namespace, Namespaces};
use crate::symbol::Symbol;
use crate::values::LispErr::{self, ErrString};
use crate::values::{list_from_vec, ToValue, Value, ValueRes};
use crate::var::Var;

/// Keeps track of internal environment data:
/// the current namespace (symbol) that we're in and
/// all the other namespaces.
#[derive(Debug, Clone)]
struct EnvironmentItem {
    current_ns: RefCell<Symbol>,
    namespaces: Namespaces,
}

impl EnvironmentItem {
    fn new(ns_name: &str) -> EnvironmentItem {
        let current_ns = Symbol::new(ns_name); // default ns
        let namespaces = Namespaces::new();
        namespaces.create_namespace(&current_ns);
        EnvironmentItem {
            current_ns: RefCell::new(current_ns),
            namespaces,
        }
    }

    /// Creates a new namespace if doesn't existsn,
    /// then set the current namespace to the Symbol.
    fn change_or_create_namespace(&self, sym: &Symbol) {
        if !self.has_namespace(sym) {
            self.namespaces.create_namespace(sym);
        }
        if self.current_ns.borrow().clone() != sym.unqualified() {
            self.current_ns.replace(sym.unqualified());
            self.update_ns_symbol();
        }

        // Update namespace metadata
        if sym.has_meta() {
            self.namespaces
                .namespace_with_meta(&self.current_ns.borrow(), sym.meta());
        }
    }

    fn update_ns_symbol(&self) {
        // Set *ns* symbol to the current namespace
        let var = self.get_from_current_namespace(&Symbol::new("*ns*"));
        let ns_rc = self.get_current_namespace().unwrap();

        if let Value::Nil = &*var {
            // Create a new Var since the symbol is not yet inside the namespace
            let var = Var::new(
                self.current_ns.borrow().clone(),
                Symbol::new("*ns*"),
                Rc::new(Value::Namespace(RefCell::new(ns_rc.clone()))),
            );
            ns_rc.insert(&Symbol::new("*ns*"), var.to_rc_value());
        } else {
            // Set the new namespace into the Var
            let _ = var.set(Value::Namespace(RefCell::new(ns_rc.clone())));
            // We update the var in the mapping since `get_from_current_namespace` returns only a copy
            ns_rc.insert(&Symbol::new("*ns*"), var);
        }
    }

    fn has_namespace(&self, sym: &Symbol) -> bool {
        self.namespaces.has_namespace(sym)
    }

    /// Return the current active namespace as Symbol
    fn get_current_namespace_symbol(&self) -> Symbol {
        self.current_ns.borrow().clone()
    }

    /// Returns the current namespace as Namespace
    fn get_current_namespace(&self) -> Option<Rc<Namespace>> {
        self.namespaces
            .get_namespace(&self.get_current_namespace_symbol())
    }

    pub fn get_all_namespaces(&self) -> Vec<Rc<Namespace>> {
        self.namespaces.get_all_namespaces()
    }

    /// Find a namespace and return it or None
    fn find_namespace(&self, ns_sym: &Symbol) -> Option<Rc<Namespace>> {
        self.namespaces.get_namespace(ns_sym)
    }

    fn insert_into_namespace(&self, ns_sym: &Symbol, sym: Symbol, val: Rc<Value>) {
        self.namespaces.insert_into_namespace(ns_sym, &sym, val)
    }

    fn insert_into_current_namespace(&self, sym: Symbol, val: Rc<Value>) {
        match val.as_ref() {
            Value::Var(_) => {
                self.namespaces
                    .insert_into_namespace(&*self.current_ns.borrow(), &sym, val);
            }
            _ => {
                panic!("Value for insert_into_current_namespace must be of type Value::Var");
            }
        }
    }

    fn get_from_namespace(&self, namespace: &Symbol, sym: &Symbol) -> Rc<Value> {
        self.namespaces.get(namespace, sym)
    }

    fn get_from_current_namespace(&self, sym: &Symbol) -> Rc<Value> {
        self.get_from_namespace(&self.current_ns.borrow(), sym)
    }

    fn add_referred_syms(
        &self,
        namespace_sym: &Symbol,
        referred_namespace_sym: &Symbol,
        syms: Vec<Symbol>,
    ) -> ValueRes {
        self.namespaces
            .add_referred_syms(namespace_sym, referred_namespace_sym, syms)
    }

    fn add_referred_namespace(
        &self,
        namespace_sym: &Symbol,
        referred_namespace_sym: &Symbol,
    ) -> ValueRes {
        self.namespaces
            .add_referred_namespace(namespace_sym, referred_namespace_sym)
    }
}

/// Stores our namespaces and our current namespace, which themselves personally store our symbols
/// mapped to values. Differentiate between a local and a global environment.
/// A module is a new global environment, while a `let` is a local one.
#[derive(Debug, Clone)]
pub enum Environment {
    MainEnvironment(EnvironmentItem),
    // Contains Parent-Local_mapping
    LocalEnvironment(Rc<Environment>, RefCell<HashMap<Symbol, Rc<Value>>>),
}

impl Default for Environment {
    /// Creates an empty `Environment` with `user` as namesapce.
    fn default() -> Environment {
        Environment::new_main_environment("fmlisp.lang")
    }
}

impl Environment {
    pub fn new_main_environment(ns_name: &str) -> Environment {
        Environment::MainEnvironment(EnvironmentItem::new(ns_name))
    }

    pub fn new_local_environment(outer_environment: Rc<Environment>) -> Environment {
        Environment::LocalEnvironment(outer_environment, RefCell::new(HashMap::new()))
    }

    /// Create a new local environment, with the current environment as `outer_environment`
    pub fn new_local(&self) -> Environment {
        Environment::new_local_environment(Rc::new((*self).clone()))
    }

    pub fn get_main_environment(&self) -> &Self {
        match self {
            Environment::MainEnvironment(_) => self,
            Environment::LocalEnvironment(parent_env, ..) => parent_env.get_main_environment(),
        }
    }

    pub fn get_outer_environment(&self) -> Rc<Environment> {
        match self {
            Environment::MainEnvironment(_) => Rc::new((*self).clone()),
            Environment::LocalEnvironment(parent_env, ..) => parent_env.get_outer_environment(),
        }
    }

    pub fn has_namespace(&self, symbol: &Symbol) -> bool {
        match self.get_main_environment() {
            Environment::MainEnvironment(env) => env.has_namespace(symbol),
            Environment::LocalEnvironment(..) => panic!(
                "get_main_environment() returns LocalEnvironment,\
		             but by definition should only return MainEnvironment"
            ),
        }
    }

    /// Changes the current namespace, or creates one first if
    /// namespace doesn't already exists.
    pub fn change_or_create_namespace(&self, ns_sym: &Symbol) {
        match self.get_main_environment() {
            Environment::MainEnvironment(env) => {
                env.change_or_create_namespace(ns_sym);
            }
            Environment::LocalEnvironment(..) => panic!(
                "get_main_environment() returns LocalEnvironment,\
		             but by definition should only return MainEnvironment"
            ),
        }
    }

    pub fn get_current_namespace(&self) -> Option<Rc<Namespace>> {
        match self {
            Environment::MainEnvironment(env) => env.get_current_namespace(),
            Environment::LocalEnvironment(parent_env, ..) => {
                parent_env.get_main_environment().get_current_namespace()
            }
        }
    }

    pub fn get_current_namespace_symbol(&self) -> Symbol {
        match self.get_main_environment() {
            Environment::MainEnvironment(env) => env.get_current_namespace_symbol(),
            Environment::LocalEnvironment(parent_env, ..) => parent_env
                .get_main_environment()
                .get_current_namespace_symbol(),
        }
    }

    pub fn get_current_namespace_name(&self) -> String {
        self.get_current_namespace_symbol().name.clone()
    }

    /// Insert a binding into an arbitrary namespace
    fn insert_into_namespace(&self, namespace: &Symbol, sym: Symbol, val: Rc<Value>) {
        match self.get_main_environment() {
            Environment::MainEnvironment(env) => env.insert_into_namespace(namespace, sym, val),
            Environment::LocalEnvironment(..) => panic!(
                "get_main_environment() returns LocalEnvironment,\
		                 but by definition should only return MainEnvironment"
            ),
        }
    }

    pub fn insert_into_current_namespace(&self, sym: Symbol, val: Rc<Value>) {
        match self.get_main_environment() {
            Environment::MainEnvironment(env) => env.insert_into_current_namespace(sym, val),
            Environment::LocalEnvironment(..) => panic!(
                "get_main_environment() returns LocalEnvironment,\
		                 but by definition should only return MainEnvironment"
            ),
        }
    }

    pub fn add_referred_syms(
        &self,
        referred_namespace_sym: &Symbol,
        syms: Vec<Symbol>,
    ) -> ValueRes {
        match self.get_main_environment() {
            Environment::MainEnvironment(env_val) => {
                let namespace_sym = self.get_current_namespace_symbol();
                env_val.add_referred_syms(&namespace_sym, referred_namespace_sym, syms)
            }
            Environment::LocalEnvironment(..) => panic!(
                "get_main_environment() returns LocalEnvironment,\
		             but by definition should only return MainEnvironment"
            ),
        }
    }

    /// Refer a namespace into the current namespace
    pub fn add_referred_namespace(&self, referred_namespace_sym: &Symbol) -> ValueRes {
        match self {
            Environment::MainEnvironment(env_val) => {
                let namespace_sym = self.get_current_namespace_symbol();
                env_val.add_referred_namespace(&namespace_sym, referred_namespace_sym)
            }
            Environment::LocalEnvironment(..) => self
                .get_main_environment()
                .add_referred_namespace(referred_namespace_sym),
        }
    }

    /// Find a namespace and return it or None
    pub fn find_namespace(&self, ns_sym: &Symbol) -> Option<Rc<Namespace>> {
        match self {
            Environment::MainEnvironment(env) => env.find_namespace(ns_sym),
            Environment::LocalEnvironment(parent_env, ..) => {
                parent_env.get_main_environment().find_namespace(ns_sym)
            }
        }
    }

    /// Returns all namespaces
    pub fn get_all_namespaces(&self) -> Vec<Rc<Namespace>> {
        match self {
            Environment::MainEnvironment(env) => env.get_all_namespaces(),
            Environment::LocalEnvironment(parent_env, ..) => {
                parent_env.get_main_environment().get_all_namespaces()
            }
        }
    }

    /// Insert into the environment context; the local bindings,
    /// or the current namespace, if this is top level.
    /// Warning: use it only if `val` is of type Value::Var.
    /// For instance,
    /// ```clojure
    ///   (def a 1)      ;; => main_environment.insert(a,1)
    ///   (let [a 1] ..) ;; => local_environment.insert(a,1)
    pub fn insert(&self, sym: Symbol, val: Rc<Value>) {
        match self {
            Environment::MainEnvironment(_) => {
                self.insert_into_current_namespace(sym, val);
            }
            Environment::LocalEnvironment(_, mappings) => match val.as_ref() {
                Value::Var(_) => {
                    mappings.borrow_mut().insert(sym, val);
                }
                _ => {
                    panic!("Value inside local environment must be of type Value::Var");
                }
            },
        }
    }

    /// Insert into the environment around you;  the local bindings,
    /// or the current namespace, if this is top level. A new Value::Var will be created.
    /// Warning: use it only if `val` is NOT type Value::Var.
    pub fn insert_var(&self, sym: Symbol, val: Rc<Value>) -> Rc<Value> {
        match val.as_ref() {
            Value::Var(_) => {
                panic!("Value for insert_var should not be of type Value::Var");
            }
            _ => {
                let var = Var::new(self.get_current_namespace_symbol(), sym.clone(), val);
                let var_val = Value::Var(var).to_rc_value();
                self.insert(sym, var_val.clone());
                var_val
            }
        }
    }

    /// Transform the string to Symbol and add the value to the env as a Var
    pub fn insert_str(&self, key: &str, val: Rc<Value>) {
        let sym = Symbol::new(key);
        let var = Var::new(self.get_current_namespace_symbol(), sym.clone(), val);
        self.insert(sym, Value::Var(var).to_rc_value());
    }

    pub fn print_debug(&self, sym: &Symbol) {
        match self.get_main_environment() {
            Environment::MainEnvironment(env) => {
                let val = env.get_from_current_namespace(sym);
                match &*val {
                    Value::Var(var) => {
                        println!(
                            "DEBUG: {} {} {} {} {}",
                            sym,
                            val,
                            var,
                            var.val.borrow(),
                            var.val.borrow().as_str()
                        );
                    }
                    _ => {
                        println!("DEBUG: {} {} {}", sym, val, val.pr_str());
                    }
                }
            }
            Environment::LocalEnvironment(..) => panic!(
                "get_main_environment() returns LocalEnvironment,\
		             but by definition should only return MainEnvironment"
            ),
        }
    }

    /// Get closest value: first try local environment, then
    /// try our main environment. If symbol is namespace qualified,
    /// use the namespace, if exists, otherwise use the current ns.
    pub fn get(&self, sym: &Symbol) -> Result<Rc<Value>, LispErr> {
        match self {
            Environment::MainEnvironment(env) => match sym.ns.clone() {
                Some(ns) => {
                    let ns_sym = Symbol::new(ns.as_str());
                    if env.has_namespace(&ns_sym) {
                        Ok(env.get_from_namespace(&ns_sym, &sym))
                    } else {
                        error_fmt!("Namespace {} not found", ns_sym)
                    }
                }
                None => Ok(env.get_from_current_namespace(&sym)),
            },
            Environment::LocalEnvironment(parent_env, mappings) => {
                if sym.ns != None {
                    self.get_main_environment().get(sym)
                } else {
                    match mappings.borrow().get(sym) {
                        Some(val) => Ok(Rc::clone(val)),
                        None => parent_env.get(sym),
                    }
                }
            }
        }
    }

    /// Returns the value of the Var associated with the Symbol in the current namespace
    pub fn get_symbol_value(&self, sym: &Symbol) -> Result<Option<Rc<Value>>, LispErr> {
        match self.get(sym)?.as_ref() {
            Value::Var(v) => Ok(Some(v.val.borrow().clone())),
            _ => Ok(None),
        }
    }

    pub fn bind(&self, mbinds: Rc<Value>, exprs: Vec<Value>) -> Result<Rc<Environment>, LispErr> {
        let new_env = Environment::new_local_environment(Rc::new((*self).clone()));
        match (*mbinds).clone() {
            Value::Vector(binds, _) => {
                for (i, b) in binds.iter().enumerate() {
                    match b {
                        Value::Symbol(s) if s.name() == "&" => {
                            if let Value::Symbol(sym) = binds[i + 1].clone() {
                                let mut params = exprs[i..].to_vec();
                                let mut is_nil = false;

                                // Editing binding behaviour for variadic functions.
                                // This is due to the fact that FMLisp supports TCO and
                                // doesn't use `recur` for recursive functions.
                                // We need to manage special cases:
                                //   - functions that return nil
                                //   - functions that return a list should not become a list of lists
                                if params.len() == 1 {
                                    match params[0].clone() {
                                        // If there's a single list of params in a variadic function,
                                        // convert is to a simple list.
                                        Value::List(l, _) => params = l.to_vec(),
                                        // If a single value that is nil, set value to Nil
                                        Value::Nil => is_nil = true,
                                        _ => {}
                                    }
                                }

                                let val = if is_nil {
                                    Rc::new(Value::Nil)
                                } else {
                                    Rc::new(list_from_vec(params))
                                };
                                let var =
                                    Var::new(self.get_current_namespace_symbol(), sym.clone(), val);
                                new_env.insert(sym, Value::Var(var).to_rc_value());
                            } else {
                                return error_fmt!(
                                    "Symbol expected, found {} instead",
                                    binds[i + 1].pr_str()
                                );
                            }
                            break;
                        }
                        _ => {
                            if let Value::Symbol(sym) = b.clone() {
                                let val = Rc::new(exprs[i].clone());
                                let var =
                                    Var::new(self.get_current_namespace_symbol(), sym.clone(), val);
                                new_env.insert(sym, Value::Var(var).to_rc_value());
                            } else {
                                return error_fmt!("Symbol expected, found {} instead", b.pr_str());
                            }
                        }
                    }
                }
                Ok(Rc::new(new_env))
            }
            _ => Err(ErrString("env_bind binds not a Vector".to_string())),
        }
    }

    pub fn to_rc(&self) -> Rc<Environment> {
        Rc::new(self.clone())
    }
}
