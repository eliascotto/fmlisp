use itertools::Itertools;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::namespaces::{Namespace, Namespaces};
use crate::symbol::Symbol;
use crate::values::LispError::{self, ErrString, ErrValue};
use crate::values::{list_from_vec, ExprArgs, ToValue, Value, ValueRes};
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
        self.current_ns.replace(sym.unqualified());
        self.update_ns_symbol();

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
        Environment::new_main_environment("fmlisp.core")
    }
}

// Utils
fn qq_iter(elts: &ExprArgs) -> Value {
    let mut acc = list![];
    for elt in elts.iter().rev() {
        if let Value::List(v, _) = elt {
            if v.len() == 2 {
                if let Value::Symbol(ref s) = v[0] {
                    if s.name() == "unquote-splicing" {
                        acc = list![Value::Symbol(sym!("concat")), v[1].clone(), acc];
                        continue;
                    }
                }
            }
        }
        acc = list![Value::Symbol(sym!("cons")), quasiquote(&elt), acc];
    }
    acc
}

fn quasiquote(ast: &Value) -> Value {
    match ast {
        Value::List(v, _) => {
            if v.len() == 2 {
                if let Value::Symbol(ref s) = v[0] {
                    if s.name() == "unquote" {
                        return v[1].clone();
                    }
                }
            }
            qq_iter(&v)
        }
        Value::Vector(v, _) => list![Value::Symbol(sym!("vec")), qq_iter(&v)],
        Value::HashMap(_, _) | Value::Symbol(_) => {
            list![Value::Symbol(sym!("quote")), ast.clone()]
        }
        _ => ast.clone(),
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

    fn get_main_environment(&self) -> &Self {
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
            Environment::LocalEnvironment(..) => panic!(
                "In get_current_namespace_name(): get_main_environment() returns LocalEnvironment,\
		                 but by definition should only return MainEnvironment"
            ),
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
        match self.get_main_environment() {
            Environment::MainEnvironment(env_val) => {
                let namespace_sym = self.get_current_namespace_symbol();
                env_val.add_referred_namespace(&namespace_sym, referred_namespace_sym)
            }
            Environment::LocalEnvironment(..) => panic!(
                "get_main_environment() returns LocalEnvironment,\
		             but by definition should only return MainEnvironment"
            ),
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

    /// Insert into the environment around you;  the local bindings,
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

    /// Get closest value "around" us;  try our local environment, then
    /// try our main environment (unless its namespace qualified)
    pub fn get(&self, sym: &Symbol) -> Rc<Value> {
        match self {
            Environment::MainEnvironment(env) => {
                env.get_from_namespace(&env.get_current_namespace_symbol(), &sym)
            }
            Environment::LocalEnvironment(parent_env, mappings) => {
                if sym.ns != None {
                    return self.get_main_environment().get(sym);
                }
                match mappings.borrow().get(sym) {
                    Some(val) => Rc::clone(val),
                    None => parent_env.get(sym),
                }
            }
        }
    }

    /// Returns the value of the Var associated with the Symbol in the current namespace
    fn get_symbol_value(&self, sym: &Symbol) -> Option<Rc<Value>> {
        match self.get(sym).as_ref() {
            Value::Var(v) => Some(v.val.borrow().clone()),
            _ => None,
        }
    }

    pub fn bind(&self, mbinds: Rc<Value>, exprs: Vec<Value>) -> Result<Environment, LispError> {
        let new_env = Environment::new_local_environment(Rc::new((*self).clone()));
        match (*mbinds).clone() {
            Value::List(binds, _) | Value::Vector(binds, _) => {
                if binds.len() != exprs.len() {
                    return Err(ErrString(
                        "Length mismatching between binds abd exprs".to_string(),
                    ));
                }
                for (i, b) in binds.iter().enumerate() {
                    match b {
                        Value::Symbol(s) if s.name() == "&" => {
                            if let Value::Symbol(sym) = binds[i + 1].clone() {
                                let val = Rc::new(list_from_vec(exprs[i..].to_vec()));
                                let var =
                                    Var::new(self.get_current_namespace_symbol(), sym.clone(), val);
                                new_env.insert(sym, Value::Var(var).to_rc_value());
                            } else {
                                panic!("Symbol expected");
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
                                panic!("Symbol expected");
                            }
                        }
                    }
                }
                Ok(new_env)
            }
            _ => Err(ErrString("env_bind binds not List/Vector".to_string())),
        }
    }

    /// Returns a macro function and the arguments, if found or None
    fn is_macro_call(&self, ast: &Value) -> Option<(Value, ExprArgs)> {
        match ast {
            Value::List(v, _) => match v[0] {
                Value::Symbol(ref s) => {
                    // let val = self.get_symbol_value(s);
                    match self.get_symbol_value(s) {
                        Some(val) => {
                            match (*val).clone() {
                                f @ Value::DefinedFunc { is_macro: true, .. } => {
                                    // Evaluate the arguments before apply the macro fn
                                    let args =
                                        v[1..].iter().map(|v| self.eval(v).unwrap()).collect();
                                    Some((f, args))
                                }
                                _ => None,
                            }
                        }
                        None => None,
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn macroexpand(&self, mut ast: Value) -> (bool, Result<Rc<Value>, LispError>) {
        let mut was_expanded = false;
        while let Some((mf, args)) = self.is_macro_call(&ast) {
            ast = match mf.apply(args, self) {
                Ok(a) => a,
                Err(e) => return (false, Err(e)),
            };
            was_expanded = true;
        }
        (was_expanded, Ok(Rc::new(ast)))
    }

    fn eval_list(&self, lst: &Rc<Vec<Value>>) -> Result<Rc<Value>, LispError> {
        let head = &lst[0];
        let args = &lst[1..].to_vec();
        match head {
            Value::Symbol(ref headsym) if headsym.name() == "def" => {
                if args.len() < 1 {
                    return error!("Wrong number of arguments given to def. Expecting at least 1");
                }
                match args[0].clone() {
                    Value::Symbol(sym) => {
                        let value = if args.len() > 1 {
                            self.eval_to_rc(&args[1].clone())?
                        } else {
                            Value::Nil.to_rc_value()
                        };

                        let new_sym = Symbol::new_with_ns(
                            &sym.name,
                            Some(&self.get_current_namespace_name()),
                        );
                        let new_var = Var::new(
                            self.get_current_namespace_symbol(),
                            new_sym.unqualified(),
                            value.clone(),
                        );
                        let sym_meta = sym.meta();
                        if !sym_meta.is_empty() {
                            new_var.with_meta(Rc::new(sym_meta));
                        }
                        let value_var = Value::Var(new_var).to_rc_value();
                        self.insert(new_sym.unqualified(), value_var.clone());
                        Ok(value_var)
                    }
                    _ => Err(ErrString(
                        "First argument to def must be a symbol".to_string(),
                    )),
                }
            }
            Value::Symbol(ref headsym) if headsym.name() == "let" => {
                if args.len() < 1 || args.len() > 2 {
                    return error!("Wrong number of arguments given to let. Expecting 1 or 2");
                }
                let local_bindings = args[0].clone();
                match local_bindings {
                    Value::Vector(ref binds, _) => {
                        if binds.len() % 2 == 1 {
                            return error!("Binding vector requires an even number of forms");
                        }

                        // Clone the environment, creating a local one
                        let local_env = self.new_local();
                        for (b, e) in binds.iter().tuples() {
                            match b {
                                Value::Symbol(sym) => {
                                    // Insert the let bindings into the local env
                                    let val = local_env.eval_to_rc(e)?;
                                    let var = Var::new(
                                        self.get_current_namespace_symbol(),
                                        (*sym).clone(),
                                        val,
                                    );
                                    local_env.insert((*sym).clone(), Value::Var(var).to_rc_value());
                                }
                                _ => {
                                    return error!(format!(
                                        "Invalid use of binding with {}, symbol expected",
                                        b
                                    ))
                                }
                            }
                        }
                        if let Some(body) = args.get(1) {
                            // Eval the body into the local env previously created
                            Ok(local_env.eval_to_rc(body)?)
                        } else {
                            // Nothing happened, nil
                            Ok(Rc::new(Value::Nil))
                        }
                    }
                    _ => error!("let bindings expected to be a vector"),
                }
            }
            Value::Symbol(ref headsym) if headsym.name() == "macroexpand" => {
                match self.macroexpand(args[0].clone()) {
                    // Returns the macro expansion
                    (_, Ok(new_ast)) => Ok(new_ast),
                    (_, e) => return e,
                }
            }
            // Implement Do as a Macro and the eval, just eval the arguments and call the symbol fn
            Value::Symbol(ref headsym) if headsym.name() == "do" => {
                if args.len() > 0 {
                    // Eval all the previous forms
                    for a in args[..lst.len() - 1].iter() {
                        self.eval_to_rc(a)?;
                    }

                    // Get the last form (or nil) and evalue it
                    let new_ast = args.last().unwrap_or(&Value::Nil).clone();
                    Ok(self.eval_to_rc(&new_ast)?)
                } else {
                    // If empty, nil
                    Ok(Value::Nil.to_rc_value())
                }
            }
            Value::Symbol(ref headsym) if headsym.name() == "var" => {
                if args.len() != 1 {
                    return error!("Wrong number of arguments given to var. Expecting 1");
                }
                match args[0].clone() {
                    Value::Symbol(sym) => {
                        let val = self.get(&sym);
                        let x = match val.as_ref() {
                            Value::Var(_) => Ok(val.clone()),
                            _ => error!("Unable to resolve var"),
                        };
                        x
                    }
                    _ => error!("Symbol required for var"),
                }
            }
            Value::Symbol(ref headsym) if headsym.name() == "if" => {
                if args.len() < 2 || args.len() > 3 {
                    return error!("Wrong number of arguments given to if. Expecting 2 or 3");
                }
                let cond = self.eval(&args[0].clone())?;
                match cond {
                    // If FALSE, eval the second form (ELSE)
                    Value::Bool(false) | Value::Nil if args.len() >= 3 => {
                        Ok(self.eval_to_rc(&args[2].clone())?)
                    }
                    Value::Bool(false) | Value::Nil => Ok(Rc::new(Value::Nil)),
                    // If TRUE, eval the first form
                    _ if args.len() >= 2 => Ok(self.eval_to_rc(&args[1].clone())?),
                    // Otherwise nil
                    _ => Ok(Rc::new(Value::Nil)),
                }
            }
            Value::Symbol(ref headsym) if headsym.name() == "fn" => {
                if args.len() < 1 {
                    return error!("Wrong number of arguments given to fn. Expecting at least 1");
                }

                let params = match args[0].clone() {
                    Value::Vector(_, _) => args[0].clone(),
                    _ => return error!("fn arguments have to be a vector"),
                };
                let body = match args.get(1) {
                    Some(v) => v,
                    None => &Value::Nil,
                };
                Ok(Rc::new(Value::DefinedFunc {
                    ast: Rc::new((*body).clone()),
                    env: Rc::new((*self).clone()),
                    params: Rc::new(params),
                    is_macro: false,
                    meta: None,
                }))
            }
            Value::Symbol(ref headsym) if headsym.name() == "eval" => {
                if args.len() != 1 {
                    return error!("Wrong number of arguments given to eval. Expecting 1");
                }
                // NB eval expects a form, not a string.
                // So first evaluate the argument, and then evaluate the form in the top environment
                let ast = self.eval(&args[0].clone())?;
                let new_env = self.get_main_environment();
                Ok(new_env.eval_to_rc(&ast)?)
            }
            Value::Symbol(ref headsym) if headsym.name() == "quote" => {
                if args.len() != 1 {
                    return error!("Wrong number of arguments given to quote. Expecting 1");
                }
                Ok(args[0].to_rc_value())
            }
            Value::Symbol(ref headsym) if headsym.name() == "quasiquoteexpand" => {
                if args.len() != 1 {
                    return error!(
                        "Wrong number of arguments given to quasiquoteexpand. Expecting 1"
                    );
                }
                Ok(Rc::new(quasiquote(&args[0])))
            }
            Value::Symbol(ref headsym) if headsym.name() == "quasiquote" => {
                if args.len() != 1 {
                    return error!("Wrong number of arguments given to quasiquote. Expecting 1");
                }
                Ok(self.eval_to_rc(&quasiquote(&args[0]))?)
            }
            Value::Symbol(ref headsym) if headsym.name() == "defmacro" => {
                if args.len() < 2 {
                    return error!(
                        "Wrong number of arguments given to defmacro. Expecting at least 2"
                    );
                }
                let macro_name = args[0].clone();
                let macro_args = args[1].clone();
                let r = self.eval(&macro_args)?;
                match r {
                    Value::DefinedFunc {
                        ast, env, params, ..
                    } => {
                        let val = Value::DefinedFunc {
                            ast: ast.clone(),
                            env: env.clone(),
                            params: params.clone(),
                            is_macro: true,
                            meta: None,
                        }
                        .to_rc_value();
                        if let Value::Symbol(sym) = macro_name {
                            let var = Var::new(
                                self.get_current_namespace_symbol(),
                                sym.clone(),
                                val.clone(),
                            );
                            self.insert(sym, Value::Var(var).to_rc_value());
                        }
                        Ok(val)
                    }
                    _ => error!("Defmacro on non-function"),
                }
            }
            Value::Symbol(ref headsym) if headsym.name() == "try" => {
                if args.len() < 2 {
                    return error!("Wrong number of arguments given to try. Expecting at least 2");
                }
                match self.eval(&args[0].clone()) {
                    Err(ref e) if args.len() >= 2 => {
                        let ex = match e {
                            ErrValue(v) => v.clone(),
                            ErrString(s) => Value::Str(s.to_string()),
                        };
                        match args[1].clone() {
                            Value::List(c, _) => {
                                let catch_env =
                                    self.bind(list![c[1].clone()].to_rc_value(), vec![ex])?;
                                Ok(catch_env.eval_to_rc(&c[2].clone())?)
                            }
                            _ => error!("Invalid catch block"),
                        }
                    }
                    res => Ok(res.unwrap().to_rc_value()),
                }
            }
            _ => {
                // At this point we just execute the function.
                // First we evaluate the function
                let ref f = self.eval_to_rc(&head.clone())?;
                match &*f.borrow() {
                    Value::Func(_, _) => {
                        let evaled_args = args
                            .iter()
                            .map(|rc_arg| self.eval(rc_arg).unwrap())
                            .collect::<Vec<Value>>();
                        let ret = f.apply(evaled_args, self)?;
                        Ok(ret.to_rc_value())
                    }
                    // An internal macro, is a function which the parameter are not evaluated,
                    // but passed to the function as is (so keeping symbols etc).
                    Value::Macro(_, _) => {
                        let ret = f.apply(args.clone(), self)?;
                        Ok(ret.to_rc_value())
                    }
                    Value::DefinedFunc {
                        ast: mast,
                        env: menv,
                        params,
                        ..
                    } => {
                        let a = &**mast;
                        let evaled_args = args
                            .iter()
                            .map(|rc_arg| self.eval(rc_arg).unwrap())
                            .collect::<Vec<Value>>();
                        let new_env = menv.bind(params.clone(), evaled_args)?;
                        Ok(new_env.eval_to_rc(&a)?)
                    }
                    _ => error!(format!("Attempt to call non-function {}", f)),
                }
            }
        }
    }

    // Probably the most important function in the entire Lisp concept
    // Need to returns a ValueError in case of eval error, to get catch by the REPL.
    /// Evaluate what's inside `value` and return a Rc to the result value
    pub fn eval_to_rc(&self, ast: &Value) -> Result<Rc<Value>, LispError> {
        match ast {
            // Symbol returns value in env 'sym ;=> 12
            Value::Symbol(sym) => match self.get_symbol_value(sym) {
                Some(val) => Ok(val),
                None => error!(format!("'{}' symbol not found in this context", sym)),
            },
            // Vector eval each item [1 2 (+ 2 3)] ;=> [1 2 5]
            Value::Vector(v, _) => {
                let evaled: Vec<Value> = v.iter().map(|e| self.eval(e).unwrap()).collect();
                Ok(Value::Vector(Rc::new(evaled), None).to_rc_value())
            }
            // HashMap similar to vector, eval each key/value
            Value::HashMap(hm, meta) => {
                let mut new_hm = HashMap::new();
                for (k, v) in hm.iter() {
                    new_hm.insert(self.eval(k).unwrap(), self.eval(v).unwrap());
                }
                Ok(Value::HashMap(Rc::new(new_hm), meta.clone()).to_rc_value())
            }
            // List is an sexpr, with function as first parameter (+ 1 2)
            Value::List(l, _) => {
                if l.len() == 0 {
                    return Ok(ast.to_rc_value());
                }

                // catch a macro symbol before eval the list
                match self.macroexpand(ast.clone()) {
                    // Eval the extended ast
                    (true, Ok(new_ast)) => return self.eval_to_rc(&*new_ast),
                    (_, Err(e)) => return Err(e),
                    _ => (), // if not a macro, don't do nothing
                }

                if l.len() == 0 {
                    return Ok(ast.to_rc_value());
                }

                Ok(self.eval_list(l)?)
            }
            // All the rest, returns itself 1=>1, "str"=>"str"
            _ => Ok(ast.to_rc_value()),
        }
    }

    pub fn eval(&self, value: &Value) -> ValueRes {
        Ok(self.eval_to_rc(value)?.to_value())
    }
}
