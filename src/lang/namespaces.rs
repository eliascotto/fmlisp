use crate::env::Environment;
use crate::keyword::Keyword;
use crate::symbol::Symbol;
use crate::utils::IsOdd;
use crate::values::{
    self, _assoc, _dissoc, error, func, hash_map_from_vec, list_from_vec, macro_fn, set_from_vec,
    vector_from_vec, ExprArgs, LispError, ToValue, Value, ValueRes,
};
use if_chain::if_chain;
use itertools::Itertools;
use std::cell::RefCell;
use std::rc::Rc;

fn ns(args: ExprArgs, env: &Environment) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to ns. Expecting 1");
    }
    match args[0] {
        Value::Symbol(ref sym) => {
            env.change_or_create_namespace(&sym);
            Ok(Value::Nil)
        }
        _ => error!("ns requires symbol"),
    }
}

fn find_ns(args: ExprArgs, env: &Environment) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to find-ns. Expecting 1");
    }

    match args[0] {
        Value::Symbol(ref sym) => match env.find_namespace(sym) {
            Some(ns) => Ok(ns.to_value()),
            None => Ok(Value::Nil),
        },
        _ => error!("find-ns requires symbol"),
    }
}

fn all_ns(args: ExprArgs, env: &Environment) -> ValueRes {
    if !args.is_empty() {
        return error("Wrong number of arguments passed to all-ns. Expecting none");
    }

    let v = env.get_all_namespaces();
    let lst: Vec<Value> = v.iter().map(|a| a.to_value()).collect();
    Ok(list_from_vec(lst))
}

/// If passed a namespace, returns it. Else, when passed a symbol,
/// returns the namespace named by it, throwing an exception if not
/// found.
fn the_ns(args: ExprArgs, env: &Environment) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to the-ns. Expecting 1");
    }

    match args[0] {
        Value::Namespace(_) => Ok(args[0].clone()),
        Value::Symbol(ref sym) => match env.find_namespace(sym) {
            Some(ns) => Ok(ns.to_value()),
            None => error!(format!("No namespace found with symbol {}", sym)),
        },
        _ => error!("the-ns requires symbol or namespace"),
    }
}

/// Returns the ns from a Var or a Symbol
fn get_ns(args: ExprArgs, env: &Environment) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to the-ns. Expecting 1");
    }

    match args[0] {
        Value::Var(ref var) => Ok(Value::Str(var.ns.clone().name)),
        Value::Symbol(ref sym) => match sym.ns.clone() {
            Some(x) => match env.find_namespace(&sym!(&x)) {
                Some(ns) => Ok(Value::Namespace(RefCell::new(ns))),
                None => Ok(Value::Nil),
            },
            None => Ok(Value::Nil),
        },
        _ => error!("get-ns requires symbol or namespace"),
    }
}

fn ns_map(args: ExprArgs, env: &Environment) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to ns-mappings. Expecting 1");
    }

    match args[0] {
        Value::Namespace(ref ns) => {
            let mappings = ns.borrow().get_mappings_as_values();
            Ok(Value::HashMap(Rc::new(mappings), None))
        }
        Value::Symbol(ref sym) => match env.find_namespace(sym) {
            Some(ns) => {
                let mappings = ns.get_mappings_as_values();
                Ok(Value::HashMap(Rc::new(mappings), None))
            }
            None => error!("namespace not found"),
        },
        _ => error!("ns-map requires namespace"),
    }
}

fn ns_unmap(args: ExprArgs, _env: &Environment) -> ValueRes {
    if args.len() != 2 {
        return error("Wrong number of arguments passed to ns-mappings. Expecting 2");
    }

    match args[0] {
        Value::Namespace(ref ns) => match args[1] {
            Value::Symbol(ref sym) => {
                ns.borrow().remove(sym);
                Ok(Value::Nil)
            }
            _ => error!("ns-unmap requires symbol as second parameter"),
        },
        _ => error!("ns-unmap requires namespace as first parameter"),
    }
}

// Refer currently supports only the filter `:only` and not `:exclude` or `:rename`.
// The referred symbols need to be checked that they don't contains ^:private in their
// metadata.
// Currently we give precedence to the namespace for the symbol lookup, avoiding an override
// if the symbol is already present. Referrers are kept in a separate table.
// Clojure instead, override the symbol if already present.
fn refer(args: ExprArgs, env: &Environment) -> ValueRes {
    if !args.len().is_odd() {
        return error("Wrong number of arguments passed to ns. Expecting at least 1");
    }

    let namespace = args.get(0).unwrap();
    if args.len() > 1 {
        for (filter_key, filter_val) in args.iter().skip(1).tuples() {
            // To avoid ungodly nesting
            // Ex: 'ns :only [a b c d]
            if_chain! {
                if let Value::Symbol(ns) = namespace;
                if let Value::Keyword(Keyword{sym: Symbol{name: filter_name,..}}) = filter_key;
                if filter_name == "only";
                if let Value::Vector(pvector, _) = filter_val;
                then {
                    // We're going to get our vector of symbols as a pvector of Rc<Value>, we need to convert that
                    // into a vector of symbols
                    let mut sym_vector: Vec<Symbol> = vec![];
                    let rc_pvector = Rc::new(pvector.clone());
                    for maybe_rc_sym in rc_pvector.iter() {
                        if let Value::Symbol(sym) = &*maybe_rc_sym {
                            sym_vector.push(sym.unqualified());
                        }
                        else {
                            return error(&format!("Expected symbol, found {}", maybe_rc_sym));
                        }
                    }
                    env.add_referred_syms(ns, sym_vector)?;
                }
            }
        }
    } else {
        if let Value::Symbol(ns) = namespace {
            env.add_referred_namespace(ns)?;
        }
    }
    Ok(Value::Nil)
}

pub fn namespace_functions() -> Vec<(&'static str, Value)> {
    vec![
        ("ns", macro_fn(ns)),
        ("find-ns", func(find_ns)),
        ("all-ns", func(all_ns)),
        ("the-ns", func(the_ns)),
        ("ns-map", func(ns_map)),
        ("ns-unmap", func(ns_unmap)),
        ("get-ns", func(get_ns)),
        ("refer", func(refer)),
    ]
}
