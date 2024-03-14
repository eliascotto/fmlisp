use itertools::Itertools;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::env::Environment;
use crate::lang;
use crate::reader;
use crate::symbol::Symbol;
use crate::values::keyword;
use crate::values::list_from_vec;
use crate::values::symbol;
use crate::values::LispErr::{self};
use crate::values::{format_error, ExprArgs, ToValue, Value, ValueRes};
use crate::var::Var;

/// Read and eval a file
fn load_file(path: &str, env: Rc<Environment>) {
    match lang::core::load_file(&String::from(path), env.clone()) {
        Ok(_) => {}
        Err(e) => match e.clone() {
            LispErr::Error(err) => {
                let prefix = if let Some(details) = err.details() {
                    format!("Error {} at {}", details, path)
                } else {
                    format!("Error at {}", path)
                };
                if err.has_trace() {
                    eprintln!(
                        "{}\n{}\nBacktrace:\n{}",
                        prefix,
                        format_error(e),
                        err.fmt_trace(false)
                    )
                } else {
                    eprintln!("{}\n{}", prefix, format_error(e))
                }
            }
            _ => eprintln!("Error at {}\n{}", path, format_error(e)),
        },
    }
}

/// Load the core functions and symbols into the namespace
pub fn load_core(env: Rc<Environment>) {
    env.change_or_create_namespace(&sym!("fmlisp.lang"));

    // Loading internal definitions
    for (k, v) in lang::core::internal_symbols(&env) {
        env.insert_var(Symbol::new(k), v.to_rc_value());
    }

    let fns = vec![
        lang::core::core_functions(),
        lang::strings::string_functions(),
        lang::namespaces::namespace_functions(),
    ];

    for fs in fns {
        for (k, v) in fs {
            // Namespace mapping Symbol - Rc<Value>
            // Load them into clojure.core ns
            env.insert_var(Symbol::new(k), v.to_rc_value());
        }
    }

    // Load language file
    load_file("src/fmlisp/core.fml", env.clone());

    // Change to default user namespace
    env.change_or_create_namespace(&Symbol::new("user"));

    let _ = env.add_referred_namespace(&sym!("fmlisp.core"));
}

/// Transform string into a FMLisp Value.
pub fn read(s: &str) -> ValueRes {
    reader::read_str(s.to_string())
}

/// Transform the Value to string.
pub fn print(exp: Value) -> String {
    exp.pr_str()
}

/// Add AST to error trace.
pub fn process_error(e: &LispErr, ast: &Value) -> LispErr {
    match e.clone() {
        // Load trace in case of apply raised an error
        LispErr::Error(mut err) => {
            err.add_trace(ast.clone());
            LispErr::Error(err)
        }
        _ => e.clone(),
    }
}

fn qq_iter(elts: &ExprArgs, env: Rc<Environment>) -> Value {
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
        acc = list![
            Value::Symbol(sym!("cons")),
            quasiquote(&elt, env.clone()),
            acc
        ];
    }
    acc
}

fn quasiquote(ast: &Value, env: Rc<Environment>) -> Value {
    match ast.clone() {
        Value::List(v, _) => {
            if v.len() == 2 {
                if let Value::Symbol(ref s) = v[0] {
                    if s.name() == "unquote" {
                        return v[1].clone();
                    }
                }
            }
            qq_iter(&v, env)
        }
        Value::Vector(v, _) => list![Value::Symbol(sym!("vec")), qq_iter(&v, env)],
        Value::Symbol(mut sym) => {
            sym.ns = Some(env.get_current_namespace_name());
            list![Value::Symbol(sym!("quote")), Value::Symbol(sym)]
        }
        Value::HashMap(_, _) | Value::Set(_, _) => {
            list![Value::Symbol(sym!("quote")), ast.clone()]
        }
        _ => ast.clone(),
    }
}

// How to solve the macro definition inside a different namespace?
//
// Problem:
//  Macro expansion should resolve all functions Var in the namespace where the
// macro has been declared. So if the Macro call a function fn_b from namespace B,
// fn_b should be resolved to B/fn_b to make sure no confusion is operated. This is
// the default behaviour for resolving namespace that Clojure does. How to do that
// in our code?
//
// Solution 1:
//  During the macroexpansion, change the current namespace to the one of the macro,
// process the macro body and resolve all the symbols in the namespace of the macro,
// then change it back to the old namespace. So the macro body will now have all the
// function symbols with namespace included.
//
// Problem 2:
//  What I get is the body which has different symbols, like arguments (list, let).
// Not all symbols are the same, so some of them are not present in the context. How
// can I differentiate them to resolve the macro?
//
// Solution 2:
//  The macro expansion, should happen only into the namespace where the macro is
// defined.

/// Returns a macro function, the arguments and the macro env, and the namespace of the var
/// that contains the symbol of the macro, if found or None.
fn is_macro_call(
    ast: Value,
    env: Rc<Environment>,
) -> Option<(Value, ExprArgs, Rc<Environment>, Symbol)> {
    match ast.clone() {
        Value::List(v, _) => match v[0] {
            Value::Symbol(ref s) => {
                //
                match env.get(s).as_ref() {
                    Value::Var(var) => match &*var.val.borrow().clone() {
                        f @ Value::Lambda {
                            is_macro: true,
                            env: e,
                            ..
                        } => Some((f.clone(), v[1..].to_vec(), e.clone(), var.ns.clone())),
                        _ => None,
                    },
                    _ => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

/// Expand the Macro call, if found in the code. It returns a tuple
/// with `true` if a macro has been expanded, and the expanded AST, false otherwise.
fn macroexpand(mut ast: Value, env: Rc<Environment>) -> (bool, Result<Rc<Value>, LispErr>) {
    let mut was_expanded = false;
    while let Some((mf, args, macro_env, macro_ns)) = is_macro_call(ast.clone(), env.clone()) {
        // During the macro expansion, the macro function has access to
        // all the public and private functions, macros, and vars defined in its own namespace.
        // So we're temporary change the current namespace for evaling the macro, then
        // changing it back to the previous one.
        let prev_ns = macro_env.get_current_namespace_symbol();
        macro_env.change_or_create_namespace(&macro_ns);

        ast = match mf.apply(args, macro_env.clone()) {
            Ok(a) => a,
            Err(e) => {
                macro_env.change_or_create_namespace(&prev_ns); // set ns back
                match e.clone() {
                    // Load trace in case of apply raised an error
                    LispErr::Error(mut err) => {
                        err.set_type("Syntax");
                        err.set_details("macroexpanding");
                        err.add_trace(ast.clone());
                        return (false, Err(LispErr::Error(err)));
                    }
                    _ => return (false, Err(e)),
                }
            }
        };

        macro_env.change_or_create_namespace(&prev_ns); // set ns back
        was_expanded = true;
    }
    (was_expanded, Ok(Rc::new(ast)))
}

fn eval_ast(ast: Value, env: Rc<Environment>) -> Result<Rc<Value>, LispErr> {
    match ast.clone() {
        // Symbol returns value in env 'sym ;=> 12
        Value::Symbol(sym) => match env.get_symbol_value(&sym) {
            Some(val) => Ok(val),
            None => {
                error_fmt!(
                    "'{}' symbol not found in this context: '{}",
                    sym.to_string(),
                    env.get_current_namespace_name()
                )
            }
        },
        // Vector eval each item [1 2 (+ 2 3)] ;=> [1 2 5]
        Value::Vector(v, _) => {
            let mut evaled = vec![];
            for c in v.iter() {
                match eval(c.clone(), env.clone()) {
                    Ok(l) => evaled.push(l),
                    Err(e) => return Err(process_error(&e, &ast)),
                }
            }
            Ok(Value::Vector(Rc::new(evaled), None).to_rc_value())
        }
        // HashMap similar to vector, eval each key/value
        Value::HashMap(hm, meta) => {
            let mut new_hm = HashMap::new();
            for (k, v) in hm.iter() {
                let keys = match eval(k.clone(), env.clone()) {
                    Ok(v) => v,
                    Err(e) => return Err(process_error(&e, &ast)),
                };
                let values = match eval(v.clone(), env.clone()) {
                    Ok(v) => v,
                    Err(e) => return Err(process_error(&e, &ast)),
                };
                new_hm.insert(keys, values);
            }
            Ok(Value::HashMap(Rc::new(new_hm), meta.clone()).to_rc_value())
        }
        Value::List(l, _) => {
            let mut evaled = vec![];
            for c in l.iter() {
                match eval(c.clone(), env.clone()) {
                    Ok(v) => evaled.push(v),
                    Err(e) => return Err(process_error(&e, &ast)),
                }
            }
            Ok(Value::List(Rc::new(evaled), None).to_rc_value())
        }
        _ => Ok(ast.to_rc_value()),
    }
}

fn get_meta_key(meta: Option<HashMap<Value, Value>>, key: Value) -> Option<Value> {
    match meta {
        Some(m) => m.get(&key).cloned(),
        None => None,
    }
}

/// Return a Value that is the body of the Lambda, or Nil.
/// Supports the extraction of multiple forms in the body.
fn get_body_definitions(args: Vec<Value>) -> Rc<Value> {
    if args.len() > 1 {
        // Let's wrap multiple forms body into a `do` func.
        // It's only in the interpreter for the moment.
        let mut defs = vec![symbol("do")];
        defs.extend(args.into_iter().map(|v| v.clone()));
        Rc::new(list_from_vec(defs))
    } else if args.len() == 1 {
        Rc::new(args[0].clone())
    } else {
        Rc::new(Value::Nil)
    }
}

fn calc_arity(params: Rc<Vec<Value>>) -> usize {
    let mut len: usize = 0;
    for p in params.iter() {
        match p {
            Value::Symbol(sym) => {
                if sym.name() == "&" {
                    // If & let's just use the max value available
                    return usize::MAX;
                }
                len += 1;
            }
            _ => panic!(
                "Expected a vector of symbols for arguments, found {:?}",
                params
            ),
        }
    }
    len
}

// Returns a tuple ast-params based on the function's arity needed
pub fn find_ast_and_params_by_arity(
    ast: &[Rc<Value>],
    params: &[Rc<Value>],
    arity: usize,
) -> Option<(Rc<Value>, Rc<Value>)> {
    // Get the index of the correct arity for params.
    // Then use the corresponding params and ast for executing the Lambda.
    for (index, param) in params.iter().enumerate() {
        if let Value::Vector(v, _) = &**param {
            if calc_arity(v.clone()) >= arity {
                return Some((ast[index].clone(), params[index].clone()));
            }
        }
    }
    None
}

// Returns a new local environment with all the binds sym-sexpr evaluated and
// binded into it. Requires binds to be a Vector with even number of arguments.
fn bind_local_env(
    binds: &Rc<Vec<Value>>,
    ast: &Value,
    parent_env: Rc<Environment>,
) -> Result<Rc<Environment>, LispErr> {
    let local_env = Rc::new(parent_env.new_local());
    for (b, e) in binds.iter().tuples() {
        match b {
            Value::Symbol(sym) => {
                let val = match eval_to_rc(e.clone(), local_env.clone()) {
                    Ok(v) => v,
                    Err(e) => return Err(process_error(&e, ast)),
                };
                let var = Var::new(
                    local_env.get_current_namespace_symbol(),
                    (*sym).clone(),
                    val,
                );
                // Insert the let bindings into the local env, as Vars
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
    Ok(local_env)
}

pub fn eval_to_rc(mut ast: Value, env: Rc<Environment>) -> Result<Rc<Value>, LispErr> {
    let ret: Result<Rc<Value>, LispErr>;

    // I'm using a RefCell here to swapping the env inside the loop.
    // I avoided using a reference to an Environment object to keep
    // the code safe from manipulation. Instead of a reference, we use a
    // counted-reference that is safe and inexpensive to clone. With the RefCell
    // we make this reference mutable.
    // From now on, the following code should use `mut_env` and not `env`.
    let mut_env = RefCell::new(env.clone());

    // He're the loop gave us the Tail Call Optimization, avoiding
    // to fill the stack with useless tail call to `eval` and instead
    // looping trough the AST.
    //
    // TCO is used by if, let, apply, quasiquote do, try/catch, case, etc.
    // Wherever you see `continue 'tco;` means that instead of calling eval
    // recursively, we eval the rest in a loop.
    'tco: loop {
        ret = match ast.clone() {
            Value::List(l, _) => {
                if l.len() == 0 {
                    return Ok(ast.to_rc_value());
                }

                // Catch a macro symbol before evaluate the list
                match macroexpand(ast.clone(), mut_env.borrow().clone()) {
                    // Eval the extended ast
                    (true, Ok(new_ast)) => {
                        return match eval_to_rc((*new_ast).clone(), mut_env.borrow().clone()) {
                            Ok(v) => Ok(v),
                            Err(e) => {
                                return Err(process_error(&e, &ast));
                            }
                        }
                    }
                    (_, Err(e)) => return Err(e),
                    _ => (), // if not a macro, don't do nothing
                }

                if l.len() == 0 {
                    return Ok(ast.to_rc_value());
                }

                let head = &l[0];
                let args = &l[1..].to_vec();
                match head {
                    Value::Symbol(ref headsym) if headsym.name() == "def" => {
                        if args.len() < 1 {
                            return error!(
                                "Wrong number of arguments given to def. Expecting at least 1"
                            );
                        }
                        match args[0].clone() {
                            Value::Symbol(sym) => {
                                let value = if args.len() > 1 {
                                    match eval_to_rc(args[1].clone(), mut_env.borrow().clone()) {
                                        Ok(v) => v,
                                        Err(e) => return Err(process_error(&e, &ast)),
                                    }
                                } else {
                                    Value::Nil.to_rc_value()
                                };
                                let sym_meta = sym.meta();

                                // Copy Meta from symbol into the Lambda definition
                                let value = match &*value {
                                    Value::Lambda {
                                        ast, env, params, ..
                                    } => {
                                        // Redefine a function to catch is it's a macro or not
                                        let is_macro =
                                            get_meta_key(Some(sym_meta.clone()), keyword("macro"))
                                                == Some(Value::Bool(true));

                                        let new_val = Value::Lambda {
                                            ast: ast.clone(),
                                            env: env.clone(),
                                            params: params.clone(),
                                            is_macro, // Set is_macro is macro in metadata
                                            meta: Some(sym_meta.clone()),
                                        };
                                        Rc::new(new_val)
                                    }
                                    _ => value.clone(),
                                };

                                // Creates a Symbol
                                let new_sym = Symbol::new_with_ns(
                                    &sym.name,
                                    Some(&mut_env.borrow().get_current_namespace_name()),
                                );
                                // Creates a new Var
                                let new_var = Var::new(
                                    mut_env.borrow().get_current_namespace_symbol(),
                                    new_sym.unqualified(),
                                    value.clone(),
                                );
                                if !sym_meta.is_empty() {
                                    new_var.with_meta(Rc::new(sym_meta));
                                }
                                let value_var = Value::Var(new_var).to_rc_value();
                                // Add symbol-var to env mapping
                                mut_env
                                    .borrow_mut()
                                    .insert(new_sym.unqualified(), value_var.clone());
                                Ok(value_var)
                            }
                            _ => Err(LispErr::ErrString(
                                "First argument to def must be a symbol".to_string(),
                            )),
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "let*" => {
                        if args.len() < 1 || args.len() > 2 {
                            return error!(
                                "Wrong number of arguments given to let. Expecting 1 or 2"
                            );
                        }
                        let local_bindings = args[0].clone();
                        match local_bindings {
                            Value::Vector(ref binds, _) => {
                                if binds.len() % 2 == 1 {
                                    return error!(
                                        "let binding vector requires an even number of forms"
                                    );
                                }

                                // Clone the environment, creating a local one
                                let local_env =
                                    match bind_local_env(binds, &ast, mut_env.borrow().clone()) {
                                        Ok(env) => env,
                                        Err(e) => return Err(e),
                                    };
                                if let Some(body) = args.get(1) {
                                    // Eval the body into the local env previously created
                                    ast = body.clone();
                                    mut_env.replace(local_env);
                                    continue 'tco;
                                }
                                // Otherwise nothing happened, return nil
                                Ok(Rc::new(Value::Nil))
                            }
                            _ => error!("let bindings expected to be a vector"),
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "macroexpand" => {
                        match macroexpand(args[0].clone(), mut_env.borrow().clone()) {
                            // Returns the macro expansion
                            (_, Ok(new_ast)) => Ok(new_ast),
                            (_, e) => return e,
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "do" => {
                        if args.is_empty() {
                            Ok(Value::Nil.to_rc_value())
                        } else {
                            // Eval all the previous forms
                            for a in args[..l.len() - 1].iter() {
                                match eval_to_rc(a.clone(), mut_env.borrow().clone()) {
                                    Ok(_) => {}
                                    Err(e) => return Err(process_error(&e, &ast)),
                                }
                            }

                            // Get the last form (or nil) and evalue it
                            let new_ast = args.last().unwrap_or(&Value::Nil).clone();
                            ast = new_ast;
                            continue 'tco;
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "loop*" => {
                        if args.len() < 1 {
                            return argument_error!(
                                "Wrong number of arguments given to if. Expecting 2 or 3"
                            );
                        }
                        match args[0] {
                            Value::Vector(ref loop_binds, _) => {
                                if loop_binds.len() % 2 == 1 {
                                    return error_fmt!(
                                        "loop binding vector requires an even number of forms"
                                    );
                                }

                                if args.len() > 1 {
                                    let loop_env = match bind_local_env(
                                        loop_binds,
                                        &ast,
                                        mut_env.borrow().clone(),
                                    ) {
                                        Ok(env) => env,
                                        Err(e) => return Err(e),
                                    };
                                    let loop_body = if args.len() == 2 {
                                        args[1].clone()
                                    } else {
                                        let mut defs = vec![Value::Symbol(sym!("do"))];
                                        defs.extend(args[1..].into_iter().map(|v| v.clone()));
                                        list_from_vec(defs)
                                    };

                                    // Extract the keys from the binding vector
                                    let binds_keys: Vec<Value> = loop_binds
                                        .chunks(2)
                                        .map(|chunk| chunk[0].clone())
                                        .collect();
                                    loop {
                                        match eval_to_rc(loop_body.clone(), loop_env.clone()) {
                                            Ok(res) => match &*res {
                                                Value::Recur(rec) => {
                                                    // We need to bind the results of recur call
                                                    // to initial binding vars
                                                    if binds_keys.len() != rec.len() {
                                                        return error_fmt!(
                                                            "recur received wrong number of params: expected {}",
                                                            binds_keys.len()
                                                        );
                                                    }

                                                    // Bind the results from recur call to the symbols defined
                                                    // into the loop binding vector.
                                                    for (i, k) in binds_keys.iter().enumerate() {
                                                        match k {
                                                            Value::Symbol(sym) => {
                                                                let var = Var::new(
                                                                    loop_env.get_current_namespace_symbol(),
                                                                    (*sym).clone(),
                                                                    Rc::new(rec[i].clone()),
                                                                );
                                                                // Insert the let bindings into the local env, as Vars
                                                                loop_env.insert(
                                                                    (*sym).clone(),
                                                                    Value::Var(var).to_rc_value(),
                                                                );
                                                            }
                                                            _ => {
                                                                return error_fmt!(
                                                                    "Invalid use of binding"
                                                                );
                                                            }
                                                        }
                                                    }
                                                }
                                                _ => return Ok(res),
                                            },
                                            Err(e) => return Err(process_error(&e, &ast)),
                                        }
                                    }
                                }
                            }
                            _ => return error_fmt!("loop requires a vector for its binding"),
                        }

                        Ok(Rc::new(Value::Nil)) // nil if no body
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "recur" => {
                        if !args.is_empty() {
                            let mut res = vec![];
                            for a in args.iter() {
                                let eval_res = match eval(a.clone(), mut_env.borrow().clone()) {
                                    Ok(v) => v,
                                    Err(e) => return Err(process_error(&e, &ast)),
                                };
                                res.push(eval_res);
                            }
                            Ok(Rc::new(Value::Recur(res)))
                        } else {
                            Ok(Rc::new(Value::Nil))
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "if" => {
                        if args.len() < 2 || args.len() > 3 {
                            return error!(
                                "Wrong number of arguments given to if. Expecting 2 or 3"
                            );
                        }
                        let cond = match eval(args[0].clone(), mut_env.borrow().clone()) {
                            Ok(v) => v,
                            Err(e) => return Err(process_error(&e, &ast)),
                        };
                        match cond {
                            // If FALSE, eval the second form (ELSE)
                            Value::Bool(false) | Value::Nil if args.len() >= 3 => {
                                ast = args[2].clone();
                                continue 'tco;
                            }
                            Value::Bool(false) | Value::Nil => Ok(Rc::new(Value::Nil)),
                            // If TRUE, eval the first form
                            _ if args.len() >= 2 => {
                                ast = args[1].clone();
                                continue 'tco;
                            }
                            // Otherwise nil
                            _ => Ok(Rc::new(Value::Nil)),
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "fn*" => {
                        if args.len() < 1 {
                            return error!(
                                "Wrong number of arguments given to fn. Expecting at least 1"
                            );
                        }
                        let mut args_idx = 0;

                        // Optional name for lambda
                        let lambda_name = match args[0].clone() {
                            Value::Symbol(_) => {
                                args_idx += 1;
                                Some(args[0].clone())
                            }
                            _ => None,
                        };
                        let mut ast_v = vec![]; // Vec of bodies
                        let mut params_v = vec![]; // Vec of params

                        // Function overloading
                        match args[args_idx].clone() {
                            // If vector, so a function with params and body
                            Value::Vector(_, _) => {
                                let params = args[args_idx].clone();
                                args_idx += 1;
                                // Then function body
                                let lambda_body = get_body_definitions(args[args_idx..].to_vec());
                                ast_v.push(lambda_body);
                                params_v.push(Rc::new(params));
                            }
                            // fn supports multiparmeter definitions using lists
                            Value::List(_, _) => {
                                // Looping trough each list
                                for index in args_idx..(args.len() - args_idx) {
                                    match args[index].clone() {
                                        Value::List(l, _) => {
                                            if l.is_empty() {
                                                return error!(
                                                    "Function overloading cannot be empty."
                                                );
                                            }
                                            // Every list should be composed by arguments and body
                                            // (fn* test
                                            //   ([a] (test a nil))
                                            //   ([a val] (operation a nil)))
                                            params_v.push(Rc::new(l[0].clone()));
                                            ast_v.push(get_body_definitions(l[1..].to_vec()));
                                        }
                                        _ => {
                                            return error!(format!(
                                                "fn definition expected a List, found {} instead",
                                                args[index].as_str()
                                            ))
                                        }
                                    }
                                }

                                // Checking overloads arity and make sure that they're all different
                                let mut fn_arity = HashSet::new();
                                for p in params_v.clone() {
                                    match &*p {
                                        Value::Vector(v, _) => {
                                            let length = v.len();
                                            if !fn_arity.insert(length) {
                                                return error!(
                                                    "Can't have 2 overloads with same arity."
                                                );
                                            }
                                        }
                                        _ => return error!("Function arguments must be a vector."),
                                    }
                                }
                            }
                            _ => {
                                return error!(format!(
                                    "fn arguments have to be in the form a vector or a list.\n{}",
                                    ast
                                ))
                            }
                        };

                        Ok(Rc::new(Value::Lambda {
                            ast: ast_v,
                            env: mut_env.borrow().clone(),
                            params: params_v,
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
                        ast = match eval(args[0].clone(), mut_env.borrow().clone()) {
                            Ok(v) => v,
                            Err(e) => return Err(process_error(&e, &ast)),
                        };
                        mut_env.replace(Rc::new(mut_env.borrow().get_main_environment().clone()));
                        continue 'tco;
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
                        Ok(Rc::new(quasiquote(&args[0], mut_env.borrow().clone())))
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "quasiquote" => {
                        if args.len() != 1 {
                            return error!(
                                "Wrong number of arguments given to quasiquote. Expecting 1"
                            );
                        }
                        ast = quasiquote(&args[0], mut_env.borrow().clone());
                        continue 'tco;
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "defmacro" => {
                        if args.len() < 2 {
                            return error!(
                                "Wrong number of arguments given to defmacro. Expecting at least 2"
                            );
                        }
                        let macro_name = args[0].clone();
                        let macro_args = args[1].clone();
                        let r = match eval(macro_args, mut_env.borrow().clone()) {
                            Ok(v) => v,
                            Err(e) => return Err(process_error(&e, &ast)),
                        };
                        match r {
                            Value::Lambda {
                                ast, env, params, ..
                            } => {
                                let val = Value::Lambda {
                                    ast: ast.clone(),
                                    env: env.clone(),
                                    params: params.clone(),
                                    is_macro: true,
                                    meta: None,
                                }
                                .to_rc_value();
                                if let Value::Symbol(sym) = macro_name {
                                    let var = Var::new(
                                        env.get_current_namespace_symbol(),
                                        sym.clone(),
                                        val.clone(),
                                    );
                                    env.insert(sym, Value::Var(var).to_rc_value());
                                }
                                Ok(val)
                            }
                            _ => error!("Defmacro on non-function"),
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "try" => {
                        // Make sure to lazy-evaluate the alternative try form!
                        if args.len() < 2 {
                            return error!(
                                "Wrong number of arguments given to try. Expecting at least 2"
                            );
                        }
                        match eval(args[0].clone(), mut_env.borrow().clone()) {
                            // Capture the evaluation error here
                            Err(ref e) if args.len() >= 2 => {
                                let ex = match e {
                                    LispErr::ErrValue(v) => *v.clone(),
                                    LispErr::ErrString(s) => Value::Str(s.to_string()),
                                    LispErr::Error(e) => Value::Str(e.to_string()),
                                };
                                match args[1].clone() {
                                    Value::List(c, _) => {
                                        let catch_env = mut_env
                                            .borrow()
                                            .bind(list![c[1].clone()].to_rc_value(), vec![ex])?;
                                        Ok(eval_to_rc(c[2].clone(), catch_env)?)
                                    }
                                    _ => error!("Invalid catch block"),
                                }
                            }
                            res => Ok(res.unwrap().to_rc_value()),
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "var" => {
                        if args.len() != 1 {
                            return error!("Wrong number of arguments passed to var. Expecting 1");
                        }
                        match args[0].clone() {
                            Value::Symbol(sym) => {
                                let val = env.get(&sym);
                                match val.as_ref() {
                                    Value::Var(_) => Ok(val.to_rc_value()),
                                    _ => error!("Unable to resolve var"),
                                }
                            }
                            _ => error!("var requires a symbol"),
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "throw" => {
                        if args.len() != 1 {
                            return error!(format!(
                                "Wrong number of arguments ({}) passed to throw. Expecting 1",
                                args.len()
                            ));
                        }
                        match eval(args[0].clone(), mut_env.borrow().clone()) {
                            Ok(res) => match res {
                                Value::Error(err) => Err(err),
                                _ => error!("throw argument has to be an error"),
                            },
                            Err(e) => error!(format_error(e)),
                        }
                    }
                    Value::Keyword(_) => {
                        // Keyword can extract a value from a hashmap
                        if args.len() != 1 {
                            return error!("Wrong number of arguments used to destructuring a map. Expecting 1");
                        }
                        match eval(args[0].clone(), mut_env.borrow().clone()) {
                            Ok(res) => match res {
                                Value::HashMap(hm, _) => {
                                    let val = hm.get(head);
                                    match val {
                                        Some(v) => Ok(v.to_rc_value()),
                                        None => Ok(Value::Nil.to_rc_value()),
                                    }
                                }
                                _ => Ok(Value::Nil.to_rc_value()),
                            },
                            Err(e) => error!(format_error(e)),
                        }
                    }
                    _ => {
                        // At this point we just have to execute the function
                        let ref f = match eval_to_rc(head.clone(), mut_env.borrow().clone()) {
                            Ok(x) => x,
                            Err(e) => return Err(process_error(&e, &ast)),
                        };
                        match &*f.borrow() {
                            // Internal FMLisp function
                            Value::Func(_, _) => {
                                let mut evaled_args = vec![];
                                for a in args {
                                    match eval(a.clone(), mut_env.borrow().clone()) {
                                        Ok(v) => evaled_args.push(v),
                                        Err(e) => {
                                            return Err(process_error(&e, &ast));
                                        }
                                    }
                                }

                                match f.apply(evaled_args, mut_env.borrow().clone()) {
                                    Ok(ret) => Ok(ret.to_rc_value()),
                                    Err(e) => {
                                        println!("error: {}\n{:?}", ast, e);
                                        return Err(process_error(&e, &ast));
                                    }
                                }
                            }
                            // An internal macro, is a function which the parameter are not evaluated,
                            // but passed to the function as is (keeping symbols).
                            Value::Macro(_, _) => {
                                match f.apply(args.clone(), mut_env.borrow().clone()) {
                                    Ok(ret) => Ok(ret.to_rc_value()),
                                    Err(e) => return Err(process_error(&e, &ast)),
                                }
                            }
                            // Lambda function defined by the user
                            Value::Lambda {
                                ast: mast,
                                env: menv,
                                params,
                                ..
                            } => {
                                let lambda_ast = &**mast;
                                let mut bind_args = vec![];
                                for arg in args.iter() {
                                    match eval(arg.clone(), mut_env.borrow().clone()) {
                                        Ok(v) => bind_args.push(v),
                                        Err(e) => return Err(process_error(&e, &ast)),
                                    }
                                }

                                let args_count = bind_args.len();
                                if let Some((a, p)) =
                                    find_ast_and_params_by_arity(lambda_ast, params, args_count)
                                {
                                    // Now bind the arguments to the lambda params in a new env
                                    let new_env = match menv.bind(p, bind_args) {
                                        Ok(v) => v,
                                        Err(e) => return Err(process_error(&e, &ast)),
                                    };
                                    ast = (*a).clone();
                                    mut_env.replace(new_env);
                                    continue 'tco;
                                } // else
                                return error!(format!(
                                    "Wrong number of arguments ({}) passed to function {}",
                                    args_count, f
                                ));
                            }
                            _ => error!(format!("Attempt to call non-function {}", f)),
                        }
                    }
                }
            }
            _ => eval_ast(ast, mut_env.borrow().clone()),
        };

        break;
    }

    ret
}

pub fn eval(ast: Value, env: Rc<Environment>) -> ValueRes {
    Ok(eval_to_rc(ast, env)?.to_value())
}
