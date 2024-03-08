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

// Transform string into a FMLisp Value
pub fn read(s: &str) -> ValueRes {
    reader::read_str(s.to_string())
}

// Transform the Value to string
pub fn print(exp: Value) -> String {
    exp.pr_str()
}

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

/// Returns a macro function and the arguments, if found or None
fn is_macro_call(ast: Value, env: Rc<Environment>) -> Option<(Value, ExprArgs)> {
    match ast {
        Value::List(v, _) => match v[0] {
            Value::Symbol(ref s) => {
                // let val = self.get_symbol_value(s);
                match env.get_symbol_value(s) {
                    Some(val) => match (*val).clone() {
                        f @ Value::Lambda { is_macro: true, .. } => Some((f, v[1..].to_vec())),
                        _ => None,
                    },
                    None => None,
                }
            }
            _ => None,
        },
        _ => None,
    }
}

// Expand the Macro call, if found in the code. It returns a tuple
// with `true` if a macro has been expanded, and the expanded AST, false otherwise.
fn macroexpand(mut ast: Value, env: Rc<Environment>) -> (bool, Result<Rc<Value>, LispErr>) {
    let mut was_expanded = false;
    while let Some((mf, args)) = is_macro_call(ast.clone(), env.clone()) {
        ast = match mf.apply(args, env.clone()) {
            Ok(a) => a,
            Err(e) => {
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
        was_expanded = true;
    }
    (was_expanded, Ok(Rc::new(ast)))
}

fn eval_ast(ast: Value, env: Rc<Environment>) -> Result<Rc<Value>, LispErr> {
    match ast.clone() {
        // Symbol returns value in env 'sym ;=> 12
        Value::Symbol(sym) => match env.get_symbol_value(&sym) {
            Some(val) => Ok(val),
            None => error!(format!("'{}' symbol not found in this context", sym)),
        },
        // Vector eval each item [1 2 (+ 2 3)] ;=> [1 2 5]
        Value::Vector(v, _) => {
            let evaled: Vec<Value> = v
                .iter()
                .map(|x| eval(x.clone(), env.clone()).unwrap())
                .collect();
            Ok(Value::Vector(Rc::new(evaled), None).to_rc_value())
        }
        // HashMap similar to vector, eval each key/value
        Value::HashMap(hm, meta) => {
            let mut new_hm = HashMap::new();
            for (k, v) in hm.iter() {
                new_hm.insert(
                    eval(k.clone(), env.clone()).unwrap(),
                    eval(v.clone(), env.clone()).unwrap(),
                );
            }
            Ok(Value::HashMap(Rc::new(new_hm), meta.clone()).to_rc_value())
        }
        Value::List(l, _) => {
            let evaled: Vec<Value> = l
                .iter()
                .map(|x| eval(x.clone(), env.clone()).unwrap())
                .collect();
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
                        return eval_to_rc((*new_ast).clone(), mut_env.borrow().clone())
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
                                    eval_to_rc(args[1].clone(), mut_env.borrow().clone())?
                                } else {
                                    Value::Nil.to_rc_value()
                                };
                                let sym_meta = sym.meta();

                                // Copy Meta from symbol into the Lambda definition
                                let value = match &*value {
                                    Value::Lambda {
                                        ast, env, params, ..
                                    } => {
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
                                        "Binding vector requires an even number of forms"
                                    );
                                }

                                // Clone the environment, creating a local one
                                let local_env = Rc::new(mut_env.borrow().new_local());
                                for (b, e) in binds.iter().tuples() {
                                    match b {
                                        Value::Symbol(sym) => {
                                            let val = eval_to_rc(e.clone(), local_env.clone())?;
                                            let var = Var::new(
                                                mut_env.borrow().get_current_namespace_symbol(),
                                                (*sym).clone(),
                                                val,
                                            );
                                            // Insert the let bindings into the local env, as Vars
                                            local_env.insert(
                                                (*sym).clone(),
                                                Value::Var(var).to_rc_value(),
                                            );
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
                                eval_to_rc(a.clone(), mut_env.borrow().clone())?;
                            }

                            // Get the last form (or nil) and evalue it
                            let new_ast = args.last().unwrap_or(&Value::Nil).clone();
                            ast = new_ast;
                            continue 'tco;
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "if" => {
                        if args.len() < 2 || args.len() > 3 {
                            return error!(
                                "Wrong number of arguments given to if. Expecting 2 or 3"
                            );
                        }
                        let cond = eval(args[0].clone(), mut_env.borrow().clone())?;
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
                        ast = eval(args[0].clone(), mut_env.borrow().clone())?;
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
                        Ok(Rc::new(quasiquote(&args[0])))
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "quasiquote" => {
                        if args.len() != 1 {
                            return error!(
                                "Wrong number of arguments given to quasiquote. Expecting 1"
                            );
                        }
                        ast = quasiquote(&args[0]);
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
                        let r = eval(macro_args, mut_env.borrow().clone())?;
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
                        let ref f = eval_to_rc(head.clone(), mut_env.borrow().clone())?;
                        match &*f.borrow() {
                            // Internal FMLisp function
                            Value::Func(_, _) => {
                                let evaled_args = args
                                    .iter()
                                    .map(|rc_arg| {
                                        eval(rc_arg.clone(), mut_env.borrow().clone()).unwrap()
                                    })
                                    .collect::<Vec<Value>>();

                                match f.apply(evaled_args, mut_env.borrow().clone()) {
                                    Ok(ret) => Ok(ret.to_rc_value()),
                                    Err(e) => match e.clone() {
                                        // Load trace in case of apply raised an error
                                        LispErr::Error(mut err) => {
                                            err.add_trace(ast.clone());
                                            Err(e)
                                        }
                                        _ => Err(e),
                                    },
                                }
                            }
                            // An internal macro, is a function which the parameter are not evaluated,
                            // but passed to the function as is (keeping symbols).
                            Value::Macro(_, _) => {
                                match f.apply(args.clone(), mut_env.borrow().clone()) {
                                    Ok(ret) => Ok(ret.to_rc_value()),
                                    Err(e) => match e.clone() {
                                        // Load trace in case of apply raised an error
                                        LispErr::Error(mut err) => {
                                            err.add_trace(ast.clone());
                                            Err(e)
                                        }
                                        _ => Err(e),
                                    },
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
                                let bind_args = args
                                    .iter()
                                    .map(|rc_arg| {
                                        eval(rc_arg.clone(), mut_env.borrow().clone()).unwrap()
                                    })
                                    .collect::<Vec<Value>>();

                                let args_count = bind_args.len();
                                if let Some((p, a)) =
                                    find_ast_and_params_by_arity(lambda_ast, params, args_count)
                                {
                                    // Now bind the arguments to the lambda params in a new env
                                    let new_env = menv.bind(p, bind_args)?;
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
