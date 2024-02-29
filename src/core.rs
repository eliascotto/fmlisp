use itertools::Itertools;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::env::Environment;
use crate::lang;
use crate::symbol::Symbol;
use crate::values::LispError::{self, ErrString, ErrValue};
use crate::values::{ExprArgs, ToValue, Value, ValueRes};
use crate::var::Var;

pub fn load_core(env: &Environment) {
    env.change_or_create_namespace(&Symbol::new("fmlisp.core"));

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
                    Some(val) => {
                        match (*val).clone() {
                            f @ Value::DefinedFunc { is_macro: true, .. } => {
                                // Evaluate the arguments before apply the macro fn
                                let args = v[1..]
                                    .iter()
                                    .map(|v| eval(v.clone(), env.clone()).unwrap())
                                    .collect();
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

fn macroexpand(mut ast: Value, env: Rc<Environment>) -> (bool, Result<Rc<Value>, LispError>) {
    let mut was_expanded = false;
    while let Some((mf, args)) = is_macro_call(ast.clone(), env.clone()) {
        ast = match mf.apply(args, env.clone()) {
            Ok(a) => a,
            Err(e) => return (false, Err(e)),
        };
        was_expanded = true;
    }
    (was_expanded, Ok(Rc::new(ast)))
}

fn eval_ast(ast: Value, env: Rc<Environment>) -> Result<Rc<Value>, LispError> {
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

pub fn eval_to_rc(mut ast: Value, env: Rc<Environment>) -> Result<Rc<Value>, LispError> {
    let ret: Result<Rc<Value>, LispError>;
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
    // TCO is used by let, apply, if, quasiquote do, try/catch
    'tco: loop {
        ret = match ast.clone() {
            Value::List(l, _) => {
                if l.len() == 0 {
                    return Ok(ast.to_rc_value());
                }

                // catch a macro symbol before eval the list
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
                                let sym_meta = sym.meta();
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
                            _ => Err(ErrString(
                                "First argument to def must be a symbol".to_string(),
                            )),
                        }
                    }
                    Value::Symbol(ref headsym) if headsym.name() == "let" => {
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
                    Value::Symbol(ref headsym) if headsym.name() == "fn" => {
                        if args.len() < 1 {
                            return error!(
                                "Wrong number of arguments given to fn. Expecting at least 1"
                            );
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
                            env: mut_env.borrow().clone(),
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
                        if args.len() < 2 {
                            return error!(
                                "Wrong number of arguments given to try. Expecting at least 2"
                            );
                        }
                        match eval(args[0].clone(), mut_env.borrow().clone()) {
                            Err(ref e) if args.len() >= 2 => {
                                let ex = match e {
                                    ErrValue(v) => v.clone(),
                                    ErrString(s) => Value::Str(s.to_string()),
                                };
                                match args[1].clone() {
                                    Value::List(c, _) => {
                                        let catch_env = mut_env
                                            .borrow()
                                            .bind(list![c[1].clone()].to_rc_value(), vec![ex])?;
                                        Ok(eval_to_rc(c[2].clone(), Rc::new(catch_env))?)
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
                        let ref f = eval_to_rc(head.clone(), mut_env.borrow().clone())?;
                        match &*f.borrow() {
                            Value::Func(_, _) => {
                                let evaled_args = args
                                    .iter()
                                    .map(|rc_arg| {
                                        eval(rc_arg.clone(), mut_env.borrow().clone()).unwrap()
                                    })
                                    .collect::<Vec<Value>>();
                                let ret = f.apply(evaled_args, mut_env.borrow().clone())?;
                                Ok(ret.to_rc_value())
                            }
                            // An internal macro, is a function which the parameter are not evaluated,
                            // but passed to the function as is (so keeping symbols etc).
                            Value::Macro(_, _) => {
                                let ret = f.apply(args.clone(), mut_env.borrow().clone())?;
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
                                    .map(|rc_arg| {
                                        eval(rc_arg.clone(), mut_env.borrow().clone()).unwrap()
                                    })
                                    .collect::<Vec<Value>>();
                                let new_env = menv.bind(params.clone(), evaled_args)?;
                                ast = a.borrow().clone();
                                mut_env.replace(Rc::new(new_env));
                                continue 'tco;
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
