use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};

use crate::env::Environment;
use crate::error_output;
use crate::lang;
use crate::reader;
use crate::symbol::Symbol;
use crate::values::LispErr::{self};
use crate::values::{ToValue, Value, ValueRes};

/// Global counter for gensym ID, Atomic
static ID: AtomicI64 = AtomicI64::new(0);

/// Return next global ID
pub fn next_id() -> i64 {
    ID.fetch_add(1, Ordering::SeqCst)
}

/// Load the language core functions and symbols into the namespace,
/// also reads the fmlisp libraries files.
pub fn load_lang_core(env: Rc<Environment>) -> Result<(), LispErr> {
    env.change_or_create_namespace(&sym!("fmlisp.lang"));

    // Loading internal definitions
    for (k, v) in lang::core::internal_symbols(&env) {
        env.insert_var(Symbol::new(k), v.to_rc_value());
    }

    let fns = vec![lang::core::core_functions()];

    for fs in fns {
        for (k, v) in fs {
            // Namespace mapping Symbol - Rc<Value>
            // Load them into clojure.core ns
            env.insert_var(Symbol::new(k), v.to_rc_value());
        }
    }

    lang::numbers::load(env.clone());
    lang::namespaces::load(env.clone());
    lang::strings::load(env.clone());

    env.change_or_create_namespace(&sym!("fmlisp.lang"));

    // Load language file
    // load_file("src/fmlisp/test.fml", env.clone());
    let lang_files = vec!["core", "string"];
    for file_name in lang_files.iter() {
        match load_file(
            format!("src/fmlisp/{}.fml", file_name).as_str(),
            env.clone(),
        ) {
            0 => {}
            -1 => return error_fmt!("Error loading file ({}.fml)", file_name),
            _ => {}
        }
    }

    // Change to default user namespace
    env.change_or_create_namespace(&Symbol::new("user"));
    // Automatically refer fmlisp.core
    let _ = env.add_referred_namespace(&sym!("fmlisp.core"));

    Ok(())
}

/// Transform string into a FMLisp Value.
pub fn read(s: &str, env: Rc<Environment>) -> ValueRes {
    reader::read_str(s.to_string(), env)
}

/// Transform the Value to string.
pub fn print(exp: Value) -> String {
    exp.pr_str()
}

/// Read and eval a file
fn load_file(path: &str, env: Rc<Environment>) -> i8 {
    match lang::commons::load_file(&String::from(path), env.clone()) {
        Ok(_) => 0,
        Err(e) => error_output::eprint(e, path),
    }
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

// Implementing the function arity returning MIN-MAX since using
// variadic functions, we can have a min number of arguments to be called
// and then max as usize::MAX
fn compute_fn_arity(params: Rc<Vec<Value>>) -> (usize, usize) {
    let mut arity: usize = 0;
    for p in params.iter() {
        match p {
            Value::Symbol(sym) => {
                // Variadic function
                if sym.name() == "&" {
                    // If & let's just use the max value available
                    return (arity, usize::MAX);
                }
                arity += 1;
            }
            _ => panic!(
                "Expected a vector of symbols for arguments, found {:?}",
                params
            ),
        }
    }
    (arity, arity)
}

/// Returns a tuple ast-params based on the function's arity needed
pub fn find_ast_and_params_by_arity(
    ast: &[Rc<Value>],
    params: &[Rc<Value>],
    arity: usize,
) -> Option<(Rc<Value>, Rc<Value>)> {
    // Get the index of the correct arity for params.
    // Then use the corresponding params and ast for executing the Lambda.
    for (index, param) in params.iter().enumerate() {
        if let Value::Vector(v, _) = &**param {
            let (min, max) = compute_fn_arity(v.clone());
            if min == max {
                // If standard function body, check arity equality
                if max == arity {
                    return Some((ast[index].clone(), params[index].clone()));
                }
            } else {
                // If variadic function, check min and max
                if arity > min && arity < max {
                    return Some((ast[index].clone(), params[index].clone()));
                }
            }
        }
    }
    None
}
