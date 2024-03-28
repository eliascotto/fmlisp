use std::rc::Rc;

use crate::env::Environment;
use crate::lang::commons;
use crate::values::{func, ExprArgs, LispErr, Value, ValueRes};

fn equiv(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.is_empty() {
        return Ok(Value::Bool(true));
    }

    let first_value = match args[0].clone() {
        Value::Integer(i) => i as f64,
        Value::Float(f) => f as f64,
        _ => {
            return error_fmt!(
                "Arguments to equiv must be numbers, received {}",
                args[0].pr_str()
            );
        }
    };

    for a in args {
        let num = match a.clone() {
            Value::Integer(i) => i as f64,
            Value::Float(f) => f as f64,
            _ => {
                return error_fmt!(
                    "Arguments to equiv must be numbers, received {}",
                    a.pr_str()
                );
            }
        };
        if num.to_bits() != first_value.to_bits() {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

pub fn numbers_functions() -> Vec<(&'static str, Value)> {
    vec![("equiv", func(equiv))]
}

pub fn load(env: Rc<Environment>) {
    commons::load_module(env, "fmlisp.lang.numbers", numbers_functions());
}
