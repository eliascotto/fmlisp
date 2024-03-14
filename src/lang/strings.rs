use crate::env::Environment;
use crate::values::{error, func, ExprArgs, Value, ValueRes};
use std::rc::Rc;

fn starts_with_q(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error("Wrong number of arguments passed to starts-with?. Expecting 2");
    }
    match (args[0].clone(), args[1].clone()) {
        (Value::Str(s), Value::Str(substr)) => {
            let res = s.starts_with(&substr);
            Ok(Value::Bool(res))
        }
        _ => error("Expecting two strings"),
    }
}

fn index_of(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error("Wrong number of arguments passed to index-of. Expecting 2");
    }
    match (args[0].clone(), args[1].clone()) {
        (Value::Str(s), Value::Str(substr)) => {
            let index = if let Some(idx) = s.find(&substr) {
                idx as i64
            } else {
                -1
            };
            Ok(Value::Integer(index))
        }
        _ => error("Expecting two strings"),
    }
}

fn concat_str(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    let mut ret = vec![];
    for seq in a.iter() {
        match seq {
            Value::Str(s) => ret.push(s.clone()),
            _ => return error("non-coll passed to concat"),
        }
    }
    Ok(Value::Str(ret.join("")))
}

pub fn string_functions() -> Vec<(&'static str, Value)> {
    vec![
        ("starts-with?", func(starts_with_q)),
        ("index-of", func(index_of)),
        ("concat-str", func(concat_str)),
    ]
}
