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

pub fn string_functions() -> Vec<(&'static str, Value)> {
    vec![("starts-with?", func(starts_with_q))]
}
