use crate::env::Environment;
use crate::values::{
    self, _assoc, _dissoc, error, func, hash_map_from_vec, list_from_vec, macro_fn, set_from_vec,
    vector_from_vec, ExprArgs, LispError, Value, ValueRes,
};

fn starts_with_q(args: ExprArgs, env: &Environment) -> ValueRes {
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
