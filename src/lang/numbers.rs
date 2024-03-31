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

fn min(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to min. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => {
            if n0 > n1 {
                Ok(Value::Integer(n1))
            } else {
                Ok(Value::Integer(n0))
            }
        }
        (Value::Integer(n0), Value::Float(n1)) => {
            if (n0 as f64) > n1 {
                Ok(Value::Float(n1))
            } else {
                Ok(Value::Integer(n0))
            }
        }
        (Value::Float(n0), Value::Integer(n1)) => {
            if n0 > n1 as f64 {
                Ok(Value::Integer(n1))
            } else {
                Ok(Value::Float(n0))
            }
        }
        (Value::Float(n0), Value::Float(n1)) => {
            if n0 > n1 {
                Ok(Value::Float(n1))
            } else {
                Ok(Value::Float(n0))
            }
        }
        _ => error_fmt!("min requires numeric values"),
    }
}

fn max(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to min. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => {
            if n0 < n1 {
                Ok(Value::Integer(n1))
            } else {
                Ok(Value::Integer(n0))
            }
        }
        (Value::Integer(n0), Value::Float(n1)) => {
            if (n0 as f64) < n1 {
                Ok(Value::Float(n1))
            } else {
                Ok(Value::Integer(n0))
            }
        }
        (Value::Float(n0), Value::Integer(n1)) => {
            if n0 < n1 as f64 {
                Ok(Value::Integer(n1))
            } else {
                Ok(Value::Float(n0))
            }
        }
        (Value::Float(n0), Value::Float(n1)) => {
            if n0 < n1 {
                Ok(Value::Float(n1))
            } else {
                Ok(Value::Float(n0))
            }
        }
        _ => error_fmt!("max requires numeric values"),
    }
}

fn abs(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error_fmt!("Wrong number of arguments passed to abs. Expecting 1");
    }

    match args[0].clone() {
        Value::Integer(i) => Ok(Value::Integer(i.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => error_fmt!("abs requires a numeric value"),
    }
}

fn quot(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error_fmt!("Wrong number of arguments passed to quot. Expecting 1");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 / n1)),
        (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 / n1)),
        (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 / n1 as f64)),
        (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 / n1)),
        _ => error_fmt!("quot requires numeric values"),
    }
}

fn reminder(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error_fmt!("Wrong number of arguments passed to reminder. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 % n1)),
        (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 % n1)),
        (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 % n1 as f64)),
        (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 % n1)),
        _ => error_fmt!("reminder requires numeric values"),
    }
}

fn bit_not(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error_fmt!("Wrong number of arguments passed to bit-not. Expecting 1");
    }

    match args[0].clone() {
        Value::Integer(i) => Ok(Value::Integer(!i)),
        _ => error_fmt!(
            "bit-not operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_and(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-and. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 & n1)),
        _ => error_fmt!(
            "bit-and operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_or(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-or. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 | n1)),
        _ => error_fmt!(
            "bit-or operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_xor(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-xor. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 ^ n1)),
        _ => error_fmt!(
            "bit-xor operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_and_not(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-and-not. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 & !n1)),
        _ => error_fmt!(
            "bit-and-not operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_clear(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-clear. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(x), Value::Integer(n)) => Ok(Value::Integer(x & !(1i64 << n))),
        _ => error_fmt!(
            "bit-clear operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_set(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-set. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(x), Value::Integer(n)) => Ok(Value::Integer(x | (1i64 << n))),
        _ => error_fmt!(
            "bit-set operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_flip(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-flip. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(x), Value::Integer(n)) => Ok(Value::Integer(x ^ (1i64 << n))),
        _ => error_fmt!(
            "bit-flip operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_test(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-test. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(x), Value::Integer(n)) => Ok(Value::Bool((x & (1i64 << n)) != 0)),
        _ => error_fmt!(
            "bit-test operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_shift_left(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-shift-left. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(x), Value::Integer(n)) => Ok(Value::Integer(x << n)),
        _ => error_fmt!(
            "bit-shift-left operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn bit_shift_right(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error_fmt!("Wrong number of arguments passed to bit-shift-right. Expecting 2");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Integer(x), Value::Integer(n)) => Ok(Value::Integer(x >> n)),
        _ => error_fmt!(
            "bit-shift-right operation not supported for type {}",
            args[0].pr_str()
        ),
    }
}

fn pos_q(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return argument_error!("Wrong number of arguments passed to pos?. Expecting 1");
    }

    match args[0].clone() {
        Value::Integer(i) => Ok(Value::Bool(i > 0)),
        Value::Float(f) => Ok(Value::Bool(f > 0.0)),
        _ => error_fmt!("pos? requires a numeric value"),
    }
}

fn neg_q(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return argument_error!("Wrong number of arguments passed to negs?. Expecting 1");
    }

    match args[0].clone() {
        Value::Integer(i) => Ok(Value::Bool(i < 0)),
        Value::Float(f) => Ok(Value::Bool(f < 0.0)),
        _ => error_fmt!("negs? requires a numeric value"),
    }
}

fn zero_q(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return argument_error!("Wrong number of arguments passed to zero?. Expecting 1");
    }

    match args[0] {
        Value::Integer(i) => Ok(Value::Bool(i == 0)),
        Value::Float(f) => Ok(Value::Bool(f == 0.0)),
        _ => error_fmt!("zero? requires a numeric value"),
    }
}

fn int(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return argument_error!("Wrong number of arguments passed to int. Expecting 1");
    }

    match args[0] {
        Value::Float(f) => Ok(Value::Integer(f as i64)),
        _ => error_fmt!("int requires a numeric value"),
    }
}

fn inc(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return argument_error!("Wrong number of arguments passed to inc. Expecting 1");
    }

    match args[0] {
        Value::Integer(i) => Ok(Value::Integer(i + 1)),
        Value::Float(f) => Ok(Value::Float(f + 1.0)),
        _ => error_fmt!("inc requires a numeric value"),
    }
}

fn dec(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return argument_error!("Wrong number of arguments passed to inc. Expecting 1");
    }

    match args[0] {
        Value::Integer(i) => Ok(Value::Integer(i - 1)),
        Value::Float(f) => Ok(Value::Float(f - 1.0)),
        _ => error_fmt!("inc requires a numeric value"),
    }
}

fn mul(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() < 2 {
        return error_fmt!("Wrong number of arguments passed to *. Expecting at least 2");
    }

    fn mul_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 * n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 * n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 * n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 * n1)),
            _ => error_fmt!("expecting number for all arguments"),
        }
    }

    let mut result = mul_fn(a[0].clone(), a[1].clone())?; // Apply function to the first two arguments
    for x in a[2..].iter() {
        result = mul_fn(result, x.clone())?; // Apply function to the previous result and the current argument
    }
    Ok(result)
}

fn div(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() < 2 {
        return error_fmt!("Wrong number of arguments passed to /. Expecting at least 2");
    }

    fn div_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 / n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 / n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 / n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 / n1)),
            _ => error_fmt!("expecting number for all arguments"),
        }
    }

    let mut result = div_fn(a[0].clone(), a[1].clone())?; // Apply function to the first two arguments
    for x in a[2..].iter() {
        result = div_fn(result, x.clone())?; // Apply function to the previous result and the current argument
    }
    Ok(result)
}

fn add(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() < 2 {
        return error_fmt!("Wrong number of arguments passed to +. Expecting at least 2");
    }

    fn add_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 + n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 + n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 + n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 + n1)),
            _ => error_fmt!("expecting number for all arguments"),
        }
    }

    let mut result = add_fn(a[0].clone(), a[1].clone())?; // Apply function to the first two arguments
    for x in a[2..].iter() {
        result = add_fn(result, x.clone())?; // Apply function to the previous result and the current argument
    }
    Ok(result)
}

fn minus(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() < 1 {
        return error_fmt!("Wrong number of arguments passed to minus. Expecting at least 1");
    }

    if args.len() == 1 {
        let ret = match args[0] {
            Value::Integer(n0) => Ok(Value::Integer(-n0)),
            Value::Float(n0) => Ok(Value::Float(-n0 as f64)),
            _ => error_fmt!("expecting number for all arguments"),
        };
        return ret;
    }

    fn sub_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 - n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 - n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 - n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 - n1)),
            _ => error_fmt!("expecting number for all arguments"),
        }
    }

    let mut result = sub_fn(args[0].clone(), args[1].clone())?; // Apply function to the first two arguments
    for x in args[2..].iter() {
        result = sub_fn(result, x.clone())?; // Apply function to the previous result and the current argument
    }
    Ok(result)
}

pub fn numbers_functions() -> Vec<(&'static str, Value)> {
    vec![
        ("add", func(add)),
        ("minus", func(minus)),
        ("multiply", func(mul)),
        ("divide", func(div)),
        ("int", func(int)),
        ("int?", func(fn_is_type!(Value::Integer(_)))),
        ("zero?", func(zero_q)),
        ("pos?", func(pos_q)),
        ("neg?", func(neg_q)),
        ("inc", func(inc)),
        ("dec", func(dec)),
        ("equiv", func(equiv)),
        ("min", func(min)),
        ("max", func(max)),
        ("abs", func(abs)),
        ("quot", func(quot)),
        ("reminder", func(reminder)),
        ("bit-not", func(bit_not)),
        ("bit-and", func(bit_and)),
        ("bit-or", func(bit_or)),
        ("bit-xor", func(bit_xor)),
        ("bit-and-not", func(bit_and_not)),
        ("bit-clear", func(bit_clear)),
        ("bit-set", func(bit_set)),
        ("bit-flip", func(bit_flip)),
        ("bit-test", func(bit_test)),
        ("bit-shift-left", func(bit_shift_left)),
        ("bit-shift-right", func(bit_shift_right)),
    ]
}

pub fn load(env: Rc<Environment>) {
    commons::load_module(env, "fmlisp.lang.numbers", numbers_functions());
}
