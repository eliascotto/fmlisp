use std::cell::RefCell;
use std::fs::read_to_string;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::core;
use crate::env::Environment;
use crate::errors;
use crate::reader;
use crate::symbol::Symbol;
use crate::values::LispErr::ErrValue;
use crate::values::{
    self, _assoc, _dissoc, error, func, hash_map_from_vec, list_from_vec, macro_fn, set_from_vec,
    vector_from_vec, ExprArgs, LispErr, ToValue, Value, ValueRes,
};

/// Macro that receive an Integer and eval the expr.
/// Returns an error if not Integer received.
macro_rules! fn_t_num {
    ($ret:ident, $fn:expr) => {{
        |a: ExprArgs, _env: Rc<Environment>| match a[0].clone() {
            Value::Integer(a0) => Ok($ret($fn(a0))),
            _ => error("expecting numeric argument"),
        }
    }};
}

/// Macro that receive two Integers and eval the expr
/// Returns an error if not Integers received.
macro_rules! fn_t_num_num {
    ($ret:ident, $fn:expr) => {{
        |a: ExprArgs, _env: Rc<Environment>| match (a[0].clone(), a[1].clone()) {
            (Value::Integer(a0), Value::Integer(a1)) => Ok($ret($fn(a0, a1))),
            _ => error("expecting (num,num) args"),
        }
    }};
}

/// Check if value 0 is of certain type. Can handle multiple types.
macro_rules! fn_is_type {
    ($($ps:pat), *) => {{
      |a: ExprArgs, _env: Rc<Environment>| { Ok(Value::Bool(match a[0] { $($ps => true,)* _ => false})) }
    }};
    ($p:pat if $e:expr) => {{
      |a: ExprArgs, _env: Rc<Environment>| { Ok(Value::Bool(match a[0] { $p if $e => true, _ => false})) }
    }};
    ($p:pat if $e:expr, $($ps:pat), *) => {{
      |a: ExprArgs, _env: Rc<Environment>| { Ok(Value::Bool(match a[0] { $p if $e => true, $($ps => true,)* _ => false})) }
    }};
}

/// Returns the file content evaluated, or an error.
pub fn load_file(path: &String, env: Rc<Environment>) -> ValueRes {
    match read_to_string(path) {
        Ok(data) => {
            // We use `do` to read the entire file as a single sexpr
            let file_data = format!("(do {})", data);
            let s = reader::read_str(file_data)?;
            core::eval(s, env.clone())
        }
        Err(e) => error(&e.to_string()),
    }
}

fn add(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() < 2 {
        return error("Wrong number of arguments passed to +. Expecting at least 2");
    }

    fn add_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 + n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 + n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 + n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 + n1)),
            _ => error("expecting number for all arguments"),
        }
    }

    let mut result = add_fn(a[0].clone(), a[1].clone())?; // Apply function to the first two arguments
    for x in a[2..].iter() {
        result = add_fn(result, x.clone())?; // Apply function to the previous result and the current argument
    }
    Ok(result)
}

fn sub(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() < 2 {
        return error("Wrong number of arguments passed to -. Expecting at least 2");
    }

    fn sub_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 - n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 - n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 - n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 - n1)),
            _ => error("expecting number for all arguments"),
        }
    }

    let mut result = sub_fn(a[0].clone(), a[1].clone())?; // Apply function to the first two arguments
    for x in a[2..].iter() {
        result = sub_fn(result, x.clone())?; // Apply function to the previous result and the current argument
    }
    Ok(result)
}

fn mul(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() < 2 {
        return error("Wrong number of arguments passed to *. Expecting at least 2");
    }

    fn mul_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 * n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 * n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 * n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 * n1)),
            _ => error("expecting number for all arguments"),
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
        return error("Wrong number of arguments passed to /. Expecting at least 2");
    }

    fn div_fn(a0: Value, a1: Value) -> ValueRes {
        match (a0, a1) {
            (Value::Integer(n0), Value::Integer(n1)) => Ok(Value::Integer(n0 / n1)),
            (Value::Integer(n0), Value::Float(n1)) => Ok(Value::Float(n0 as f64 / n1)),
            (Value::Float(n0), Value::Integer(n1)) => Ok(Value::Float(n0 / n1 as f64)),
            (Value::Float(n0), Value::Float(n1)) => Ok(Value::Float(n0 / n1)),
            _ => error("expecting number for all arguments"),
        }
    }

    let mut result = div_fn(a[0].clone(), a[1].clone())?; // Apply function to the first two arguments
    for x in a[2..].iter() {
        result = div_fn(result, x.clone())?; // Apply function to the previous result and the current argument
    }
    Ok(result)
}

fn read_string(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to read-string. Expecting 1");
    }
    reader::read_str(a[0].to_string())
}

fn slurp(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to slurp. Expecting 1");
    }

    match a[0] {
        Value::Str(ref str) => match read_to_string(str) {
            Ok(data) => Ok(Value::Str(data)),
            Err(e) => error(&e.to_string()),
        },
        _ => error("slurp expects a string"),
    }
}

fn cons(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[1].clone() {
        Value::List(v, _) | Value::Vector(v, _) => {
            let mut ret = vec![a[0].clone()];
            ret.extend_from_slice(&v);
            Ok(list_from_vec(ret.to_vec()))
        }
        _ => error("cons expects seq as second arg"),
    }
}

fn concat(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    let mut ret = vec![];
    for seq in a.iter() {
        match seq {
            Value::List(v, _) | Value::Vector(v, _) => ret.extend_from_slice(v),
            _ => return error("non-coll passed to concat"),
        }
    }
    Ok(list_from_vec(ret.to_vec()))
}

fn vec(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0].clone() {
        Value::List(l, _) | Value::Vector(l, _) => Ok(vector_from_vec(l.to_vec())),
        Value::HashMap(hm, _) => {
            let mut ret = vec![];
            for (k, v) in hm.iter() {
                ret.push(vector![k.clone(), v.clone()]);
            }
            Ok(vector_from_vec(ret))
        }
        Value::Str(s) => Ok(list_from_vec(s.chars().map(Value::Char).collect())),
        Value::Nil => Ok(vector![]),
        _ => error("vec called with non-seq"),
    }
}

fn nth(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match (a[0].clone(), a[1].clone()) {
        (Value::List(seq, _), Value::Integer(idx))
        | (Value::Vector(seq, _), Value::Integer(idx)) => {
            if seq.len() <= idx as usize {
                return error("nth: index out of range");
            }
            Ok(seq[idx as usize].clone())
        }
        (Value::Str(_), Value::Integer(idx)) => a[0].char_at(idx as usize),
        _ => error("invalid args to nth"),
    }
}

fn first(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to first. Expecting 1");
    }
    match a[0].clone() {
        Value::List(seq, _) | Value::Vector(seq, _) if seq.len() == 0 => Ok(Value::Nil),
        Value::List(seq, _) | Value::Vector(seq, _) => Ok(seq[0].clone()),
        Value::Str(s) if s.len() == 0 => Ok(Value::Nil),
        Value::Str(s) => Ok(Value::Char(s.chars().nth(0).unwrap())),
        Value::Nil => Ok(Value::Nil),
        _ => error("first called with non-seq"),
    }
}

fn second(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to second. Expecting 1");
    }
    match a[0].clone() {
        Value::List(seq, _) | Value::Vector(seq, _) if seq.len() < 2 => Ok(Value::Nil),
        Value::List(seq, _) | Value::Vector(seq, _) => Ok(seq[1].clone()),
        Value::Str(s) if s.len() < 2 => Ok(Value::Nil),
        Value::Str(s) => Ok(Value::Char(s.chars().nth(2).unwrap())),
        Value::Nil => Ok(Value::Nil),
        _ => error("second called with non-seq"),
    }
}

fn rest(a: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to rest. Expecting 1");
    }
    match a[0].clone() {
        Value::List(seq, _) | Value::Vector(seq, _) => {
            if seq.len() > 1 {
                Ok(list_from_vec(seq[1..].to_vec()))
            } else {
                Ok(list![])
            }
        }
        Value::Str(_) => rest(vec![a[0].to_chars_list().unwrap()], env),
        Value::Nil => Ok(list![]),
        _ => error("rest called with non-seq"),
    }
}

fn next(a: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to next. Expecting 1");
    }
    match a[0].clone() {
        Value::List(seq, _) | Value::Vector(seq, _) => {
            if seq.len() > 1 {
                Ok(list_from_vec(seq[1..].to_vec()))
            } else {
                Ok(Value::Nil)
            }
        }
        Value::Str(_) => next(vec![a[0].to_chars_list().unwrap()], env),
        Value::Nil => Ok(Value::Nil),
        _ => error("next called with non-seq"),
    }
}

fn last(a: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to last. Expecting 1");
    }
    match a[0].clone() {
        Value::List(seq, _) | Value::Vector(seq, _) => {
            Ok(seq.last().unwrap_or(&Value::Nil).clone())
        }
        Value::Str(_) => last(vec![a[0].to_chars_list().unwrap()], env),
        Value::Nil => Ok(Value::Nil),
        _ => error("last called with non-seq"),
    }
}

fn butlast(a: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if a.len() != 1 {
        return error("Wrong number of arguments passed to butlast. Expecting 1");
    }
    match a[0].clone() {
        Value::List(seq, _) | Value::Vector(seq, _) => {
            if seq.len() > 1 {
                Ok(list_from_vec(seq[..seq.len() - 1].to_vec()))
            } else {
                Ok(Value::Nil)
            }
        }
        Value::Str(_) => butlast(vec![a[0].to_chars_list().unwrap()], env),
        Value::Nil => Ok(Value::Nil),
        _ => error("butlast called with non-seq"),
    }
}

fn subvec(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() < 2 || args.len() > 3 {
        return error("Wrong number of arguments passed to subvec. Expecting 2 or 3");
    }
    match args[0].clone() {
        Value::Vector(seq, _) => match args[1].clone() {
            Value::Integer(start) => {
                let end = match args.get(2) {
                    Some(n) => match n {
                        Value::Integer(end) => Some(end),
                        _ => return error("subvec index should be integer"),
                    },
                    None => None,
                };

                let new_vec: Vec<Value> = if let Some(e) = end {
                    seq[(start as usize)..(e.clone() as usize)].to_vec()
                } else {
                    seq[start as usize..].to_vec()
                };
                Ok(vector_from_vec(new_vec))
            }
            _ => error("subvec index should be integer"),
        },
        _ => error("subvec called with non-vector"),
    }
}

fn apply(a: ExprArgs, env: Rc<Environment>) -> ValueRes {
    match a[a.len() - 1] {
        Value::List(ref v, _) | Value::Vector(ref v, _) => {
            let f = &a[0];
            let mut xargs = a[1..a.len() - 1].to_vec();
            xargs.extend_from_slice(&v);
            f.apply(xargs, env)
        }
        _ => error("apply called with non-seq"),
    }
}

fn map(a: ExprArgs, env: Rc<Environment>) -> ValueRes {
    match a[1] {
        Value::List(ref v, _) | Value::Vector(ref v, _) => {
            let f = &a[0];
            let mut ret = vec![];
            for v in v.iter() {
                ret.push(f.apply(vec![v.clone()], env.clone())?);
            }
            Ok(list_from_vec(ret))
        }
        Value::Str(_) => map(vec![a[0].clone(), a[1].to_chars_list().unwrap()], env),
        _ => error("map called with non-seq"),
    }
}

fn symbol(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::Str(ref s) => Ok(Value::Symbol(sym!(s))),
        Value::Symbol(_) => Ok(a[0].clone()),
        _ => error("Illegal symbol call"),
    }
}

fn get_name(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::Symbol(ref sym) => Ok(sym.name().to_value()),
        _ => error("get-name called with non-symbol"),
    }
}

fn assoc(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::HashMap(ref hm, ref meta) => {
            _assoc((**hm).clone(), (*meta).clone(), a[1..].to_vec())
        }
        _ => error("Cannot use assoc on non-HashMap"),
    }
}

fn dissoc(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::HashMap(ref hm, ref meta) => {
            _dissoc((**hm).clone(), (*meta).clone(), a[1..].to_vec())
        }
        _ => error("Cannot use dissoc on non-HashMap"),
    }
}

fn get(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    let key_arg = a[1].clone();
    match (a[0].clone(), key_arg.clone()) {
        (Value::Nil, _) => Ok(Value::Nil),
        (Value::HashMap(ref hm, _), Value::Str(_)) => match hm.get(&key_arg) {
            Some(v) => Ok(v.clone()),
            None => Ok(Value::Nil),
        },
        (Value::HashMap(ref hm, _), Value::Keyword(_)) => match hm.get(&key_arg) {
            Some(v) => Ok(v.clone()),
            None => Ok(Value::Nil),
        },
        _ => error("illegal get args"),
    }
}

fn contains_q(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    let key_arg = a[1].clone();
    match (a[0].clone(), key_arg.clone()) {
        (Value::HashMap(ref hm, _), Value::Str(_))
        | (Value::HashMap(ref hm, _), Value::Keyword(_)) => {
            Ok(Value::Bool(hm.contains_key(&key_arg)))
        }
        _ => error("illegal contains? args"),
    }
}

fn keys(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::HashMap(ref h, _) => Ok(list_from_vec((**h).keys().map(|a| a.clone()).collect())),
        _ => error("keys requires Hash Map"),
    }
}

fn vals(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::HashMap(ref h, _) => Ok(list_from_vec((**h).values().map(|v| v.clone()).collect())),
        _ => error("keys requires Hash Map"),
    }
}

fn readline(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::Str(ref s) => {
            let mut input = String::new();
            print!("{}", s.to_string());
            let _ = stdout().flush();
            stdin().read_line(&mut input).expect("Invalid string");

            if input.ends_with('\n') {
                input.pop();
                if input.ends_with('\r') {
                    input.pop();
                }
            }
            Ok(Value::Str(input.clone()))
        }
        _ => error("readline expects a string"),
    }
}

fn time_ms(_a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    let ms_e = match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(d) => d,
        Err(e) => return error(&format!("{:?}", e)),
    };
    Ok(Value::Integer(
        ms_e.as_secs() as i64 * 1000 + ms_e.subsec_nanos() as i64 / 1_000_000,
    ))
}

fn conj(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::List(ref l, _) => {
            let args = a[1..]
                .iter()
                .rev()
                .map(|a| a.clone())
                .collect::<Vec<Value>>();
            Ok(list_from_vec([&args[..], l].concat()))
        }
        Value::Vector(ref v, _) => Ok(vector_from_vec([v, &a[1..]].concat())),
        _ => error("conj expects a sequence"),
    }
}

fn keyword(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::Str(ref s) => Ok(values::keyword(&(*s))),
        Value::Symbol(ref s) => Ok(values::keyword(&(*s.name()))),
        Value::Keyword(ref k) => Ok(Value::Keyword(k.clone())),
        _ => error(&format!(
            "keyword not supported on this type: {}",
            a[0].as_str()
        )),
    }
}

fn seq(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        Value::List(ref v, _) | Value::Vector(ref v, _) if v.len() == 0 => Ok(Value::Nil),
        Value::List(ref v, _) | Value::Vector(ref v, _) => Ok(list_from_vec(v.to_vec())),
        Value::Str(ref s) if s.len() == 0 => Ok(Value::Nil),
        Value::Str(ref s) => Ok(list_from_vec(
            s.chars().map(|c| Value::Char(c as char)).collect(),
        )),
        Value::Nil => Ok(Value::Nil),
        _ => error("seq called with non-seq"),
    }
}

fn set(a: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    match a[0] {
        // Calling set on a set, just clear out the meta
        Value::Set(ref s, _) => Ok(Value::Set(s.clone(), None)),
        Value::List(ref v, _) | Value::Vector(ref v, _) => {
            let cloned_vec: Vec<Value> = v.iter().cloned().collect();
            Ok(set_from_vec(cloned_vec))
        }
        Value::HashMap(ref hm, _) => {
            let mut m = Vec::default();
            for (k, v) in hm.iter() {
                m.push(vector![k.clone(), v.clone()]);
            }
            Ok(set_from_vec(m.clone()))
        }
        Value::Str(ref s) => Ok(set_from_vec(s.chars().map(Value::Char).collect())),
        _ => error("set called with non-seq"),
    }
}

fn meta(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to meta. Expecting 1");
    }
    match args[0].get_meta() {
        Some(meta) => Ok(meta),
        None => Ok(Value::Nil),
    }
}

fn with_meta(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error("Wrong number of arguments passed to with-meta. Expecting 2");
    }
    let new_meta = args[1].clone();
    args[0].clone().with_meta(&new_meta)
}

fn equiv(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error("Wrong number of arguments passed to equiv. Expecting 2");
    }
    Ok(Value::Bool(args[0] == args[1]))
}

fn throw(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to throw. Expecting 1");
    }
    Err(ErrValue(Box::new(args[0].clone())))
}

fn error_fn(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to error. Expecting 1");
    }
    match args[0] {
        Value::Str(_) => {
            let e = errors::Error::new(args[0].clone()).to_lisp_error();
            Ok(Value::Error(e))
        }
        _ => error!("error requires string as first parameter"),
    }
}

fn empty_q(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to empty?. Expecting 1");
    }
    args[0].empty_q()
}

fn type_fn(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to type. Expecting 1");
    }
    Ok(Value::Symbol(sym!(args[0].as_str())))
}

fn count(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to count. Expecting 1");
    }
    args[0].count()
}

fn swap_bang(args: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if args.len() < 2 {
        return error("Wrong number of arguments passed to swap!. Expecting at least 2");
    }
    args[0].swap_bang(&args[1..].to_vec(), env)
}

fn reset_bang(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error("Wrong number of arguments passed to reset!. Expecting 2");
    }
    args[0].reset_bang(&args[1])
}

fn deref(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to deref. Expecting 1");
    }
    args[0].deref()
}

fn atom(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to atom. Expecting 1");
    }
    Ok(values::atom(&args[0]))
}

fn is_q(args: ExprArgs, _env: Rc<Environment>) -> ValueRes {
    if args.len() != 2 {
        return error("Wrong number of arguments passed to is?. Expecting 2");
    }
    let is = match args[0] {
        Value::Symbol(ref sym) => match args[1] {
            Value::Nil => sym.name == "Nil",
            Value::Integer(_) => sym.name == "Integer",
            Value::Float(_) => sym.name == "Float",
            Value::Bool(_) => sym.name == "Boolean",
            Value::Char(_) => sym.name == "Character",
            Value::Str(_) => sym.name == "String",
            Value::Symbol(_) => sym.name == "Symbol",
            Value::Keyword(_) => sym.name == "Keyword",
            Value::Var(_) => sym.name == "Var",
            Value::List(_, _) => sym.name == "List",
            Value::Vector(_, _) => sym.name == "Vector",
            Value::HashMap(_, _) => sym.name == "HashMap",
            Value::Set(_, _) => sym.name == "Set",
            Value::Func(_, _) => sym.name == "Function",
            Value::Macro(_, _) => sym.name == "Function",
            Value::Lambda { .. } => sym.name == "Function",
            Value::Namespace(_) => sym.name == "Namespace",
            Value::Atom(_) => sym.name == "Atom",
            Value::Error(_) => sym.name == "Error",
        },
        _ => return error!("is? requires symbol as first parameter"),
    };
    Ok(Value::Bool(is))
}

fn public_q(args: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to is-public. Expecting 1");
    }
    match args[0] {
        Value::Symbol(ref sym) => {
            let var = env.get(sym);
            Ok(Value::Bool(!var.is_private()))
        }
        _ => error!("public? requires symbol"),
    }
}

fn private_q(args: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to is-private. Expecting 1");
    }
    match args[0] {
        Value::Symbol(ref sym) => {
            let var = env.get(sym);
            Ok(Value::Bool(var.is_private()))
        }
        _ => error!("private? requires symbol"),
    }
}

/// Finds or creates a var named by the symbol name in the namespace
/// ns (which can be a symbol or a namespace), setting its root binding
/// to val if supplied. The namespace must exist. The var will adopt any
/// metadata from the name symbol.  Returns the var.
fn intern(args: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if args.len() < 2 || args.len() > 3 {
        return error("Wrong number of arguments passed to intern. Expecting 2 or 3");
    }

    match (args[0].clone(), args[1].clone()) {
        (Value::Symbol(ref ns), Value::Symbol(ref name)) => {
            let sym = Symbol::new_with_ns(&name.name, Some(&ns.name));
            let val = if let Some(v) = args.get(2) {
                v.clone()
            } else {
                Value::Nil
            };

            let v = (*env.get(&sym)).clone();
            let var = match v {
                Value::Var(_) => v,
                _ => {
                    let new_var = env.insert_var(sym.unqualified(), Rc::new(val));
                    (*new_var).clone()
                }
            };
            Ok(var)
        }
        _ => error!("intern requires a symbol for namespace and a symbol for var name"),
    }
}

fn print_debug(args: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to print-debug. Expecting 1");
    }

    match args[0] {
        Value::Symbol(ref sym) => {
            env.print_debug(sym);
            Ok(Value::Nil)
        }
        _ => error!("print-debug requires a symbol"),
    }
}

fn load_file_fn(args: ExprArgs, env: Rc<Environment>) -> ValueRes {
    if args.len() != 1 {
        return error("Wrong number of arguments passed to load-file. Expecting 1");
    }

    match args[0] {
        // Read file
        Value::Str(ref str) => {
            let _ = load_file(str, env)?;
            Ok(Value::Nil)
        }
        _ => error!("load-file requires a string"),
    }
}

/// Returns a vector of string/values.
pub fn internal_symbols(env: &Environment) -> Vec<(&'static str, Value)> {
    vec![("*ns*", {
        // Contains the current Namespace object
        let ns_rc = env.get_current_namespace().unwrap();
        Value::Namespace(RefCell::new(ns_rc))
    })]
}

/// Returns a vector of functions/macros defined in Rust, used as part of the
/// core namespace. They should be available `fmlisp.core`.
///
/// An internal macro, is just a function that receives the arguments not
/// previously evaluated.
pub fn core_functions() -> Vec<(&'static str, Value)> {
    use crate::values::Value::{Bool, Integer, Nil, Str};
    use crate::values::{pr_seq, pr_seq_readability};

    vec![
        // MATH
        ("+", func(add)),
        ("-", func(sub)),
        ("*", func(mul)),
        ("/", func(div)),
        ("inc", func(fn_t_num!(Integer, |i| { i + 1 }))),
        ("dec", func(fn_t_num!(Integer, |i| { i - 1 }))),
        // COMPARISONS
        ("=", func(equiv)),
        ("<", func(fn_t_num_num!(Bool, |i, j| { i < j }))),
        ("<=", func(fn_t_num_num!(Bool, |i, j| { i <= j }))),
        (">", func(fn_t_num_num!(Bool, |i, j| { i > j }))),
        (">=", func(fn_t_num_num!(Bool, |i, j| { i >= j }))),
        // PRINT
        (
            "prn",
            func(|a, _| {
                println!("{}", pr_seq_readability(&a, "", "", " "));
                Ok(Nil)
            }),
        ),
        (
            "println",
            func(|a, _| {
                println!("{}", pr_seq(&a, "", "", " "));
                Ok(Nil)
            }),
        ),
        (
            "pr-str",
            func(|a, _| Ok(Str(pr_seq_readability(&a, "", "", " ")))),
        ),
        // STRING
        ("str", func(|a, _| Ok(Value::Str(pr_seq(&a, "", "", ""))))),
        ("string?", func(fn_is_type!(Value::Str(_)))),
        ("char?", func(fn_is_type!(Value::Char(_)))),
        ("list", func(|a, _| Ok(list_from_vec(a)))),
        ("list?", func(fn_is_type!(Value::List(_, _)))),
        ("empty?", func(empty_q)),
        ("count", func(count)),
        ("read-string", func(read_string)),
        ("readline", func(readline)),
        ("slurp", func(slurp)),
        // ATOM
        ("atom", func(atom)),
        ("atom?", func(fn_is_type!(Value::Atom(_)))),
        ("deref", func(deref)),
        ("reset!", func(reset_bang)),
        ("swap!", func(swap_bang)),
        // COLLECTIONS
        (
            "sequential?",
            func(fn_is_type!(Value::List(_, _), Value::Vector(_, _))),
        ),
        ("vector", func(|a, _| Ok(vector_from_vec(a)))),
        ("vector?", func(fn_is_type!(Value::Vector(_, _)))),
        ("cons", func(cons)),
        ("concat", func(concat)),
        ("conj", func(conj)),
        ("vec", func(vec)),
        ("nth", func(nth)),
        ("first", func(first)),
        ("second", func(second)),
        ("rest", func(rest)),
        ("next", func(next)),
        ("last", func(last)),
        ("butlast", func(butlast)),
        ("subvec", func(subvec)),
        ("apply", func(apply)),
        ("map", func(map)),
        ("nil?", func(fn_is_type!(Value::Nil))),
        ("true?", func(fn_is_type!(Value::Bool(true)))),
        ("false?", func(fn_is_type!(Value::Bool(false)))),
        // Symbols
        ("symbol?", func(fn_is_type!(Value::Symbol(_)))),
        ("symbol", func(symbol)),
        ("get-name", func(get_name)),
        // Keyword
        ("keyword", func(keyword)),
        ("keyword?", func(fn_is_type!(Value::Keyword(_)))),
        // Set
        ("set", func(set)),
        ("set?", func(fn_is_type!(Value::Set(_, _)))),
        // HASHMAP
        ("hash-map", func(|a, _| hash_map_from_vec(a))),
        ("map?", func(fn_is_type!(Value::HashMap(_, _)))),
        ("assoc", func(assoc)),
        ("dissoc", func(dissoc)),
        ("get", func(get)),
        ("contains?", func(contains_q)),
        ("keys", func(keys)),
        ("vals", func(vals)),
        ("number?", func(fn_is_type!(Value::Integer(_)))),
        (
            "fn?",
            func(fn_is_type!(Value::Lambda{is_macro,..} if !is_macro, Value::Func(_,_))),
        ),
        (
            "macro?",
            func(fn_is_type!(Value::Lambda{is_macro,..} if is_macro)),
        ),
        ("time-ms", func(time_ms)),
        ("seq", func(seq)),
        ("meta", func(meta)),
        ("with-meta", func(with_meta)),
        // GENERICS
        ("type", func(type_fn)),
        ("is?", func(is_q)),
        ("public?", macro_fn(public_q)),
        ("private?", macro_fn(private_q)),
        ("intern", func(intern)),
        ("print-debug", macro_fn(print_debug)),
        ("load-file", func(load_file_fn)),
        // ERROR HANDLING
        ("throw", func(throw)),
        ("error", func(error_fn)),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::env::Environment;
    use crate::values::{self, string};
    use std::collections::HashSet;

    // cons

    #[test]
    fn test_cons() {
        let env = Environment::default().to_rc();

        let args = vec![
            Value::Integer(1),
            list![Value::Integer(2), Value::Integer(3)],
        ];
        let expected_lst = list![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        assert_eq!(cons(args, env.clone()).unwrap(), expected_lst); // prepend elements

        let args = vec![
            Value::Integer(1),
            vector![Value::Integer(2), Value::Integer(3)],
        ];
        let expected_lst = list![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        assert_eq!(cons(args, env.clone()).unwrap(), expected_lst); // return always list

        let args = vec![string("str"), vector![Value::Integer(2), Value::Integer(3)]];
        let expected_lst = list![string("str"), Value::Integer(2), Value::Integer(3)];
        assert_eq!(cons(args, env).unwrap(), expected_lst); // accepts mixed data type
    }

    // concat

    #[test]
    fn test_concat() {
        let env = Environment::default().to_rc();

        let args = vec![
            list![Value::Integer(1), Value::Integer(2)],
            list![Value::Integer(3), Value::Integer(4)],
        ];
        let expected_lst = list![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4)
        ];
        assert_eq!(concat(args, env.clone()).unwrap(), expected_lst); // concat collections

        let args = vec![
            list![Value::Integer(1), Value::Integer(2)],
            vector![Value::Integer(3), Value::Integer(4)],
        ];
        let expected_lst = list![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4)
        ];
        assert_eq!(concat(args, env).unwrap(), expected_lst); // returns always list
    }

    #[test]
    #[should_panic(expected = "non-coll passed to concat")]
    fn test_concat_with_errors() {
        let env = Environment::default().to_rc();

        let args = vec![
            Value::Integer(1),
            list![Value::Integer(3), Value::Integer(4)],
        ];
        concat(args, env).unwrap();
    }

    // vec

    #[test]
    fn test_vec() {
        let env = Environment::default().to_rc();

        let args = vec![list![Value::Integer(1), Value::Integer(2)]];
        let expected_lst = vector![Value::Integer(1), Value::Integer(2)];
        assert_eq!(vec(args, env.clone()).unwrap(), expected_lst); // list to vec

        let args = vec![vector![Value::Integer(3), Value::Integer(4)]];
        let expected_vec = vector![Value::Integer(3), Value::Integer(4)];
        assert_eq!(vec(args, env.clone()).unwrap(), expected_vec); // works for all colls

        let args = vec![string("test")];
        let expected_str = vector![
            Value::Char('t'),
            Value::Char('e'),
            Value::Char('s'),
            Value::Char('t')
        ];
        assert_eq!(vec(args, env).unwrap(), expected_str); // works for strings
    }

    #[test]
    #[should_panic]
    fn test_vec_panic_arg_type() {
        let env = Environment::default().to_rc();
        let args = vec![Value::Integer(1)];
        vec(args, env).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_vec_panic_arg_length() {
        let env = Environment::default().to_rc();
        let args = vec![Value::Integer(1), Value::Integer(2)];
        vec(args, env).unwrap();
    }

    // nth

    #[test]
    fn test_nth() {
        let env = Environment::default().to_rc();

        let args = vec![
            list![Value::Integer(1), Value::Integer(2), Value::Integer(1)],
            Value::Integer(1),
        ];
        let expected_val = Value::Integer(2);
        assert_eq!(nth(args, env.clone()).unwrap(), expected_val); // works with lists

        let args = vec![
            vector![string("1"), string("2"), string("3")],
            Value::Integer(1),
        ];
        let expected_val = string("2");
        assert_eq!(nth(args, env.clone()).unwrap(), expected_val); // works with vectors and string values

        let args = vec![string("hello"), Value::Integer(1)];
        let expected_char = Value::Char('e');
        assert_eq!(nth(args, env).unwrap(), expected_char); // works with strings
    }

    #[test]
    #[should_panic]
    fn test_nth_panic_arg_type() {
        let env = Environment::default().to_rc();
        let args = vec![Value::Integer(2), Value::Integer(1)];
        nth(args, env).unwrap();
    }

    // first

    #[test]
    fn test_first() {
        let env = Environment::default().to_rc();

        let args = vec![list![]];
        assert_eq!(first(args, env.clone()).unwrap(), Value::Nil);

        let args = vec![vector![]];
        assert_eq!(first(args, env.clone()).unwrap(), Value::Nil);

        let args = vec![list![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3)
        ]];
        assert_eq!(first(args, env.clone()).unwrap(), Value::Integer(1));

        let args = vec![vector![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3)
        ]];
        assert_eq!(first(args, env.clone()).unwrap(), Value::Integer(1));

        let args = vec![string("string")];
        assert_eq!(first(args, env.clone()).unwrap(), Value::Char('s'));

        let args = vec![Value::Nil];
        assert_eq!(first(args, env).unwrap(), Value::Nil);
    }

    #[test]
    #[should_panic]
    fn test_first_panic() {
        let env = Environment::default().to_rc();
        let args = vec![Value::Integer(1)];
        first(args, env).unwrap();
    }

    // rest

    #[test]
    fn test_rest() {
        let env = Environment::default().to_rc();

        let args = vec![list![]];
        assert_eq!(rest(args, env.clone()).unwrap(), list![]);

        let args = vec![vector![]];
        assert_eq!(rest(args, env.clone()).unwrap(), list![]);

        let args = vec![list![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3)
        ]];
        assert_eq!(
            rest(args, env.clone()).unwrap(),
            list![Value::Integer(2), Value::Integer(3)]
        );

        let args = vec![vector![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3)
        ]];
        assert_eq!(
            rest(args, env.clone()).unwrap(),
            list![Value::Integer(2), Value::Integer(3)]
        );

        let args = vec![Value::Nil];
        assert_eq!(rest(args, env.clone()).unwrap(), list![]);
    }

    #[test]
    #[should_panic]
    fn test_rest_panic() {
        let env = Environment::default().to_rc();
        let args = vec![Value::Integer(1)];
        rest(args, env).unwrap();
    }

    // apply

    #[test]
    fn test_apply_working() {
        let env = Environment::default().to_rc();
        let sum_fn = func(|args, _| {
            let sum: i64 = args
                .iter()
                .filter_map(|val| {
                    // Match on the Value to extract the integer
                    match val {
                        Value::Integer(i) => Some(*i),
                        _ => None,
                    }
                })
                .sum();
            Ok(Value::Integer(sum))
        });

        let numbers = list![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        let result = apply(vec![sum_fn, numbers], env);
        assert_eq!(result.unwrap(), Value::Integer(6));
    }

    #[test]
    #[should_panic(expected = "apply called with non-seq")]
    fn test_apply_panic_non_seq() {
        let env = Environment::default().to_rc();
        let non_seq_arg = Value::Integer(42);
        apply(vec![func(|_args, _| Ok(Value::Nil)), non_seq_arg], env).unwrap();
    }

    #[test]
    fn test_apply_empty_seq() {
        let env = Environment::default().to_rc();
        let empty_list = list![];
        let result = apply(
            vec![Value::Func(|_args, _| Ok(Value::Nil), None), empty_list],
            env,
        );
        assert_eq!(result.unwrap(), Value::Nil);
    }

    #[test]
    fn test_apply_single_value() {
        let env = Environment::default().to_rc();
        let single_value_seq = list![Value::Integer(42)];
        let result = apply(vec![func(|_args, _| Ok(Value::Nil)), single_value_seq], env);
        assert_eq!(result.unwrap(), Value::Nil);
    }

    // map

    #[test]
    fn test_map_with_list() {
        let env = Environment::default().to_rc();
        let square_fn = func(|args, _| {
            let value = &args[0];
            match value {
                Value::Integer(i) => Ok(Value::Integer(i * i)),
                _ => error("Expected an integer"),
            }
        });

        let numbers = list![Value::Integer(1), Value::Integer(2), Value::Integer(3)];

        let result = map(vec![square_fn, numbers], env).unwrap();
        let expected = list![Value::Integer(1), Value::Integer(4), Value::Integer(9)];
        assert_eq!(result, expected);
    }

    #[test]
    fn test_map_with_vector() {
        let env = Environment::default().to_rc();
        let square_fn = func(|args, _| {
            let value = &args[0];
            match value {
                Value::Integer(i) => Ok(Value::Integer(i * i)),
                _ => error("Expected an integer"),
            }
        });

        let numbers = vector![Value::Integer(1), Value::Integer(2), Value::Integer(3)];

        let result = map(vec![square_fn, numbers], env).unwrap();
        let expected = list![Value::Integer(1), Value::Integer(4), Value::Integer(9)];
        assert_eq!(result, expected);
    }

    #[test]
    fn test_map_with_string() {
        let env = Environment::default().to_rc();

        let uppercase_fn = func(|args, _| {
            let value = &args[0];
            match value {
                Value::Char(c) => Ok(Value::Char(c.to_ascii_uppercase())),
                _ => error(&format!("Expected a character, received {:?}", value)),
            }
        });

        let string = string("hello");
        let result = map(vec![uppercase_fn, string], env).unwrap();
        let expected = list![
            Value::Char('H'),
            Value::Char('E'),
            Value::Char('L'),
            Value::Char('L'),
            Value::Char('O')
        ];
        assert_eq!(result, expected);
    }

    #[test]
    #[should_panic]
    fn test_map_with_non_seq() {
        let env = Environment::default().to_rc();
        // Test the function with a non-sequence argument
        let non_seq_arg = Value::Integer(42);
        map(vec![func(|_args, _| Ok(Value::Nil)), non_seq_arg], env).unwrap();
    }

    // keyword

    #[test]
    fn test_keyword() {
        let env = Environment::default().to_rc();

        let args = vec![string("test")];
        assert_eq!(keyword(args, env.clone()).unwrap(), values::keyword("test"));

        let args = vec![values::symbol("test")];
        assert_eq!(keyword(args, env.clone()).unwrap(), values::keyword("test"));

        let args = vec![values::keyword("test")];
        assert_eq!(keyword(args, env.clone()).unwrap(), values::keyword("test"));

        let args = vec![string(":test")];
        assert_eq!(
            keyword(args, env.clone()).unwrap(),
            values::keyword(":test")
        );
    }

    #[test]
    #[should_panic(expected = "keyword not supported on this type")]
    fn test_keyword_panic_other_types() {
        let env = Environment::default().to_rc();
        let unsupported = vec![Value::Integer(42)];
        keyword(unsupported, env).unwrap();
    }

    // symbol

    #[test]
    fn test_symbol() {
        let env = Environment::default().to_rc();

        let args = vec![string("test")];
        assert_eq!(
            symbol(args, env.clone()).unwrap(),
            Value::Symbol(sym!("test"))
        );

        let args = vec![Value::Symbol(sym!("test"))];
        assert_eq!(
            symbol(args, env.clone()).unwrap(),
            Value::Symbol(sym!("test"))
        );
    }

    #[test]
    #[should_panic]
    fn test_symbol_panic() {
        let env = Environment::default().to_rc();
        let args = vec![Value::Integer(1)];
        symbol(args, env).unwrap();
    }

    // assoc

    #[test]
    fn test_assoc() {
        let env = Environment::default().to_rc();

        let hm = hash_map_from_vec(vec![
            values::keyword("a"),
            Value::Integer(1),
            values::keyword("b"),
            Value::Integer(2),
        ])
        .unwrap();
        let args = vec![hm, values::keyword("c"), Value::Integer(3)];
        let expected_hm = hash_map_from_vec(vec![
            values::keyword("a"),
            Value::Integer(1),
            values::keyword("b"),
            Value::Integer(2),
            values::keyword("c"),
            Value::Integer(3),
        ])
        .unwrap();
        assert_eq!(assoc(args, env).unwrap(), expected_hm);
    }

    // dissoc

    #[test]
    fn test_dissoc() {
        let env = Environment::default().to_rc();
        let hm = hash_map_from_vec(vec![
            values::keyword("a"),
            Value::Integer(1),
            values::keyword("b"),
            Value::Integer(2),
        ])
        .unwrap();
        let args = vec![hm, values::keyword("b")];
        let expected_hm = hash_map_from_vec(vec![values::keyword("a"), Value::Integer(1)]).unwrap();
        assert_eq!(dissoc(args, env).unwrap(), expected_hm);
    }

    // assoc

    #[test]
    fn test_get() {
        let env = Environment::default().to_rc();
        let hm = hash_map_from_vec(vec![
            values::keyword("a"),
            Value::Integer(1),
            values::keyword("b"),
            Value::Integer(2),
        ])
        .unwrap();
        let args = vec![hm.clone(), values::keyword("b")];
        assert_eq!(get(args, env.clone()).unwrap(), Value::Integer(2));
        let args = vec![hm.clone(), values::keyword("c")];
        assert_eq!(get(args, env.clone()).unwrap(), Value::Nil);
    }

    // contains

    #[test]
    fn test_contains_q() {
        let env = Environment::default().to_rc();
        let hm = hash_map_from_vec(vec![
            values::keyword("a"),
            Value::Integer(1),
            values::keyword("b"),
            Value::Integer(2),
        ])
        .unwrap();
        let args = vec![hm.clone(), values::keyword("b")];
        assert_eq!(contains_q(args, env.clone()).unwrap(), Value::Bool(true));
        let args = vec![hm.clone(), values::keyword("c")];
        assert_eq!(contains_q(args, env.clone()).unwrap(), Value::Bool(false));
    }

    // keys

    #[test]
    fn test_keys() {
        let env = Environment::default().to_rc();
        let hm = hash_map_from_vec(vec![
            values::keyword("a"),
            Value::Integer(1),
            values::keyword("b"),
            Value::Integer(2),
        ])
        .unwrap();
        let args = vec![hm.clone()];
        let expected = set![values::keyword("a"), values::keyword("b")];
        let result = keys(args, env).unwrap();

        if let Value::List(l, _) = result {
            assert_eq!(values::set_from_vec(l.to_vec()), expected);
        } else {
            panic!("key didn't returned a list");
        }
    }

    // vals

    #[test]
    fn test_vals() {
        let env = Environment::default().to_rc();
        let hm = hash_map_from_vec(vec![
            values::keyword("a"),
            Value::Integer(1),
            values::keyword("b"),
            Value::Integer(2),
        ])
        .unwrap();
        let args = vec![hm.clone()];
        let expected = set![Value::Integer(1), Value::Integer(2)];
        let result = vals(args, env).unwrap();

        if let Value::List(l, _) = result {
            assert_eq!(values::set_from_vec(l.to_vec()), expected);
        } else {
            panic!("key didn't returned a list");
        }
    }

    // conj

    #[test]
    fn test_conj() {
        let env = Environment::default().to_rc();

        let args = vec![
            list![Value::Integer(1), Value::Integer(2)],
            Value::Integer(3),
        ];
        let expected = list![Value::Integer(3), Value::Integer(1), Value::Integer(2)];
        assert_eq!(conj(args, env.clone()).unwrap(), expected);

        let args = vec![
            vector![Value::Integer(1), Value::Integer(2)],
            Value::Integer(3),
        ];
        let expected = vector![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
        assert_eq!(conj(args, env.clone()).unwrap(), expected);

        let args = vec![vector![Value::Integer(1), Value::Integer(2)]];
        let expected = vector![Value::Integer(1), Value::Integer(2)];
        assert_eq!(conj(args, env.clone()).unwrap(), expected);
    }

    // seq

    #[test]
    fn test_seq() {
        let env = Environment::default().to_rc();

        let args = vec![list![]];
        assert_eq!(seq(args, env.clone()).unwrap(), Value::Nil);

        let args = vec![vector![]];
        assert_eq!(seq(args, env.clone()).unwrap(), Value::Nil);

        let args = vec![string("")];
        assert_eq!(seq(args, env.clone()).unwrap(), Value::Nil);

        let args = vec![Value::Nil];
        assert_eq!(seq(args, env.clone()).unwrap(), Value::Nil);

        let args = vec![list![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3)
        ]];
        assert_eq!(
            seq(args, env.clone()).unwrap(),
            list![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
        );

        let args = vec![vector![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3)
        ]];
        assert_eq!(
            seq(args, env.clone()).unwrap(),
            list![Value::Integer(1), Value::Integer(2), Value::Integer(3)]
        );

        let args = vec![string("hello")];
        assert_eq!(
            seq(args, env.clone()).unwrap(),
            list![
                Value::Char('h'),
                Value::Char('e'),
                Value::Char('l'),
                Value::Char('l'),
                Value::Char('o')
            ]
        );
    }

    #[test]
    #[should_panic]
    fn test_seq_non_seq() {
        let env = Environment::default().to_rc();
        let non_seq = Value::Integer(42);
        seq(vec![non_seq.clone()], env).unwrap();
    }
}
