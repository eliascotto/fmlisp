use formatx::formatx;
use std::fs::read_to_string;
use std::rc::Rc;

use crate::core;
use crate::env::Environment;
use crate::reader;
use crate::symbol::Symbol;
use crate::values::{LispErr, ToValue, Value, ValueRes};

/// Returns the file content evaluated, or an error.
pub fn load_file(path: &String, env: Rc<Environment>) -> ValueRes {
    match read_to_string(path) {
        Ok(data) => {
            let mut r = reader::tokenize(&data, env.clone());
            let mut res = vec![];
            let mut last_val;

            // Loop for multi-form evaluation.
            // We eval each form sequentially to avoid using wrong namespace during syntax quote expansion.
            // So when we read a file with `(ns..)` at the beninning, all the following code expansions,
            // happening at reading time, will use the new namespace declared at the top of the file.
            // This unfortunately forces the declaration to be sequential (require/circular import/declare)
            // which is doesn't makes the language simple as declaring new methods in Rust.
            //
            // doing the following results in wrong namespace applied at `p1`:
            // (eval (read-string "(do
            //                       (ns foo.bar)
            //                       (defn p1 [c] (+ 1 c))
            //                       (println *ns*)
            //                       (defmacro x [v] `(p1 v))
            //                       (x 2))"))
            //
            // If we clone the same code in a file and load it instead, the code will be executed correctly.
            // This is because file-reading process form sequentially while `read-string` just read 1 form and
            // then eval it.
            //
            loop {
                let v = match reader::read_form(&mut r) {
                    Ok(s) => {
                        last_val = core::eval(s, env.clone())?;
                    }
                    Err(e) => return Err(e),
                };
                res.push(v);
                if r.peek().is_err() {
                    break;
                }
            }
            Ok(last_val)
        }
        Err(e) => error!(&e.to_string()),
    }
}

/// Load a module with a specific name into the environment
pub fn load_module(env: Rc<Environment>, mod_name: &str, defs: Vec<(&'static str, Value)>) {
    env.change_or_create_namespace(&sym!(mod_name));

    for (k, v) in defs {
        // Namespace mapping Symbol - Rc<Value>
        // Load them into clojure.core ns
        env.insert_var(Symbol::new(k), v.to_rc_value());
    }
}

pub fn format_runtime(s: &String, args: &Vec<String>) -> Result<String, LispErr> {
    let formatted = match args.len() {
        0 => formatx!(s),
        1 => formatx!(s, args[0].clone()),
        2 => formatx!(s, args[0].clone(), args[1].clone()),
        3 => formatx!(s, args[0].clone(), args[1].clone(), args[2].clone()),
        4 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone()
        ),
        5 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone()
        ),
        6 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone()
        ),
        7 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone()
        ),
        8 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone()
        ),
        9 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone()
        ),
        10 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone()
        ),
        11 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone()
        ),
        12 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone()
        ),
        13 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone()
        ),
        14 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone()
        ),
        15 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone()
        ),
        16 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone()
        ),
        17 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone()
        ),
        18 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone(),
            args[17].clone()
        ),
        19 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone(),
            args[17].clone(),
            args[18].clone()
        ),
        20 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone(),
            args[17].clone(),
            args[18].clone(),
            args[19].clone()
        ),
        21 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone(),
            args[17].clone(),
            args[18].clone(),
            args[19].clone(),
            args[20].clone()
        ),
        22 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone(),
            args[17].clone(),
            args[18].clone(),
            args[19].clone(),
            args[20].clone(),
            args[21].clone()
        ),
        23 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone(),
            args[17].clone(),
            args[18].clone(),
            args[19].clone(),
            args[20].clone(),
            args[21].clone(),
            args[22].clone()
        ),
        24 => formatx!(
            s,
            args[0].clone(),
            args[1].clone(),
            args[2].clone(),
            args[3].clone(),
            args[4].clone(),
            args[5].clone(),
            args[6].clone(),
            args[7].clone(),
            args[8].clone(),
            args[9].clone(),
            args[10].clone(),
            args[11].clone(),
            args[12].clone(),
            args[13].clone(),
            args[14].clone(),
            args[15].clone(),
            args[16].clone(),
            args[17].clone(),
            args[18].clone(),
            args[19].clone(),
            args[20].clone(),
            args[21].clone(),
            args[22].clone(),
            args[23].clone()
        ),
        _ => {
            return error_fmt!(
                "too many arguments ({}) provided to format. (Max args 24).",
                args.len()
            )
        }
    };
    match formatted {
        Ok(fmt) => Ok(fmt),
        Err(e) => error_fmt!("Error formatting string \"{}\": {}", s, e),
    }
}
