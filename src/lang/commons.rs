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
