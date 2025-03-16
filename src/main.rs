extern crate dotenvy;
extern crate fnv;
extern crate formatx;
extern crate if_chain;
extern crate itertools;
extern crate regex;
extern crate rustyline;
extern crate termion;

use fmlisp::values::{format_error, list_from_vec, ToValue, Value};
use fmlisp::repl::Repl;
use dotenvy::dotenv;

fn main() {
    // Loading .env
    match dotenv() {
        Ok(_) => {}
        Err(_) => println!(".env file not found"),
    }

    let repl = match Repl::default() {
        Ok(r) => r,
        Err(_) => return,
    };

    let mut args = std::env::args();
    let arg1 = args.nth(1);
    let argv: Vec<Value> = args.map(Value::Str).collect();
    repl.insert_str("*command-line-args*", list_from_vec(argv).to_rc_value());

    // Command line args load-eval-exit
    if let Some(f) = arg1 {
        match repl.rep(&format!("(load-file \"{}\")", f)) {
            Ok(_) => std::process::exit(0),
            Err(e) => {
                println!("Error: {}", format_error(e));
                std::process::exit(1);
            }
        }
    }

    repl.run();
}
