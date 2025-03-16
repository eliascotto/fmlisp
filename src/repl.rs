extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::rc::Rc;

use crate::core;
use crate::env::Environment;
use crate::error_output;
use crate::symbol::Symbol;
use crate::values::LispErr;
use crate::values::Value;

pub struct Repl {
    environment: Rc<Environment>,
}

impl Repl {
    pub fn new() -> Repl {
        Repl {
            environment: Rc::new(Environment::default()),
        }
    }

    /// Creates a Repl object with all the standard definitions loaded.
    pub fn default() -> Result<Repl, LispErr> {
        let repl = Repl::new();

        // Load language core
        match core::load_lang_core(repl.environment.clone()) {
            Ok(_) => Ok(repl),
            Err(e) => Err(e),
        }
    }

    /// Insert a sym-val into the environment
    pub fn insert_str(&self, sym: &str, val: Rc<Value>) {
        self.environment.insert_str(sym, val)
    }

    /// Change or create a new namespace
    pub fn change_or_create_namespace(&self, sym: &Symbol) {
        self.environment.change_or_create_namespace(sym);
    }

    // Read Eval Print
    pub fn rep(&self, s: &str) -> Result<String, LispErr> {
        let ast: Value = core::read(s, self.environment.clone())?;
        let exp: Value = core::eval(ast.clone(), self.environment.clone())?;
        Ok(core::print(exp))
    }

    /// Run the REP loop
    pub fn run(&self) {
        let mut rl = DefaultEditor::new().unwrap();
        if rl.load_history("history").is_err() {}

        // Loop calling REP
        'rep: loop {
            let current_ns_name = self.environment.get_current_namespace_name();
            let readline = rl.readline(&format!("{}> ", current_ns_name));
            match readline {
                Ok(line) => {
                    if let Err(err) = rl.add_history_entry(line.as_str()) {
                        eprintln!("Error adding to history: {:?}", err);
                    }

                    if let Err(err) = rl.save_history("history") {
                        eprintln!("Error saving history: {:?}", err);
                    }

                    if line.len() > 0 {
                        match self.rep(&line) {
                            Ok(out) => println!("{}", out),
                            Err(e) => match error_output::eprint(e, "REPL") {
                                0 => continue 'rep,
                                -1 => return,
                                _ => {}
                            },
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => continue 'rep,
                Err(ReadlineError::Eof) => break 'rep,
                Err(err) => {
                    println!("Error: {:?}", err);
                    break 'rep;
                }
            }
        }
    }
}
