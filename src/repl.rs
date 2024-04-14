extern crate rustyline;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::rc::Rc;

use crate::compiler::Compiler;
use crate::core;
use crate::env::Environment;
use crate::error_output;
use crate::symbol::Symbol;
use crate::values::LispErr;
use crate::values::Value;

pub struct Repl {
    environment: Rc<Environment>,
    compiler: Rc<Compiler>,
}

impl Repl {
    pub fn new() -> Repl {
        Repl {
            environment: Rc::new(Environment::default()),
            compiler: Rc::new(Compiler::new()),
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
        let exp: Value = self.compiler.eval(ast.clone(), self.environment.clone())?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::{thread, time::Duration};

    #[test]
    fn test_builtin_fn() {
        let repl = Repl::default().unwrap();

        assert_eq!(repl.rep("(symbol? 'abc)").unwrap(), "true");
        assert_eq!(repl.rep("(symbol? \"abc\")").unwrap(), "false");

        assert_eq!(repl.rep("(nil? nil)").unwrap(), "true");
        assert_eq!(repl.rep("(nil? true)").unwrap(), "false");

        assert_eq!(repl.rep("(true? true)").unwrap(), "true");
        assert_eq!(repl.rep("(true? false)").unwrap(), "false");
        assert_eq!(repl.rep("(true? true?)").unwrap(), "false");

        assert_eq!(repl.rep("(false? false)").unwrap(), "true");
        assert_eq!(repl.rep("(false? true)").unwrap(), "false");

        // Testing apply function with core functions

        assert_eq!(repl.rep("(apply + (list 2 3))").unwrap(), "5");
        assert_eq!(repl.rep("(apply + 4 (list 5))").unwrap(), "9");
        assert_eq!(
            repl.rep("(apply prn (list 1 2 \"3\" (list)))").unwrap(),
            "nil"
        );
        assert_eq!(
            repl.rep("(apply prn 1 2 (list \"3\" (list)))").unwrap(),
            "nil"
        );
        assert_eq!(repl.rep("(apply list (list))").unwrap(), "()");
        assert_eq!(
            repl.rep("(apply symbol? (list (quote two)))").unwrap(),
            "true"
        );

        // Testing apply function with user functions

        assert_eq!(
            repl.rep("(apply (fn [a b] (+ a b)) (list 2 3))").unwrap(),
            "5"
        );
        assert_eq!(
            repl.rep("(apply (fn [a b] (+ a b)) 4 (list 5))").unwrap(),
            "9"
        );

        // Testing map function

        repl.rep("(def nums (list 1 2 3))").unwrap();
        repl.rep("(def double (fn [a] (* 2 a)))").unwrap();

        assert_eq!(repl.rep("(map double nums)").unwrap(), "(2 4 6)");
        assert_eq!(
            repl.rep("(map (fn [x] (symbol? x)) (list 1 (quote two) \"three\"))")
                .unwrap(),
            "(false true false)"
        );
        assert_eq!(repl.rep("(= () (map str ()))").unwrap(), "true");
    }

    #[test]
    fn test_symbol_and_keyword_functions() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(symbol? :abc)").unwrap(), "false");
        assert_eq!(repl.rep("(symbol? 'abc)").unwrap(), "true");
        assert_eq!(repl.rep("(symbol? \"abc\")").unwrap(), "false");
        assert_eq!(repl.rep("(symbol? (symbol \"abc\"))").unwrap(), "true");
        assert_eq!(repl.rep("(keyword? :abc)").unwrap(), "true");
        assert_eq!(repl.rep("(keyword? 'abc)").unwrap(), "false");
        assert_eq!(repl.rep("(keyword? \"abc\")").unwrap(), "false");
        assert_eq!(repl.rep("(keyword? \"\")").unwrap(), "false");
        assert_eq!(repl.rep("(keyword? (keyword \"abc\"))").unwrap(), "true");
        assert_eq!(repl.rep("(symbol \"abc\")").unwrap(), "abc");
        assert_eq!(repl.rep("(keyword \"abc\")").unwrap(), ":abc");
    }

    #[test]
    fn test_sequential_function() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(sequential? (list 1 2 3))").unwrap(), "true");
        assert_eq!(repl.rep("(sequential? [15])").unwrap(), "true");
        assert_eq!(repl.rep("(sequential? sequential?)").unwrap(), "false");
        assert_eq!(repl.rep("(sequential? nil)").unwrap(), "false");
        assert_eq!(repl.rep("(sequential? \"abc\")").unwrap(), "false");
    }

    #[test]
    fn test_apply_function_core_functions_args_in_vector() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(apply + 4 [5])").unwrap(), "9");
        assert_eq!(repl.rep("(apply prn 1 2 [\"3\" 4])").unwrap(), "nil");
        assert_eq!(repl.rep("(apply list [])").unwrap(), "()");
    }

    #[test]
    fn test_apply_function_user_functions_args_in_vector() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(apply (fn [a b] (+ a b)) [2 3])").unwrap(), "5");
        assert_eq!(repl.rep("(apply (fn [a b] (+ a b)) 4 [5])").unwrap(), "9");
    }

    #[test]
    fn test_throw() {
        let repl = Repl::default().unwrap();
        assert!(repl.rep("(throw \"err1\")").is_err());
    }

    #[test]
    fn test_dissoc_repl() {
        let repl = Repl::default().unwrap();
        repl.rep("(def hm1 (hash-map))").unwrap();
        repl.rep("(def hm2 (assoc hm1 \"a\" 1))").unwrap();
        repl.rep("(def hm3 (assoc hm2 \"b\" 2))").unwrap();
        assert_eq!(repl.rep("(count (keys (dissoc hm2 \"a\")))").unwrap(), "0");
        assert_eq!(repl.rep("(count (vals (dissoc hm2 \"a\")))").unwrap(), "0");
        assert_eq!(repl.rep("(dissoc hm3 \"a\")").unwrap(), "{\"b\" 2}");
        assert_eq!(repl.rep("(dissoc hm3 \"a\" \"b\")").unwrap(), "{}");
        assert_eq!(repl.rep("(dissoc hm3 \"a\" \"b\" \"c\")").unwrap(), "{}");
        assert_eq!(repl.rep("(count (keys hm3))").unwrap(), "2");

        assert_eq!(repl.rep("(def f {:a 1 :b 2})").unwrap(), "#'user/f");
        assert_eq!(repl.rep("(dissoc f :a)").unwrap(), "{:b 2}");
        assert_eq!(
            repl.rep("(dissoc {:cde 345 :fgh 456} :cde)").unwrap(),
            "{:fgh 456}"
        );
        assert_eq!(
            repl.rep("(dissoc {:cde nil :fgh 456} :cde)").unwrap(),
            "{:fgh 456}"
        );
        assert_eq!(
            repl.rep("(dissoc {:cde 123 :fgh nil} :cde)").unwrap(),
            "{:fgh nil}"
        );
    }

    #[test]
    fn test_equality_of_hash_maps() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(= {} {})").unwrap(), "true");
        assert_eq!(repl.rep("(= {} (hash-map))").unwrap(), "true");
        assert_eq!(
            repl.rep("(= {:a 11 :b 22} (hash-map :b 22 :a 11))")
                .unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(= {:a 11 :b [22 33]} (hash-map :b [22 33] :a 11))")
                .unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(= {:a 11 :b {:c 33}} (hash-map :b {:c 33} :a 11))")
                .unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(= {:a 11 :b 22} (hash-map :b 23 :a 11))")
                .unwrap(),
            "false"
        );
        assert_eq!(
            repl.rep("(= {:a 11 :b 22} (hash-map :a 11))").unwrap(),
            "false"
        );
        assert_eq!(
            repl.rep("(= {:a [11 22]} {:a (list 11 22)})").unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(= {:a 11 :b 22} (list :a 11 :b 22))").unwrap(),
            "false"
        );
        assert_eq!(repl.rep("(= {} [])").unwrap(), "false");
        assert_eq!(repl.rep("(= [] {})").unwrap(), "false");

        assert_eq!(repl.rep("(keyword :abc)").unwrap(), ":abc");
        assert_eq!(
            repl.rep("(keyword? (first (keys {\":abc\" 123, \":def\" 456})))")
                .unwrap(),
            "false"
        );

        // Testing that hashmaps don't alter function ast
        assert_eq!(
            repl.rep("(def bar (fn [a] {:foo (get a :foo)}))").unwrap(),
            "#'user/bar"
        );
        assert_eq!(
            repl.rep("(bar {:foo (fn [x] x)})").unwrap(),
            "{:foo (fn [x] x)}"
        );
        assert_eq!(repl.rep("(bar {:foo 3})").unwrap(), "{:foo 3}");
    }

    #[test]
    fn test_atoms() {
        let repl = Repl::default().unwrap();
        repl.rep("(def e (atom {\"+\" +}))").unwrap();
        repl.rep("(swap! e assoc \"-\" -)").unwrap();
        assert_eq!(repl.rep("((get @e \"+\") 7 8)").unwrap(), "15");
        assert_eq!(repl.rep("((get @e \"-\") 11 8)").unwrap(), "3");
        repl.rep("(swap! e assoc \"foo\" (list))").unwrap();
        assert_eq!(repl.rep("(get @e \"foo\")").unwrap(), "()");
        repl.rep("(swap! e assoc \"bar\" '(1 2 3))").unwrap();
        assert_eq!(repl.rep("(get @e \"bar\")").unwrap(), "(1 2 3)");

        // Additional tests for atom manipulation
        repl.rep("(def my-atom (atom 0))").unwrap();
        assert_eq!(repl.rep("@my-atom").unwrap(), "0");

        repl.rep("(swap! my-atom inc)").unwrap();
        assert_eq!(repl.rep("@my-atom").unwrap(), "1");

        repl.rep("(swap! my-atom (fn [n] (* (+ n n) 2)))").unwrap();
        assert_eq!(repl.rep("@my-atom").unwrap(), "4");

        repl.rep("(reset! my-atom 0)").unwrap();
        assert_eq!(repl.rep("@my-atom").unwrap(), "0");
    }

    #[test]
    fn test_metadata_on_mal_functions() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(meta (fn [a] a))").unwrap(), "nil");
        assert_eq!(
            repl.rep("(meta (with-meta (fn [a] a) {\"b\" 1}))").unwrap(),
            "{\"b\" 1}"
        );
        assert_eq!(
            repl.rep("(meta (with-meta (fn [a] a) \"abc\"))").unwrap(),
            "{:tag \"abc\"}"
        );

        assert_eq!(
            repl.rep("(def l-wm (with-meta (fn [a] a) {\"b\" 2}))")
                .unwrap(),
            "#'user/l-wm"
        );
        assert_eq!(repl.rep("(meta l-wm)").unwrap(), "{\"b\" 2}");

        assert_eq!(
            repl.rep("(meta (with-meta l-wm {\"new_meta\" 123}))")
                .unwrap(),
            "{\"new_meta\" 123}"
        );
        assert_eq!(repl.rep("(meta l-wm)").unwrap(), "{\"b\" 2}");

        assert_eq!(
            repl.rep("(def f-wm (with-meta (fn [a] (+ 1 a)) {\"abc\" 1}))")
                .unwrap(),
            "#'user/f-wm"
        );
        assert_eq!(repl.rep("(meta f-wm)").unwrap(), "{\"abc\" 1}");

        assert_eq!(
            repl.rep("(meta (with-meta f-wm {\"new_meta\" 123}))")
                .unwrap(),
            "{\"new_meta\" 123}"
        );
        assert_eq!(repl.rep("(meta f-wm)").unwrap(), "{\"abc\" 1}");

        assert_eq!(
            repl.rep("(def ^{\"abc\" 1} f-wm2 (fn [a] (+ 1 a)))")
                .unwrap(),
            "#'user/f-wm2"
        );
        assert_eq!(repl.rep("(meta #'f-wm2)").unwrap(), "{\"abc\" 1}");

        assert_eq!(repl.rep("(meta +)").unwrap(), "nil");

        assert_eq!(
            repl.rep("(def gen-plusX (fn [x] (with-meta (fn [b] (+ x b)) {\"meta\" 1})))")
                .unwrap(),
            "#'user/gen-plusX"
        );
        assert_eq!(
            repl.rep("(def plus7 (gen-plusX 7))").unwrap(),
            "#'user/plus7"
        );
        assert_eq!(repl.rep("(plus7 8)").unwrap(), "15");
        assert_eq!(repl.rep("(meta #'plus7)").unwrap(), "nil");
        assert_eq!(
            repl.rep("(meta (with-meta plus7 {\"meta\" 2}))").unwrap(),
            "{\"meta\" 2}"
        );
    }

    #[test]
    fn test_def() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(def x {:a 1})").unwrap(), "#'user/x");
        assert_eq!(repl.rep("(def ^:dyn y)").unwrap(), "#'user/y");
        assert_eq!(repl.rep("(meta #'y)").unwrap(), "{:dyn true}");
    }

    #[test]
    fn test_meta() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(def ^:tx x)").unwrap(), "#'user/x");
        assert_eq!(repl.rep("(meta #'x)").unwrap(), "{:tx true}");
        assert_eq!(repl.rep("(def ^{:ty 23} y)").unwrap(), "#'user/y");
        assert_eq!(repl.rep("(meta #'y)").unwrap(), "{:ty 23}");
    }

    #[test]
    fn test_validation_functions() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(string? \"\")").unwrap(), "true");
        assert_eq!(repl.rep("(string? 'abc)").unwrap(), "false");
        assert_eq!(repl.rep("(string? \"abc\")").unwrap(), "true");
        assert_eq!(repl.rep("(string? :abc)").unwrap(), "false");
        assert_eq!(repl.rep("(string? (keyword \"abc\"))").unwrap(), "false");
        assert_eq!(repl.rep("(string? 234)").unwrap(), "false");
        assert_eq!(repl.rep("(string? nil)").unwrap(), "false");

        assert_eq!(repl.rep("(number? 123)").unwrap(), "true");
        assert_eq!(repl.rep("(number? -1)").unwrap(), "true");
        assert_eq!(repl.rep("(number? nil)").unwrap(), "false");
        assert_eq!(repl.rep("(number? false)").unwrap(), "false");
        assert_eq!(repl.rep("(number? \"123\")").unwrap(), "false");
    }

    #[test]
    fn test_fn_macro() {
        let repl = Repl::default().unwrap();
        repl.rep("(def add1 (fn [x] (+ x 1)))").unwrap();

        // Testing fn? function
        assert_eq!(repl.rep("(fn? +)").unwrap(), "true");
        assert_eq!(repl.rep("(fn? add1)").unwrap(), "true");
        assert_eq!(repl.rep("(fn? cond)").unwrap(), "false");
        assert_eq!(repl.rep("(fn? \"+\")").unwrap(), "false");
        assert_eq!(repl.rep("(fn? :+)").unwrap(), "false");
        assert_eq!(repl.rep("(fn? (fn [] 0))").unwrap(), "true");
        assert_eq!(
            repl.rep("(fn? ^{\"ismacro\" true} (fn [] 0))").unwrap(),
            "true"
        );

        // Testing macro? function
        assert_eq!(repl.rep("(macro? cond)").unwrap(), "true");
        assert_eq!(repl.rep("(macro? +)").unwrap(), "false");
        assert_eq!(repl.rep("(macro? add1)").unwrap(), "false");
        assert_eq!(repl.rep("(macro? \"+\")").unwrap(), "false");
        assert_eq!(repl.rep("(macro? :+)").unwrap(), "false");
        assert_eq!(repl.rep("(macro? {})").unwrap(), "false");
    }

    #[test]
    fn test_conj_seq_metadata() {
        let repl = Repl::default().unwrap();
        repl.rep("(def l-wm (with-meta (fn [a] a) {\"b\" 2}))")
            .unwrap();

        // Testing conj function
        assert_eq!(repl.rep("(conj (list) 1)").unwrap(), "(1)");
        assert_eq!(repl.rep("(conj (list 1) 2)").unwrap(), "(2 1)");
        assert_eq!(repl.rep("(conj (list 2 3) 4)").unwrap(), "(4 2 3)");
        assert_eq!(repl.rep("(conj (list 2 3) 4 5 6)").unwrap(), "(6 5 4 2 3)");
        assert_eq!(repl.rep("(conj (list 1) (list 2 3))").unwrap(), "((2 3) 1)");

        assert_eq!(repl.rep("(conj [] 1)").unwrap(), "[1]");
        assert_eq!(repl.rep("(conj [1] 2)").unwrap(), "[1 2]");
        assert_eq!(repl.rep("(conj [2 3] 4)").unwrap(), "[2 3 4]");
        assert_eq!(repl.rep("(conj [2 3] 4 5 6)").unwrap(), "[2 3 4 5 6]");
        assert_eq!(repl.rep("(conj [1] [2 3])").unwrap(), "[1 [2 3]]");

        // Testing seq function
        assert_eq!(repl.rep("(seq \"abc\")").unwrap(), "(\\a \\b \\c)");
        assert_eq!(repl.rep("(seq '(2 3 4))").unwrap(), "(2 3 4)");
        assert_eq!(repl.rep("(seq [2 3 4])").unwrap(), "(2 3 4)");

        assert_eq!(repl.rep("(seq \"\")").unwrap(), "nil");
        assert_eq!(repl.rep("(seq '())").unwrap(), "nil");
        assert_eq!(repl.rep("(seq [])").unwrap(), "nil");
        assert_eq!(repl.rep("(seq nil)").unwrap(), "nil");

        // Testing metadata on collections
        assert_eq!(repl.rep("(meta [1 2 3])").unwrap(), "nil");
        assert_eq!(
            repl.rep("(with-meta [1 2 3] {\"a\" 1})").unwrap(),
            "[1 2 3]"
        );
        assert_eq!(
            repl.rep("(meta (with-meta [1 2 3] {\"a\" 1}))").unwrap(),
            "{\"a\" 1}"
        );
        assert_eq!(
            repl.rep("(vector? (with-meta [1 2 3] {\"a\" 1}))").unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(meta (with-meta [1 2 3] \"abc\"))").unwrap(),
            "{:tag \"abc\"}"
        );
        assert_eq!(repl.rep("(with-meta [] \"abc\")").unwrap(), "[]");
        assert_eq!(
            repl.rep("(meta (with-meta (list 1 2 3) {\"a\" 1}))")
                .unwrap(),
            "{\"a\" 1}"
        );
        assert_eq!(
            repl.rep("(list? (with-meta (list 1 2 3) {\"a\" 1}))")
                .unwrap(),
            "true"
        );
        assert_eq!(repl.rep("(with-meta (list) {\"a\" 1})").unwrap(), "()");
        assert_eq!(
            repl.rep("(empty? (with-meta (list) {\"a\" 1}))").unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(meta (with-meta {\"abc\" 123} {\"a\" 1}))")
                .unwrap(),
            "{\"a\" 1}"
        );
        assert_eq!(
            repl.rep("(map? (with-meta {\"abc\" 123} {\"a\" 1}))")
                .unwrap(),
            "true"
        );
        assert_eq!(repl.rep("(with-meta {} {\"a\" 1})").unwrap(), "{}");
        assert_eq!(
            repl.rep("(meta (with-meta [4 5 6] {\"b\" 2}))").unwrap(),
            "{\"b\" 2}"
        );
        assert_eq!(
            repl.rep("(meta (with-meta l-wm {\"new_meta\" 123}))")
                .unwrap(),
            "{\"new_meta\" 123}"
        );
        assert_eq!(repl.rep("(meta l-wm)").unwrap(), "{\"b\" 2}");
        assert_eq!(repl.rep("(meta +)").unwrap(), "nil");

        repl.rep("(def gen-plusX (fn [x] (with-meta (fn [b] (+ x b)) {\"meta\" 1})))")
            .unwrap();
        repl.rep(" (def plus7 (gen-plusX 7))").unwrap();
        repl.rep("(def plus8 (gen-plusX 8))").unwrap();

        assert_eq!(repl.rep("(plus7 8)").unwrap(), "15");
        assert_eq!(repl.rep("(meta plus7)").unwrap(), "{\"meta\" 1}");
        assert_eq!(repl.rep("(meta plus8)").unwrap(), "{\"meta\" 1}");
        assert_eq!(
            repl.rep("(meta (with-meta plus7 {\"meta\" 2}))").unwrap(),
            "{\"meta\" 2}"
        );
    }

    #[test]
    fn test_time_ms() {
        let repl = Repl::default().unwrap();
        let start_time: i64 = repl.rep("(time-ms)").unwrap().parse().unwrap();
        assert_ne!(start_time, 0);
        thread::sleep(Duration::from_millis(10));
        let end_time: i64 = repl.rep("(time-ms)").unwrap().parse().unwrap();
        assert!(end_time > start_time);
    }

    #[test]
    fn test_macro_definition() {
        let repl = Repl::default().unwrap();
        repl.rep("(def f (fn [x] (number? x)))").unwrap();
        repl.rep("(defmacro m f)").unwrap();
        let result_f: bool = repl.rep("(f (+ 1 1))").unwrap().parse().unwrap();
        assert_eq!(result_f, true);
        let result_m: bool = repl.rep("(m (+ 1 1))").unwrap().parse().unwrap();
        assert_eq!(result_m, true);
    }

    #[test]
    fn test_trivial_macros() {
        let repl = Repl::default().unwrap();
        repl.rep("(defmacro one (fn [] 1))").unwrap();
        let result_one: i32 = repl.rep("(one)").unwrap().parse().unwrap();
        assert_eq!(result_one, 1);
        repl.rep("(defmacro two (fn [] 2))").unwrap();
        let result_two: i32 = repl.rep("(two)").unwrap().parse().unwrap();
        assert_eq!(result_two, 2);
    }

    #[test]
    fn test_unless_macros() {
        let repl = Repl::default().unwrap();
        repl.rep("(defmacro unless (fn [pred a b] `(if ~pred ~b ~a)))")
            .unwrap();
        let res = repl.rep("(unless false 7 8)").unwrap(); // if
        let result_unless: i32 = res.parse().unwrap();
        assert_eq!(result_unless, 7);
        repl.rep("(defmacro unless2 (fn [pred a b] (list 'if (list 'not pred) a b)))")
            .unwrap();
        let result_unless2: i32 = repl.rep("(unless2 true 7 8)").unwrap().parse().unwrap();
        assert_eq!(result_unless2, 8);
    }

    #[test]
    fn test_macroexpand() {
        let repl = Repl::default().unwrap();
        repl.rep("(defmacro one (fn [] 1))").unwrap();
        repl.rep("(defmacro unless (fn [pred a b] `(if ~pred ~b ~a)))")
            .unwrap();
        let result_one_macroexpand: i32 = repl.rep("(macroexpand (one))").unwrap().parse().unwrap();
        assert_eq!(result_one_macroexpand, 1);
        let result_unless_macroexpand = repl.rep("(macroexpand (unless 'PRED 'A 'B))").unwrap();
        assert_eq!(result_unless_macroexpand, "(if PRED B A)");
    }

    #[test]
    fn test_macro_evaluation() {
        let repl = Repl::default().unwrap();
        repl.rep("(defmacro identity (fn [x] x))").unwrap();
        let result_identity_macro = repl
            .rep("(let [a 123] (macroexpand (identity a)))")
            .unwrap();
        assert_eq!(result_identity_macro, "123");
        let result_identity: i32 = repl
            .rep("(let [a 123] (identity a))")
            .unwrap()
            .parse()
            .unwrap();
        assert_eq!(result_identity, 123);
    }

    #[test]
    fn test_empty_list_macro() {
        let repl = Repl::default().unwrap();
        let result_empty_list = repl.rep("()").unwrap();
        assert_eq!(result_empty_list, "()");
    }

    #[test]
    fn test_quasiquote_macro() {
        let repl = Repl::default().unwrap();
        let result_quasiquote = repl.rep("`(1)").unwrap();
        assert_eq!(result_quasiquote, "(1)");
    }

    #[test]
    fn test_set_fns() {
        let repl = Repl::default().unwrap();
        let res = repl.rep("(set {:a 2 :b 2})").unwrap();
        assert!(res == "#{[:a 2] [:b 2]}" || res == "#{[:b 2] [:a 2]}");
        let res = repl.rep("(set \"st\")").unwrap();
        assert!(res == "#{\\t \\s}" || res == "#{\\s \\t}");
        let res = repl.rep("(set [1 2])").unwrap();
        assert!(res == "#{1 2}" || res == "#{2 1}");
    }

    #[test]
    fn test_do() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(do)").unwrap(), "nil");
        assert_eq!(repl.rep("(do (prn 101))").unwrap(), "nil");
        assert_eq!(repl.rep("(do (prn 102) 7)").unwrap(), "7");
        assert_eq!(repl.rep("(do (prn 101) (prn 102) (+ 1 2))").unwrap(), "3");
        assert_eq!(repl.rep("(do (def a 6) 7 (+ a 8))").unwrap(), "14");
    }

    #[test]
    #[should_panic]
    fn error_symbol() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(fkdfd 'a 124)").unwrap(), "nil");
    }

    #[test]
    fn test_type_function() {
        let repl = Repl::default().unwrap();

        // Test type of integer
        assert_eq!(repl.rep("(type 42)").unwrap(), "Integer");

        // Test type of string
        assert_eq!(repl.rep("(type \"hello\")").unwrap(), "String");

        // Test type of symbol
        assert_eq!(repl.rep("(type 'symbol)").unwrap(), "Symbol");

        // Test type of list
        assert_eq!(repl.rep("(type '(1 2 3))").unwrap(), "List");

        // Test type of vector
        assert_eq!(repl.rep("(type [1 2 3])").unwrap(), "Vector");

        // Test type of map
        assert_eq!(repl.rep("(type {:a 1 :b 2})").unwrap(), "HashMap");

        // Test type of keyword
        assert_eq!(repl.rep("(type :keyword)").unwrap(), "Keyword");

        // Test type of function
        assert_eq!(repl.rep("(type +)").unwrap(), "Function");

        // Test type of nil
        assert_eq!(repl.rep("(type nil)").unwrap(), "Nil");

        // Test type of boolean
        assert_eq!(repl.rep("(type true)").unwrap(), "Boolean");

        // Test type of atom
        assert_eq!(repl.rep("(type (atom 0))").unwrap(), "Atom");

        // Test type of unknown type
        assert_eq!(repl.rep("(type \\a)").unwrap(), "Character");
    }

    #[test]
    #[should_panic(expected = "'bee' symbol not found in this context")]
    fn test_not_found() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("bee").unwrap(), "Integer");
    }

    #[test]
    fn test_ns_symbol() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("*ns*").unwrap(), "#Namespace<user>");
        assert_eq!(repl.rep("(ns boo)").unwrap(), "nil");
        assert_eq!(repl.rep("*ns*").unwrap(), "#Namespace<boo>");
    }

    #[test]
    fn test_redef() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(def x 3)").unwrap(), "#'user/x");
        assert_eq!(repl.rep("x").unwrap(), "3");
        assert_eq!(repl.rep("(def x 4)").unwrap(), "#'user/x");
        assert_eq!(repl.rep("x").unwrap(), "4");
    }

    // #[test]
    // fn test_refer_basic() {
    //     let repl = Repl::default().unwrap();
    //     assert_eq!(repl.rep("(def x 3)").unwrap(), "#'user/x");
    //     assert_eq!(repl.rep("(ns foo)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(def y 4)").unwrap(), "#'foo/y");
    //     assert_eq!(repl.rep("(ns user)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(refer 'foo)").unwrap(), "nil");
    //     assert_eq!(repl.rep("x").unwrap(), "3");
    //     assert_eq!(repl.rep("y").unwrap(), "4");
    // }

    // #[test]
    // fn test_refer_override() {
    //     let repl = Repl::default().unwrap();
    //     assert_eq!(repl.rep("(def x 3)").unwrap(), "#'user/x");
    //     assert_eq!(repl.rep("(ns foo)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(def x 4)").unwrap(), "#'foo/x");
    //     assert_eq!(repl.rep("(ns user)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(refer 'foo)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(var x)").unwrap(), "#'user/x");
    //     assert_eq!(repl.rep("x").unwrap(), "3");
    // }

    // #[test]
    // #[should_panic(expected = "`x` symbol not found in the environment")]
    // fn test_refer_private_symbols() {
    //     let repl = Repl::default().unwrap();
    //     assert_eq!(repl.rep("(ns foo)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(def ^:private x 3)").unwrap(), "#'foo/x");
    //     assert_eq!(repl.rep("(meta #'foo/x)").unwrap(), "{:private true}");
    //     assert_eq!(repl.rep("(ns user)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(refer 'foo)").unwrap(), "nil");
    //     repl.rep("x").unwrap();
    // }

    #[test]
    fn test_find_ns() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(ns foo)").unwrap(), "nil");
        assert_eq!(repl.rep("(ns user)").unwrap(), "nil");
        assert_eq!(repl.rep("(find-ns 'foo)").unwrap(), "#Namespace<foo>");
        assert_eq!(repl.rep("(find-ns 'fee)").unwrap(), "nil");
    }

    #[test]
    fn test_starts_with_q() {
        let repl = Repl::default().unwrap();
        assert_eq!(
            repl.rep("(fmlisp.string/starts-with? \"beer\" \"be\")")
                .unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(fmlisp.string/starts-with? \"beer\" \"b\")")
                .unwrap(),
            "true"
        );
        assert_eq!(
            repl.rep("(fmlisp.string/starts-with? \"beer\" \"e\")")
                .unwrap(),
            "false"
        );
        assert_eq!(
            repl.rep("(fmlisp.string/starts-with? \"a\" \"ab\")")
                .unwrap(),
            "false"
        );
        assert_eq!(
            repl.rep("(fmlisp.string/starts-with? \"ab\" \"abc\")")
                .unwrap(),
            "false"
        );
    }

    #[test]
    fn test_ends_with() {
        let repl = Repl::default().unwrap();

        assert_eq!(
            repl.rep("(fmlisp.string/ends-with? \"beer\" \"er\")")
                .unwrap(),
            "true"
        );

        assert_eq!(
            repl.rep("(fmlisp.string/ends-with? \"beer\" \"r\")")
                .unwrap(),
            "true"
        );

        assert_eq!(
            repl.rep("(fmlisp.string/ends-with? \"beer\" \"b\")")
                .unwrap(),
            "false"
        );

        assert_eq!(
            repl.rep("(fmlisp.string/ends-with? \"a\" \"ab\")").unwrap(),
            "false"
        );

        assert_eq!(
            repl.rep("(fmlisp.string/ends-with? \"ab\" \"abc\")")
                .unwrap(),
            "false"
        );
    }

    #[test]
    fn test_all_ns() {
        let repl = Repl::default().unwrap();
        let res = repl.rep("(all-ns)").unwrap();
        assert!(res.contains("#Namespace<user>"));
        assert!(res.contains("#Namespace<fmlisp.lang>"));
    }

    #[test]
    fn test_instance_q() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(is? 'Integer 12)").unwrap(), "true");
        assert_eq!(repl.rep("(is? 'Integer 12.4)").unwrap(), "false");
        assert_eq!(repl.rep("(is? 'Float 12.4)").unwrap(), "true");
        assert_eq!(repl.rep("(is? 'String \"abc\")").unwrap(), "true");
        assert_eq!(repl.rep("(is? 'HashMap {:x 1})").unwrap(), "true");
        assert_eq!(repl.rep("(is? 'Vector [1 2 3])").unwrap(), "true");
    }

    #[test]
    fn test_is_private_public() {
        let repl = Repl::default().unwrap();
        repl.rep("(def ^:private prv 1)").unwrap();
        repl.rep("(def pub 2)").unwrap();
        assert_eq!(repl.rep("(private? prv)").unwrap(), "true");
        assert_eq!(repl.rep("(private? pub)").unwrap(), "false");
        assert_eq!(repl.rep("(public? pub)").unwrap(), "true");
        assert_eq!(repl.rep("(public? prv)").unwrap(), "false");
    }

    #[test]
    fn test_is_even_odd() {
        let repl = Repl::default().unwrap();

        assert_eq!(repl.rep("(even? 0)").unwrap(), "true");
        assert_eq!(repl.rep("(even? 2)").unwrap(), "true");
        assert_eq!(repl.rep("(even? 42)").unwrap(), "true");
        assert_eq!(repl.rep("(even? -10)").unwrap(), "true");

        assert_eq!(repl.rep("(even? 1)").unwrap(), "false");
        assert_eq!(repl.rep("(even? 3)").unwrap(), "false");
        assert_eq!(repl.rep("(even? 41)").unwrap(), "false");
        assert_eq!(repl.rep("(even? -11)").unwrap(), "false");

        assert_eq!(repl.rep("(odd? 1)").unwrap(), "true");
        assert_eq!(repl.rep("(odd? 3)").unwrap(), "true");
        assert_eq!(repl.rep("(odd? 41)").unwrap(), "true");
        assert_eq!(repl.rep("(odd? -11)").unwrap(), "true");

        assert_eq!(repl.rep("(odd? 0)").unwrap(), "false");
        assert_eq!(repl.rep("(odd? 2)").unwrap(), "false");
        assert_eq!(repl.rep("(odd? 42)").unwrap(), "false");
        assert_eq!(repl.rep("(odd? -10)").unwrap(), "false");
    }

    #[test]
    fn test_some() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(some even? '(1 2 3 4))").unwrap(), "true");
        assert_eq!(repl.rep("(some true? [false false false])").unwrap(), "nil");
    }

    #[test]
    fn test_invoke_var() {
        let repl = Repl::default().unwrap();
        assert_eq!(
            repl.rep("(intern (the-ns 'user) 'sum (fn [a b c] (+ a b c)))")
                .unwrap(),
            "#'user/sum"
        );
        assert_eq!(repl.rep("(sum 1 2 3)").unwrap(), "6");
    }

    #[test]
    fn test_number_operations() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(+ 1 2 3)").unwrap(), "6");
    }

    #[test]
    fn test_get_keyword_hm() {
        let repl = Repl::default().unwrap();
        assert_eq!(repl.rep("(def x {:a 1})").unwrap(), "#'user/x");
        assert_eq!(repl.rep("(:a x)").unwrap(), "1");
        assert_eq!(repl.rep("(:a 1)").unwrap(), "nil");
        assert_eq!(repl.rep("(:a \"abc\")").unwrap(), "nil");
        assert_eq!(repl.rep("(:b x)").unwrap(), "nil");
    }

    #[test]
    #[should_panic(expected = "'d' symbol not found in this context")]
    fn test_get_keyword_hm_panic() {
        let repl = Repl::default().unwrap();
        repl.rep("(:a d)").unwrap();
    }

    #[test]
    fn test_loop_basic() {
        let repl = Repl::default().unwrap();

        assert_eq!(
            repl.rep("(loop [i 0] (if (< i 5) (recur (inc i)) i))")
                .unwrap(),
            "5"
        );
        assert_eq!(
            repl.rep("(loop [i 0 acc 0] (if (< i 5) (recur (inc i) (+ acc i)) acc))")
                .unwrap(),
            "10"
        );
    }

    #[test]
    fn test_loop_nested() {
        let repl = Repl::default().unwrap();

        assert_eq!(repl.rep("(loop [i 0 j 0] (if (< i 3) (recur (inc i) 0) (if (< j 3) (recur i (inc j)) [i j])))").unwrap(), "[3 3]");
    }

    #[test]
    fn test_loop_with_binding() {
        let repl = Repl::default().unwrap();

        assert_eq!(
            repl.rep("(loop [i 0 x 0] (if (< i 5) (let [y (+ x i)] (recur (inc i) y)) x))")
                .unwrap(),
            "10"
        );
    }

    // #[test]
    // fn test_require_alias() {
    //     let repl = Repl::default().unwrap();
    //     assert_eq!(repl.rep("(ns air)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(def rock 1)").unwrap(), "#'air/rock");
    //     assert_eq!(repl.rep("(ns user)").unwrap(), "nil");
    //     assert_eq!(repl.rep("(require '[air :as fire])").unwrap(), "nil");
    //     assert_eq!(repl.rep("fire/rock").unwrap(), "1");
    // }
}
