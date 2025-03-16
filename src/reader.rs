use regex::{Captures, Regex};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::OnceLock;

use crate::key;
use crate::sym;
use crate::list;
use crate::error;
use crate::core;
use crate::env::Environment;
use crate::keyword::Keyword;
use crate::symbol::Symbol;
use crate::utils::{self, str_first};
use crate::values::LispErr::ErrString;
use crate::values::{
    self, error, hash_map_from_kv, hash_map_from_vec, list_from_vec, set_from_vec, vector_from_vec,
    LispErr, ToValue, Value, ValueRes,
};

/// A Token is a string with row,col position inside
/// the original source.
pub struct Token {
    tok: String,
    row: usize,
    col: usize,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} ({}, {})", self.tok, self.row, self.col)
    }
}

#[derive(Debug)]
pub struct Reader {
    // Tokens is a vector of forms which contains tokens
    tokens: Vec<Token>,
    pos: usize, // position counter inside the sexpr
    env: Rc<Environment>,
}

impl Reader {
    fn next(&mut self) -> Result<&Token, LispErr> {
        let token = match self.tokens.get(self.pos) {
            Some(t) => t,
            None => return Err(ErrString("EOF while reading".to_string())),
        };

        self.pos += 1;
        Ok(token)
    }

    pub fn peek(&self) -> Result<&Token, LispErr> {
        match self.tokens.get(self.pos) {
            Some(t) => return Ok(t),
            None => return Err(ErrString("EOF while reading".to_string())),
        };
    }

    fn prev(&mut self) {
        self.pos -= 1;
    }

    /// Returns a tuple `(row,col)`.
    fn get_tok_pos(&self) -> (usize, usize) {
        let tok = self.peek().unwrap();
        (tok.row + 1, tok.col + 1)
    }
}

//
// REGEX are static
//
pub fn int_regex() -> &'static Regex {
    static INT_RE: OnceLock<Regex> = OnceLock::new();
    INT_RE.get_or_init(|| Regex::new(r"^\d+$").unwrap())
}
pub fn float_regex() -> &'static Regex {
    static FLOAT_RE: OnceLock<Regex> = OnceLock::new();
    FLOAT_RE.get_or_init(|| Regex::new(r"^-?\d+(\.\d+)?$").unwrap())
}
pub fn str_regex() -> &'static Regex {
    static STR_RE: OnceLock<Regex> = OnceLock::new();
    STR_RE.get_or_init(|| Regex::new(r#""(?:\\.|[^\\"])*""#).unwrap())
}
pub fn unescape_regex() -> &'static Regex {
    static UN_RE: OnceLock<Regex> = OnceLock::new();
    UN_RE.get_or_init(|| Regex::new(r#"\\(.)"#).unwrap())
}
pub fn tokens_regex() -> &'static Regex {
    static TOKEN_RE: OnceLock<Regex> = OnceLock::new();
    TOKEN_RE.get_or_init(|| {
        Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@#_]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)"###)
            .unwrap()
    })
}

/// Returns a Reader instance.
/// `s` is the source string to extract the tokens,
/// `multi_form` enable multi form parsing;
/// helpful if `s` it's meant to be a multi form string
/// otherwhise only the first form will be tokenized.
pub fn tokenize(s: &str, env: Rc<Environment>) -> Reader {
    // Trim string to avoid processing ending spaces and \n
    let trim_s = s.trim();
    let mut tokens = vec![];
    let mut col: usize = 0;
    let mut row: usize = 0;

    for cap in tokens_regex().captures_iter(trim_s) {
        let full_str = &cap[0];
        let tok_str = &cap[1];

        if tok_str.starts_with(";") {
            let newline_count = utils::count_char_occurrences(full_str, '\n');
            row += newline_count; // A comment will have an ending new line or EOF
            continue; // it's a comment form
        }

        if str_first(tok_str) != "\"" {
            // Count newlines if it's not a string to increment the row index
            let newline_count = utils::count_char_occurrences(full_str, '\n');
            row += newline_count;
        } else {
            let string_start_idx = full_str.find('"').unwrap();
            let leading_s = &full_str[..string_start_idx];
            row += utils::count_newlines(leading_s);
        }

        // Count only leading spaces to get the position at the beginning of the token
        match full_str.rfind('\n') {
            Some(last_newline_idx) => {
                // If new lines are in the token, get the length after the last \n
                col = utils::count_leading_whitespace(&full_str[last_newline_idx..]) - 1;
            }
            None => {
                col += utils::count_leading_whitespace(full_str);
            }
        }

        let tok = Token {
            tok: String::from(tok_str),
            row,
            col,
        };

        // Increase column by token length
        col += tok_str.len();
        if str_first(tok_str) == "\"" {
            // If string, now increment the number of rows in a string
            row += utils::count_char_occurrences(tok_str, '\n');
        }

        tokens.push(tok);
    }

    Reader {
        tokens,
        env,
        pos: 0,
    }
}

fn unescape_str(s: &str) -> String {
    unescape_regex()
        .replace_all(&s, |caps: &Captures| {
            format!("{}", if &caps[1] == "n" { "\n" } else { &caps[1] })
        })
        .to_string()
}

fn error_with_pos(err_str: &str, r: &mut Reader) -> Result<Value, LispErr> {
    r.prev();
    let mut err = crate::errors::Error::new_from_str(String::from(err_str));
    let tok_pos = r.get_tok_pos();
    err.add_info("at", format!("{}:{}", tok_pos.0, tok_pos.1));
    return Err(values::LispErr::Error(err));
}

fn read_atom(r: &mut Reader) -> ValueRes {
    let token = r.next()?;

    // Using &token[..] to match against a reference to the string
    match token.tok.as_str() {
        "nil" => Ok(Value::Nil),
        "false" => Ok(Value::Bool(false)),
        "true" => Ok(Value::Bool(true)),
        _ => {
            let token_str = token.tok.as_str();
            if int_regex().is_match(&token_str) {
                Ok(Value::Integer(token_str.parse().unwrap()))
            } else if float_regex().is_match(&token_str) {
                Ok(Value::Float(token_str.parse().unwrap()))
            } else if str_regex().is_match(&token_str) {
                Ok(Value::Str(unescape_str(&token_str[1..token_str.len() - 1])))
            } else if token_str.starts_with("\"") {
                return error_with_pos(format!("Unexpected string: {}", token_str).as_str(), r);
            } else if token_str.starts_with("\\") {
                if token_str.len() == 2 {
                    Ok(Value::Char(token_str.chars().nth(1).unwrap() as char))
                } else {
                    return error_with_pos(
                        format!("Unsupported character: {}", token_str).as_str(),
                        r,
                    );
                }
            } else if token_str.starts_with(":") {
                Ok(Value::Keyword(key!(&token_str[1..].to_string())))
            } else {
                Ok(Value::Symbol(sym!(&token_str.to_string())))
            }
        }
    }
}

enum CollType {
    List,
    Vector,
    HashMap,
    Set,
}

fn read_seq(r: &mut Reader, end: &str, coll_type: CollType) -> ValueRes {
    let mut seq: Vec<Value> = vec![];
    r.next()?; // remove the seq start (open paren)

    loop {
        let token: String = match r.peek() {
            Ok(t) => t.tok.clone(),
            Err(_) => {
                return error_with_pos(format!("Expected {} got EOF", end).as_str(), r);
            }
        };
        if token == end {
            break;
        }
        seq.push(read_form(r)?);
    }
    let _ = r.next(); // end sequence
    match coll_type {
        CollType::List => Ok(list_from_vec(seq)),
        CollType::Vector => Ok(vector_from_vec(seq)),
        CollType::HashMap => Ok(hash_map_from_vec(seq).unwrap()),
        CollType::Set => Ok(set_from_vec(seq)),
    }
}

pub fn read_form(r: &mut Reader) -> ValueRes {
    let token = r.peek()?;

    match token.tok.as_str() {
        // Macro characters: https://clojure.org/reference/reader#macrochars
        "'" => {
            let _ = r.next();
            Ok(list![Value::Symbol(sym!("quote")), read_form(r)?])
        }
        "#" => {
            // Dispatch: https://clojure.org/reference/reader#_dispatch
            let _ = r.next();
            let next_token = r.peek()?;
            match next_token.tok.as_str() {
                // Var-quote (#'sym)
                "'" => {
                    let _ = r.next();
                    Ok(list![Value::Symbol(sym!("var")), read_form(r)?])
                }
                // Ignore next form (#_form)
                "_" => {
                    let _ = r.next();
                    let _ = read_form(r)?; // discard next form
                    read_form(r)
                }
                // Create a set (#{..})
                "{" => read_seq(r, "}", CollType::Set),
                _ => {
                    return error_with_pos(
                        format!("Wrong token found: {}", next_token.tok).as_str(),
                        r,
                    )
                }
            }
        }
        "`" => {
            // Syntax-quote: https://clojure.org/reference/reader#syntax-quote
            let _ = r.next();
            // Ok(list![Value::Symbol(sym!("quasiquote")), read_form(r)?])
            let mut sqr = SyntaxQuoteReader::new(r.env.clone());
            Ok(syntax_quote(&read_form(r)?, &mut sqr))
        }
        "~" => {
            let _ = r.next();
            Ok(list![Value::Symbol(sym!("unquote")), read_form(r)?])
        }
        "~@" => {
            let _ = r.next();
            Ok(list![
                Value::Symbol(sym!("unquote-splicing")),
                read_form(r)?
            ])
        }
        "^" => {
            let _ = r.next();
            let meta = read_form(r)?;
            let ometa = match meta {
                // Symbols and Strings, get inserted into :tags
                Value::Symbol(_) | Value::Str(_) => {
                    hash_map_from_kv(Keyword::new("tag").to_value(), meta.clone())
                }
                // A keyword becomes a key with value true
                Value::Keyword(_) => hash_map_from_kv(meta.clone(), Value::Bool(true)),
                // HashMap is directly cloned (or merged?)
                Value::HashMap(_, _) => meta.clone(),
                _ => return error!("Metadata must be Symbol, Keyword, String or HashMap"),
            };

            let mut obj = read_form(r)?;
            obj.with_meta(&ometa).unwrap();
            Ok(obj)
        }
        "@" => {
            let _ = r.next();
            Ok(list![Value::Symbol(sym!("deref")), read_form(r)?])
        }
        ")" => error_with_pos("Unmatched delimiter: ')'", r),
        "(" => read_seq(r, ")", CollType::List),
        "]" => error_with_pos("Unmatched delimiter: ']'", r),
        "[" => read_seq(r, "]", CollType::Vector),
        "}" => error_with_pos("Unmatched delimiter: '}'", r),
        "{" => read_seq(r, "}", CollType::HashMap),
        _ => read_atom(r),
    }
}

// Current Environment is imported to expand the symbols
// when the Reader find a syntax-quote

pub fn read_str(s: String, env: Rc<Environment>) -> ValueRes {
    let mut reader = tokenize(&s, env);

    // println!("{:#?}", reader);

    read_form(&mut reader)
}

pub fn read_str_multiform(s: String, env: Rc<Environment>) -> Result<Vec<Value>, LispErr> {
    let mut reader = tokenize(&s, env);
    let mut res = vec![];

    println!("{:#?}", reader.tokens);

    loop {
        let v = match read_form(&mut reader) {
            Ok(z) => z,
            Err(e) => return Err(e),
        };
        res.push(v);
        if reader.peek().is_err() {
            break;
        }
    }
    Ok(res)
}

// List of internal reserved keyword used by the compiler
static RESERVED_KW: [&'static str; 16] = [
    "def",
    "let*",
    "macroexpand",
    "do",
    "loop*",
    "recur",
    "if",
    "fn*",
    "eval",
    "quote",
    "quasiquoteexpand",
    "quasiquote",
    "defmacro",
    "try",
    "var",
    "throw",
];

fn resolve_symbol(sym: &mut Symbol, env: Rc<Environment>) -> Symbol {
    if sym.has_ns() || RESERVED_KW.contains(&sym.name.as_str()) {
        return sym.clone();
    }

    match env.get(sym) {
        Ok(val) => match &*val {
            Value::Var(var) => {
                sym.ns = Some(var.ns.name().to_string());
                return sym.clone();
            }
            _ => {}
        },
        Err(_) => {}
    }

    sym.ns = Some(env.get_current_namespace_name());
    sym.clone()
}

/// Struct used to store data for Reader
pub struct SyntaxQuoteReader {
    gensyms: HashMap<Symbol, Symbol>,
    env: Rc<Environment>,
}

impl SyntaxQuoteReader {
    pub fn new(env: Rc<Environment>) -> SyntaxQuoteReader {
        SyntaxQuoteReader {
            gensyms: HashMap::new(),
            env,
        }
    }
}

fn expand_list(elts: &Vec<Value>, sqr: &mut SyntaxQuoteReader) -> Value {
    let mut acc = list![];
    for elt in elts.iter().rev() {
        if let Value::List(v, _) = elt {
            if v.len() == 2 {
                if let Value::Symbol(ref s) = v[0] {
                    if s.name() == "unquote-splicing" {
                        acc = list![Value::Symbol(sym!("concat")), v[1].clone(), acc];
                        continue;
                    }
                }
            }
        }
        acc = list![Value::Symbol(sym!("cons")), syntax_quote(&elt, sqr), acc];
    }
    acc
}

pub fn syntax_quote(ast: &Value, sqr: &mut SyntaxQuoteReader) -> Value {
    match ast.clone() {
        Value::Symbol(mut sym) => {
            let ret: Value;
            if sym.ns == None && sym.name.ends_with("#") {
                // Gensym literal
                if let Some(gs) = sqr.gensyms.get(&sym) {
                    ret = Value::Symbol(gs.clone());
                } else {
                    // remove trailing #
                    let gen_sym = format!(
                        "{}__{}__auto__",
                        utils::remove_last_char(sym.name.clone()),
                        core::next_id()
                    );
                    let new_sym = Symbol::new(gen_sym.as_str());
                    sqr.gensyms.insert(sym, new_sym.clone());
                    ret = Value::Symbol(new_sym);
                }
            } else {
                // Resolve every symbol
                let s = resolve_symbol(&mut sym, sqr.env.clone());
                ret = Value::Symbol(s);
            }

            list![Value::Symbol(sym!("quote")), ret]
        }
        Value::List(v, _) => {
            if v.len() == 2 {
                if let Value::Symbol(ref s) = v[0] {
                    if s.name() == "unquote" {
                        return v[1].clone();
                    }
                }
            }
            expand_list(&v, sqr)
        }
        Value::Vector(v, _) => list![Value::Symbol(sym!("vec")), expand_list(&v, sqr)],
        Value::HashMap(_, _) | Value::Set(_, _) => {
            list![Value::Symbol(sym!("quote")), ast.clone()]
        }
        _ => ast.clone(),
    }
}
