use regex::{Captures, Regex};
use std::rc::Rc;
use std::sync::OnceLock;

use crate::keyword::Keyword;
use crate::symbol::Symbol;
use crate::values::LispErr::ErrString;
use crate::values::{
    self, error, hash_map_from_kv, hash_map_from_vec, list_from_vec, set_from_vec, vector_from_vec,
    LispErr, ToValue, Value, ValueRes,
};

struct Reader {
    tokens: Vec<String>,
    pos: usize,
}

impl Reader {
    fn next(&mut self) -> Result<String, LispErr> {
        let token = match self.tokens.get(self.pos) {
            Some(t) => t.clone(),
            None => return Err(ErrString("underflow".to_string())),
        };

        self.pos += 1;
        Ok(token)
    }

    fn peek(&self) -> Result<String, LispErr> {
        Ok(self
            .tokens
            .get(self.pos)
            .ok_or(ErrString("underflow".to_string()))?
            .to_string())
    }
}

//
// REGEX are static
//
pub fn int_regex() -> &'static Regex {
    static INT_RE: OnceLock<Regex> = OnceLock::new();
    INT_RE.get_or_init(|| Regex::new(r"^-?\[0-9\]+$").unwrap())
}
pub fn float_regex() -> &'static Regex {
    static FLOAT_RE: OnceLock<Regex> = OnceLock::new();
    FLOAT_RE.get_or_init(|| Regex::new(r"^-?\\d+(\\.\\d+)?$").unwrap())
}
pub fn str_regex() -> &'static Regex {
    static STR_RE: OnceLock<Regex> = OnceLock::new();
    STR_RE.get_or_init(|| Regex::new(r#""(?:\\\\.|\[^\\\\"\])\*""#).unwrap())
}
pub fn unescape_regex() -> &'static Regex {
    static UN_RE: OnceLock<Regex> = OnceLock::new();
    UN_RE.get_or_init(|| Regex::new(r#"\\(.)"#).unwrap())
}
pub fn tokens_regex() -> &'static Regex {
    static TOKEN_RE: OnceLock<Regex> = OnceLock::new();
    TOKEN_RE.get_or_init(|| {
        Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)"###)
            .unwrap()
    })
}

pub fn tokenize(s: &str) -> Vec<String> {
    let mut res = vec![];
    for cap in tokens_regex().captures_iter(s) {
        if cap[1].starts_with(";") {
            continue;
        }
        println!("{}", String::from(&cap[1]));
        res.push(String::from(&cap[1]));
    }
    res
}

fn unescape_str(s: &str) -> String {
    unescape_regex()
        .replace_all(&s, |caps: &Captures| {
            format!("{}", if &caps[1] == "n" { "\n" } else { &caps[1] })
        })
        .to_string()
}

fn read_atom(r: &mut Reader) -> ValueRes {
    let token = r.next()?;

    // Using &token[..] to match against a reference to the string
    match &token[..] {
        "nil" => Ok(Value::Nil),
        "false" => Ok(Value::Bool(false)),
        "true" => Ok(Value::Bool(true)),
        _ => {
            if int_regex().is_match(&token) {
                Ok(Value::Integer(token.parse().unwrap()))
            } else if float_regex().is_match(&token) {
                Ok(Value::Float(token.parse().unwrap()))
            } else if str_regex().is_match(&token) {
                Ok(Value::Str(unescape_str(&token[1..token.len() - 1])))
            } else if token.starts_with("\"") {
                error("expected '\"', got EOF")
            } else if token.starts_with("\\") {
                if token.len() == 2 {
                    Ok(Value::Char(token.chars().nth(1).unwrap() as char))
                } else {
                    error(&format!("Unsupported character: {}", token))
                }
            } else if token.starts_with(":") {
                Ok(Value::Keyword(key!(&token[1..].to_string())))
            } else {
                Ok(Value::Symbol(sym!(&token.to_string())))
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
            Ok(t) => t,
            Err(_) => {
                let msg = format!("Expected {} got EOF", end);
                let mut err = crate::errors::Error::new_from_str(msg);
                // err.add_info("at", format!("line {} col {}", r.line, r.col));
                return Err(values::LispErr::Error(err));
            }
        };
        if token == end {
            break;
        }
        seq.push(read_form(r)?);
    }
    let _ = r.next();
    match coll_type {
        CollType::List => Ok(list_from_vec(seq)),
        CollType::Vector => Ok(vector_from_vec(seq)),
        CollType::HashMap => Ok(hash_map_from_vec(seq).unwrap()),
        CollType::Set => Ok(set_from_vec(seq)),
    }
}

fn read_form(r: &mut Reader) -> ValueRes {
    let token = r.peek()?;
    match &token[..] {
        "'" => {
            let _ = r.next();
            Ok(list![Value::Symbol(sym!("quote")), read_form(r)?])
        }
        "#" => {
            let _ = r.next();
            let next_token = r.peek()?;
            match &next_token[..] {
                // Call var with #'sym
                "'" => {
                    let _ = r.next();
                    Ok(list![Value::Symbol(sym!("var")), read_form(r)?])
                }
                // Create a set with #{..}
                "{" => read_seq(r, "}", CollType::Set),
                _ => return error!(format!("Wrong token found: {}", next_token)),
            }
        }
        "`" => {
            let _ = r.next();
            Ok(list![Value::Symbol(sym!("quasiquote")), read_form(r)?])
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
        ")" => error("unexpected ')'"),
        "(" => read_seq(r, ")", CollType::List),
        "]" => error("unexpected ']'"),
        "[" => read_seq(r, "]", CollType::Vector),
        "}" => error("unexpected '}'"),
        "{" => read_seq(r, "}", CollType::HashMap),
        _ => read_atom(r),
    }
}

pub fn read_str(s: String) -> ValueRes {
    let tokens = tokenize(&s);

    let mut reader = Reader { tokens, pos: 0 };
    read_form(&mut reader)
}
