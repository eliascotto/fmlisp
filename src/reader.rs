use regex::{Captures, Regex};
use std::fs::read_to_string;
use std::rc::Rc;

use crate::env::Environment;
use crate::keyword::Keyword;
use crate::symbol::Symbol;
use crate::values::LispError::ErrString;
use crate::values::{
    error, hash_map_from_kv, hash_map_from_vec, list_from_vec, set_from_vec, vector_from_vec,
    LispError, ToValue, Value, ValueRes,
};

struct Reader {
    tokens: Vec<String>,
    pos: usize,
}

impl Reader {
    fn next(&mut self) -> Result<String, LispError> {
        self.pos += 1;
        Ok(self
            .tokens
            .get(self.pos - 1)
            // Ok keep processing, otherwise propagate the ErrString back to the caller
            .ok_or(ErrString("underflow".to_string()))?
            .to_string())
    }

    fn peek(&self) -> Result<String, LispError> {
        Ok(self
            .tokens
            .get(self.pos)
            // Ok keep processing, otherwise propagate the ErrString back to the caller
            .ok_or(ErrString("underflow".to_string()))?
            .to_string())
    }
}

pub fn tokenize(s: &str) -> Vec<String> {
    let re: Regex =
        Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"###)
            .unwrap();

    let mut res = vec![];
    for cap in re.captures_iter(s) {
        if cap[1].starts_with(";") {
            continue;
        }
        res.push(String::from(&cap[1]));
    }
    res
}

fn unescape_str(s: &str) -> String {
    let re: Regex = Regex::new(r#"\\(.)"#).unwrap();
    re.replace_all(&s, |caps: &Captures| {
        format!("{}", if &caps[1] == "n" { "\n" } else { &caps[1] })
    })
    .to_string()
}

fn read_atom(r: &mut Reader) -> ValueRes {
    let int_re: Regex = Regex::new(r"^-?[0-9]+$").unwrap();
    let float_re: Regex = Regex::new(r"^-?\d+(\.\d+)?$").unwrap();
    let str_re: Regex = Regex::new(r#""(?:\\.|[^\\"])*""#).unwrap();

    let token = r.next()?;

    // Using &token[..] to match against a reference to the string
    match &token[..] {
        "nil" => Ok(Value::Nil),
        "false" => Ok(Value::Bool(false)),
        "true" => Ok(Value::Bool(true)),
        _ => {
            if int_re.is_match(&token) {
                Ok(Value::Integer(token.parse().unwrap()))
            } else if float_re.is_match(&token) {
                Ok(Value::Float(token.parse().unwrap()))
            } else if str_re.is_match(&token) {
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
    r.next()?; // remove the seq start

    loop {
        let token: String = match r.peek() {
            Ok(t) => t,
            Err(_) => return error(&format!("Expected {} got EOF", end)),
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
