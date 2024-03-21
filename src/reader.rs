use regex::{Captures, Regex};
use std::rc::Rc;
use std::sync::OnceLock;

use crate::keyword::Keyword;
use crate::symbol::Symbol;
use crate::utils::{self, str_first};
use crate::values::LispErr::ErrString;
use crate::values::{
    self, error, hash_map_from_kv, hash_map_from_vec, list_from_vec, set_from_vec, vector_from_vec,
    LispErr, ToValue, Value, ValueRes,
};

struct Token {
    tok: String,
    row: usize,
    col: usize,
}

pub struct Reader {
    // Using a vec of a vec of tokens, so every sexpr is a vector of tokens.
    tokens: Vec<Vec<Token>>,
    pos: usize,  // position counter inside the sexpr
    form: usize, // position counter of the current form
}

impl Reader {
    fn next(&mut self) -> Result<&Token, LispErr> {
        if self.pos >= self.tokens[self.form].len() {
            self.pos = 0;
            self.form += 1;
        }

        self.pos += 1;
        let token = self.peek()?;
        Ok(token)
    }

    fn peek(&self) -> Result<&Token, LispErr> {
        match self.tokens[self.form].get(self.pos - 1) {
            Some(t) => return Ok(t),
            None => return Err(ErrString("underflow".to_string())),
        };
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

pub fn tokenize(s: &str, multi_form: bool) -> Reader {
    let mut temp_s = s.to_string();
    let mut tokens = vec![];
    let mut col: usize = 0;
    let mut row: usize = 0;
    let mut tot_len = 0;

    // Looping here since `captures_iter` currently only matches
    // the first occurrence of a sexpr, so we keep interating
    // on the same string if multi-form is enabled.
    loop {
        let mut tok_v = vec![];
        let mut last_match_end = 0;

        for cap in tokens_regex().captures_iter(temp_s.as_str()) {
            let tok_str = &cap[1];
            if tok_str.starts_with(";") {
                continue; //  comment form
            }

            // Count newlines if it's not a string to get the current row number
            if str_first(tok_str) != "\"" {
                let nl_count = utils::count_char_occurrences(&cap[0], '\n');
                row += nl_count;
            }
            // Count both spaces and characters
            col += &cap[0].replace("\n", "").len();

            let tok = Token {
                tok: String::from(tok_str),
                row,
                col,
            };
            tok_v.push(tok);
            // Save the capture end
            last_match_end = cap.get(0).unwrap().end();
        }

        tot_len += last_match_end;

        if tot_len >= s.len() || multi_form == false {
            // if we finish iterating on the string, or multiform is not enabled
            break;
        } else {
            tokens.push(tok_v);
            // Reduce the current string when starting from the total
            temp_s = temp_s[tot_len..].to_string();
        }
    }

    Reader {
        tokens,
        pos: 0,
        form: 0,
    }
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
    match token.tok.as_str() {
        "nil" => Ok(Value::Nil),
        "false" => Ok(Value::Bool(false)),
        "true" => Ok(Value::Bool(true)),
        _ => {
            let token = token.tok.as_str();
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
            Ok(t) => t.tok.clone(),
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
    match token.tok.as_str() {
        "'" => {
            let _ = r.next();
            Ok(list![Value::Symbol(sym!("quote")), read_form(r)?])
        }
        "#" => {
            let _ = r.next();
            let next_token = r.peek()?;
            match next_token.tok.as_str() {
                // Call var with #'sym
                "'" => {
                    let _ = r.next();
                    Ok(list![Value::Symbol(sym!("var")), read_form(r)?])
                }
                // Create a set with #{..}
                "{" => read_seq(r, "}", CollType::Set),
                _ => return error!(format!("Wrong token found: {}", next_token.tok)),
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

pub fn read_str(s: String, multi_form: bool) -> ValueRes {
    let mut reader = tokenize(&s, multi_form);
    read_form(&mut reader)
}
