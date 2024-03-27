use std::io::stdin;
use termion::event::Key;
use termion::input::TermRead;

use crate::errors::Error;
use crate::values::{self, LispErr};

fn get_etype(err: &Error) -> String {
    if err.has_type() {
        err.get_type()
    } else {
        String::from("Execution error")
    }
}

fn get_edesc(err: &Error, path: &str) -> String {
    let error_w_type = get_etype(&err);

    if let Some(details) = err.details() {
        format!("{} {} at {}", error_w_type, details, path)
    } else if let Some(at) = err.info.get(&String::from("at")) {
        format!("{} at {} {}", error_w_type, path, at)
    } else {
        format!("{} at {}", error_w_type, path)
    }
}

fn print_backtrace(err: &Error) {
    eprintln!("\nBacktrace:\n{}", err.fmt_trace(true))
}

fn print_options(err: &Error) {
    println!("Actions:");
    println!("\t0: [ABORT] Return to top level");
    println!("\t1: [QUIT] Quit FMLisp");
    if err.has_trace() {
        println!("\t2: [BACKTRACE] Print backtrace");
    }
}

/// Utiliy to format error based on Error data.
/// Returns 0 if exit normally, or -1 if quit program is requested.
pub fn eprint(e: LispErr, path: &str) -> i8 {
    match e.clone() {
        LispErr::Error(err) => {
            let edesc = get_edesc(&err, path);
            let stdin = stdin();

            eprintln!("{}\n{}\n", edesc, values::format_error(e));
            print_options(&err);

            for c in stdin.keys() {
                // EOF automatically quit the loop
                match c.unwrap() {
                    Key::Char('q') => return 0,
                    Key::Char('0') => return 0,
                    Key::Char('1') => return -1,
                    Key::Char('2') if err.has_trace() => {
                        print_backtrace(&err);
                        break;
                    }
                    _ => {}
                }
            }
        }
        _ => eprintln!("Error at {}\n{}", path, values::format_error(e)),
    }
    return 0;
}
