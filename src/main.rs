use std::process;

use clap::{Command, Arg};
use rlox::{run_file, run_prompt};

fn main() {
    let fname = get_filename();
    if let Err(e) = match fname {
        None => run_prompt(),
        Some(x) => run_file(x),
    } {
        eprintln!("ERROR: {e}");
        process::exit(1);
    }
}

fn get_filename() -> Option<String> {
    let matches = Command::new("rlox")
        .version("0.1.0")
        .about("Lox compiler/interpreter written in Rust")
        .arg(Arg::new("filename").required(false))
        .get_matches();

    matches.get_one::<String>("filename").cloned()
}

