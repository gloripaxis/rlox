use std::error::Error;
use std::io::Write;
use std::{fs, io, process};

use compile::resolver::Resolver;
use compile::{interpreter::Interpreter, lexer::Lexer, parser::Parser};
use errors::LoxError;

mod builtins;
mod compile;
mod errors;
mod types;
mod visitors;

fn run(source: &str, interpreter: &mut Interpreter) -> Result<(), Vec<LoxError>> {
    let tokens = Lexer::new(source).scan()?;
    let program = Parser::new(tokens).parse()?;

    Resolver::new(interpreter).resolve(&program)?;
    interpreter.interpret(program)?;
    Ok(())
}

pub fn run_file(fname: String) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(&fname)?;

    let mut interpreter = Interpreter::new();
    if let Err(errors) = run(&contents, &mut interpreter) {
        for e in errors.iter() {
            eprintln!("{e}");
        }
        process::exit(1);
    }
    Ok(())
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    println!("Welcome to Lox REPL! Press Ctrl+D to exit...");

    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut source = String::new();
        let bytes = io::stdin().read_line(&mut source)?;
        match bytes {
            0 => {
                println!("Ctrl+D\nTerminating rlox...");
                break;
            }
            _ => {
                if source == "\n" {
                    continue;
                }
                let result = run(source.trim(), &mut interpreter);
                if let Err(x) = result {
                    for e in x.iter() {
                        eprintln!("{e}");
                    }
                }
            }
        }
    }
    Ok(())
}
