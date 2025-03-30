use std::error::Error;
use std::io::Write;
use std::{fs, io};

use lexer::Lexer;
use parser::Parser;
use visitors::interpreter::Interpreter;

mod env;
mod errors;
mod lexer;
mod parser;
mod visitors;

fn run(source: String, interpreter: &mut Interpreter) -> Result<(), Box<dyn Error>> {
    let tokens = Lexer::new(&source).scan()?;
    let program = Parser::new(tokens).parse()?;

    interpreter.interpret(program)?;
    Ok(())
}

pub fn run_file(fname: String) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(&fname)?;

    let mut interpreter = Interpreter::new();
    run(contents, &mut interpreter)?;
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
                let result = run(source, &mut interpreter);
                if let Err(x) = result {
                    eprintln!("{x}");
                }
            }
        }
    }
    Ok(())
}
