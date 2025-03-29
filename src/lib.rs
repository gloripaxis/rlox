use std::error::Error;
use std::io::Write;
use std::{fs, io};

use lexer::Lexer;
use parser::Parser;
use visitors::interpreter;

mod errors;
mod lexer;
mod parser;
mod visitors;

fn run(source: String) -> Result<(), Box<dyn Error>> {
    let tokens = Lexer::new(&source).scan()?;
    let expression = Parser::new(tokens).parse()?;
    let value = interpreter::visit(expression)?;
    println!("{value}");
    Ok(())
}

pub fn run_file(fname: String) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(&fname)?;
    run(contents)?;
    Ok(())
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    println!("Welcome to Lox REPL! Press Ctrl+D to exit...");
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
                let result = run(source);
                if let Err(x) = result {
                    eprintln!("{x}");
                }
            }
        }
    }
    Ok(())
}
