use std::error::Error;
use std::io::Write;
use std::{fmt, fs, io};

use lexer::Lexer;

mod lexer;

#[derive(Debug, Clone)]
pub struct LoxError {
    message: String,
}

impl LoxError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

impl Error for LoxError {}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parsing error: {}", self.message)
    }
}

fn run(source: String) -> Result<(), Box<dyn Error>> {
    let result = Lexer::new(&source).scan()?;
    for tok in result.iter() {
        println!("{tok}");
    }
    Ok(())
}

pub fn run_file(fname: String) -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string(&fname)?;
    run(contents)?;
    Ok(())
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    loop {
        print!("> ");
        io::stdout().flush()?;
        
        let mut source = String::new();
        let bytes = io::stdin().read_line(&mut source)?;
        match bytes {
            0 => {
                println!("Ctrl+D\nTerminating rlox...");
                break;
            },
            // TODO: Add error handling - an error in the interactive loop shouldn't kill the session
            _ => run(source)?  
        }
    }
    Ok(())
}
