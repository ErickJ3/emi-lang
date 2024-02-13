use std::env;
use std::fs::File;
use std::io::{self, Read, Write};

mod interpreter;
mod parser;
mod scanner;

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::scanner::Scanner;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("usage: emi [script]");
        std::process::exit(64);
    } else if args.len() == 2 {
        run_file(&args[1])?;
    } else {
        run_prompt()?;
    }

    Ok(())
}

fn run_file(file_path: &str) -> io::Result<()> {
    let mut file = File::open(file_path)?;
    let mut str_content = String::new();

    file.read_to_string(&mut str_content)?;

    println!("code: {:?}", &str_content);

    let mut scanner = Scanner::new(&str_content);
    let tokens = scanner.scanning();

    println!("tokens: {:#?}", tokens);

    let mut parser = Parser::new(&tokens);

    let mut interpreter = Interpreter::new();

    let expr = parser.parse().unwrap();

    println!("{:#?}", expr);


    let eval = interpreter.evaluate(expr);

    println!("{:?}", eval);
    Ok(())
}

fn run_prompt() -> io::Result<()> {
    let stdin = io::stdin();
    let mut buffer = String::new();

    loop {
        print!("> ");
        let _ = io::stdout().flush();

        stdin.read_line(&mut buffer)?;

        if buffer.trim().len() == 0 {
            break;
        }

        if buffer.trim() == "exit" {
            break;
        }

        buffer.clear();
    }

    Ok(())
}
