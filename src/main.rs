mod scanner;

use crate::parser::Parser;
use crate::scanner::Scanner;
use std::env;
use std::fs::File;
use std::io::{self, Read, Write};

mod parser;

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

    let mut scanner = Scanner::new(str_content);
    let tokens = scanner.scanning();

    print!("{:#?}", tokens);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{:#?}", ast);

    Ok(())
}

fn run_prompt() -> io::Result<()> {
    let stdin = io::stdin();
    let mut buffer = String::new();

    loop {
        print!("> ");
        let _ = io::stdout().flush();
        buffer.clear();

        stdin.read_line(&mut buffer)?;

        if buffer.trim().len() == 0 {
            break;
        }

        if buffer.trim() == "exit" {
            break;
        }
    }

    Ok(())
}
