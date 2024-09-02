use anyhow::Result;
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;
mod ast;
mod parser;
mod scanner;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return Ok(());
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = scanner::Scanner::new(file_contents);
            let scan_result = scanner.scan_tokens()?;
            scanner.token_structure();
            if scan_result.had_error {
                exit(65);
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = scanner::Scanner::new(file_contents);
            let scan_result = scanner.scan_tokens()?;
            if scan_result.had_error {
                exit(65);
            }

            let mut parser = parser::Parser::new(scan_result.tokens);
            let expression = parser.parse();

            if parser.had_error || expression.is_err() {
                exit(65);
            }

            let ast_printer = ast::AstPrinter::new();
            let ast_string = ast_printer.print(&expression.unwrap());
            println!("{}", ast_string);
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut scanner = scanner::Scanner::new(file_contents);
            let scan_result = scanner.scan_tokens()?;
            if scan_result.had_error {
                exit(65);
            }

            let mut parser = parser::Parser::new(scan_result.tokens);
            let expression = parser.parse();
            
            if parser.had_error || expression.is_err() {
                exit(65);
            }

            let interpreter = ast::Interpreter::new();
            let result = interpreter.interpret(expression.unwrap());
            
            match result {
                Ok(res) => {
                    println!("{}", res);
                },
                Err(err) => {
                    let runtime_error = err.downcast_ref::<ast::RuntimeError>().unwrap();
                    eprintln!(
                        "{}\n[line {}]", runtime_error.message, runtime_error.token.line.to_string());
                    
                    exit(70);
                }
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
        }
    }
    Ok(())
}
