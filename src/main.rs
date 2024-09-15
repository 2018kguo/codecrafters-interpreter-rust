use anyhow::Result;
use std::env;
use std::fs;
use std::process::exit;

use crate::ast::stringify_literal;
mod ast;
mod environment;
mod parser;
mod scanner;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return Ok(());
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
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
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = scanner::Scanner::new(file_contents);
            let scan_result = scanner.scan_tokens()?;
            if scan_result.had_error {
                exit(65);
            }

            let mut parser = parser::Parser::new(scan_result.tokens);
            let expression = parser.parse_expression();

            if parser.had_error || expression.is_err() {
                exit(65);
            }

            let ast_printer = ast::AstPrinter::new();
            let ast_string = ast_printer.print(&expression.unwrap());
            println!("{}", ast_string);
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let mut scanner = scanner::Scanner::new(file_contents);
            let scan_result = scanner.scan_tokens()?;
            if scan_result.had_error {
                exit(65);
            }

            let mut parser = parser::Parser::new(scan_result.tokens);
            let expression = parser.parse_expression();

            if parser.had_error || expression.is_err() {
                exit(65);
            }

            let interpreter = ast::Interpreter::new();
            let result = interpreter.interpret_expression(expression.unwrap());

            match result {
                Ok(result_literal) => {
                    println!("{}", stringify_literal(&result_literal));
                }
                Err(err) => {
                    let runtime_error = err.downcast_ref::<ast::RuntimeError>().unwrap();
                    eprintln!(
                        "{}\n[line {}]",
                        runtime_error.message, runtime_error.token.line
                    );

                    exit(70);
                }
            }
        }
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
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

            let mut interpreter = ast::Interpreter::new();
            let result = interpreter.interpret(expression.unwrap());

            match result {
                Ok(_) => {}
                Err(err) => {
                    let runtime_error = err.downcast_ref::<ast::RuntimeError>().unwrap();
                    eprintln!(
                        "{}\n[line {}]",
                        runtime_error.message, runtime_error.token.line
                    );

                    exit(70);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command)
        }
    }
    Ok(())
}
