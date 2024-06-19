use crate::scanner::Scanner;
use std::io::{BufRead, Write};
use std::{env, fs};
use crate::interpreter::Interpreter;
use crate::parser::{Expression, Parser};

pub struct Driver<'a> {
    interpreter: Interpreter<'a>,
}

type ExitCode = i32;
impl Driver<'_> {
    pub fn new() -> Self {
        Self {
            interpreter: Interpreter::new(),
        }
    }

    pub fn main_loop(&mut self) {
        let args: Vec<String> = env::args().collect();

        match args.len() {
            2 => self.run_file(&args[1]),
            1 => self.run_prompt(),
            _ => eprintln!("Usage: rlox [script]"),
        }
    }
    pub fn run_file(&mut self, file_path: &str) {
        let source = fs::read_to_string(file_path).expect("Could not open file");
        let exit_type = self.run(source);

        if exit_type != 0 {
            std::process::exit(exit_type);
        }
    }

    fn run_prompt(&mut self) {
        let stdin = std::io::stdin();
        let handle = stdin.lock();

        print!("> ");
        std::io::stdout().flush().expect("Failed to flush");
        for line in handle.lines() {
            self.run(line.expect("Failed to read line"));
            print!("> ");
            std::io::stdout().flush().expect("Failed to flush");
        }
    }

    fn run(&mut self, source: String) -> ExitCode {
        let mut scanner = Scanner::new(source);
        let (tokens, errors) = scanner.scan_tokens();

        if !errors.is_empty() {
            for error in errors {
                println!("{:?}", error);
            }
            return 65
        }

        let mut parser;
        parser = Parser::new(tokens);
        let statements = parser.parse();

        self.interpreter.interpret(statements);


        0
    }
}
