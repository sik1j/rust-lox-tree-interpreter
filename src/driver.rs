use crate::scanner::Scanner;
use std::io::{BufRead, Write};
use std::{env, fs};
use crate::parser::Parser;

pub struct Driver {}

impl Driver {
    pub fn new() -> Self {
        Self {}
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
        let had_error = self.run(source);

        if had_error {
            std::process::exit(65);
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

    fn run(&mut self, source: String) -> bool {
        let mut scanner = Scanner::new(source);
        let (tokens, errors) = scanner.scan_tokens();

        if !errors.is_empty() {
            return false
        }

        let mut parser;
        parser = Parser::new(tokens);
        dbg!(parser.parse());

        true
    }
}
