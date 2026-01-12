mod lexer;
mod parser;
mod semantic_checker;
mod vm;
mod common;
mod cli;
mod diagnostic;

use crate::cli::Cli;
use colored::Colorize;

fn main() {
    use clap::Parser;

    let cli = Cli::parse();
    if let Err(err) = run_cli(cli) {
        println!("{err}");
    }
}

pub fn run_cli(cli: Cli) -> Result<(), String> {
    use std::fs;

    let contents = fs::read_to_string(&cli.input)
        .map_err(|_| format!("{}: No such file: {}", "error".bright_red().bold(), cli.input))?
        .replace("\r\n", "\n");

    let tokens = match lexer::lex(&cli.input, &contents) {
        Ok(toks) => toks,
        Err(err) => {
            let lines = contents.lines().collect::<Vec<_>>();
            let line_starts = line_starts(&contents);
            return Err(err.display(&line_starts, &lines));
        },
    };

    if cli.debug {
        let debug_toks = tokens
            .iter()
            .map(|tok| tok.fmt_span())
            .collect::<Vec<_>>();
        eprintln!("Tokens: {debug_toks:#?}");
    }

    let mut parser = parser::Parser::new(&cli.input, &tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            let lines = contents.lines().collect::<Vec<_>>();
            let line_starts = line_starts(&contents);
            return Err(
                err
                    .iter()
                    .map(|d| d.display(&line_starts, &lines))
                    .collect::<Vec<_>>().join("\n")
            );
        },
    };

    if cli.debug {
        eprintln!("AST: {:#?}", ast.nodes);
    }

    let mut sch = semantic_checker::SemanticChecker::new(&ast);

    sch.validate().map_err(|errors| {
        let lines = contents.lines().collect::<Vec<_>>();
        let line_starts = line_starts(&contents);
        errors
            .iter()
            .map(|d| d.display(&line_starts, &lines))
            .collect::<Vec<_>>().join("\n")
    })?;

    Ok(())
}

fn line_starts(s: &str) -> Vec<usize> {
    let mut indices = vec![0];
    
    for (i, ch) in s.char_indices() {
        if ch == '\n' {
            indices.push(i + 1);
        }
    }

    indices
}

#[cfg(test)]
mod tests {
    use crate::vm::{AmaiVM, value::Value, inst::*};

    #[test]
    fn benchmark() {
        use std::time::Instant;
        let constants = [Value::from_int(5), Value::from_int(3)];
        let bytecode = [
            LOAD, 0, 0, 0,
            LOAD, 1, 1, 0,
            IADD, 2, 0, 1,
            HALT, 0, 0, 0,
        ];
        
        let mut vm = AmaiVM::new(&constants);
        vm.add_function(&bytecode, 2);
        vm.call_function(0);
        let start = Instant::now();
        for _ in 0..1_000_000 {
            vm.run().expect("Runtime error");
            vm.frames.last_mut().unwrap().ip = vm.frames.last_mut().unwrap().function.bytecode.as_ptr();
        }
        let elapsed = start.elapsed();
        println!("AmaiVM: 1M iterations: {:?} ({:?} per iteration)", 
                elapsed, elapsed / 1_000_000);

        let start = Instant::now();
        for _ in 0..1_000_000 {
            let _result = 5 + 3;
        }
        let elapsed = start.elapsed();
        println!("Rust: 1M iterations: {:?} ({:?} per iteration)", 
                elapsed, elapsed / 1_000_000);
    }

    #[test]
    fn zdiv() {
        let constants = [Value::from_int(5), Value::from_int(0)];
        let bytecode = [
            LOAD, 0, 0, 0,
            LOAD, 1, 1, 0,
            IDIV, 2, 0, 1,
            HALT, 0, 0, 0,
        ];
        
        let mut vm = AmaiVM::new(&constants);
        vm.add_function(&bytecode, 2);
        vm.call_function(0);
        let result = vm.run();
        assert_eq!(result, Err("Division by zero"));
    }
}