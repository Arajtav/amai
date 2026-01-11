mod lexer;
mod parser;
mod semantic_checker;
mod vm;
mod common;
mod cli;
mod diagnostic;

use crate::cli::Cli;
use colored::Colorize;
use vm::{AmaiVM, value::Value, inst::*};

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
        eprintln!("Tokens: [");
        for tok in &tokens {
            eprintln!("\t{},", tok.fmt_span());
        }
        eprintln!("]");
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
        }
    )?;

    let constants = [Value::from_int(5), Value::from_int(3)];
    let mut vm = AmaiVM::new(&constants);

    let bytecode = [
        LOAD, 0, 0, 0,
        LOAD, 1, 1, 0,
        IADD, 2, 0, 1,
        HALT,
    ];
    vm.call_function(&bytecode, 2);
    vm.run()
        .map_err(|err|
            format!("{}: {err}", "error".bright_red().bold())
        )?;

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