mod lexer;
mod parser;
mod semantic_checker;
mod bytecodegen;
mod operator;
mod cli;
mod diagnostic;

use std::rc::Rc;

use crate::{bytecodegen::BytecodeGenerator, cli::Cli};
use colored::Colorize;
use amai_vm::{AmaiVM, Chunk, Function};

type Span = std::ops::Range<usize>;

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

    let tokens = lexer::tokenize(&contents);

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
                    .collect::<Vec<_>>().join("\n\n")
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
                .collect::<Vec<_>>().join("\n\n")
        }
    )?;

    /*
        let mut bcg = BytecodeGenerator::new(ast);
        let bytes = bcg.generate();

        for byte in &bytes {
            println!("{:#010X}", byte);
        }
        println!("{:#?}", &bcg.constants[0..10]);

        let mut vm = AmaiVM::init();
        vm.functions.push(Rc::new(Function {
            chunk: Chunk {
                code: bytes,
                constants: bcg.constants.try_into().unwrap(),
                lines: vec![]
            },
            arity: 0,
            locals: 0,
        }));
        vm.call_function_from_idx(0)?;

        vm.run()?;
    */

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