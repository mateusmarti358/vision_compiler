use std::collections::VecDeque;
use std::env::args;
use std::fs::{self, OpenOptions};
use std::path::Path;

use std::io::Write;

use lexer::Token;
use parser::Statement;
use util::args::{self, parse_args};

use crate::util::file::read_src;

mod util;

mod symbol_table;
mod types;

mod lexer;
mod parser;

mod ctranspiler;

#[derive(Debug)]
pub enum CompilationError {
    LexerError(lexer::LexerError),
    ParserError(parser::ParserError),
    ArgumentsError(args::ArgumentParserError),
}

fn show_help() {
    println!("Usage:");
    println!("vsc (-i | --input) <input-file>.v (-o | --output <output-file>)");
    println!("\nOptions: ");
    println!("-gen-asm    ;; Generate Assembly file");
    println!("-gen-c      // Generate C file");
    println!("-tokens     // Show tokens");
    println!("-ast        // Show AST (Abstract Syntax Tree)");
    println!("clean (all) // clean C files, Object files and Assembly files");
}

fn clean(all: bool) {
    let entries = fs::read_dir("out/").unwrap();
    for entry in entries {
        let entry = entry.unwrap();
        let file_path = entry.path();

        if let Some(extension) = file_path.extension() {
            if ["s", "o", "c"].contains(&extension.to_str().unwrap()) || all {
                fs::remove_file(&file_path).unwrap();
                println!("Deleted file: {}", file_path.to_str().unwrap());
            }
        }
    }
}

fn generate_c(
    input: Option<String>,
    output: Option<String>,
) -> Result<(Vec<Token>, Vec<Statement>), CompilationError> {
    let input_path = match &input {
        Some(input) => Path::new(input),
        None => Path::new("main.v"),
    };
    let (tokens, ast, transpiled) = ctranspiler::transpile_from_src(input_path)?;

    {
        let mut out_file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(format!(
                "./out/{}.c",
                match output {
                    Some(output) => output,
                    None => input_path
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap()
                        .to_string(),
                }
            ))
            .unwrap();

        out_file
            .write_all(transpiled.as_bytes())
            .expect("Unable to write to file");
    }

    return Ok((tokens, ast));
}

fn main() -> Result<(), CompilationError> {
    let mut args = args().collect::<VecDeque<String>>();
    args.pop_front();

    let options = match parse_args(args) {
        Ok(options) => options,
        Err(err) => return Err(CompilationError::ArgumentsError(err)),
    };

    if options.help {
        show_help();
    }

    if options.gen_c {
        let (tokens, ast) = generate_c(options.input, options.output)?;

        if options.show_tokens {
            eprintln!("Tokens: {:#?}", tokens);
        }
        if options.show_ast {
            eprintln!("AST: {:#?}", ast);
        }
    }

    if options.clean {
        clean(options.clean_all);
    }

    Ok(())
}
