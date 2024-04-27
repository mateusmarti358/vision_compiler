use std::{ fs::{self, OpenOptions}, process::Command };
use std::env::args;

use std::io::Write;

use bytecode::interpreter;

use crate::util::file::{read_bytes, read_src};

mod util;

mod types;
mod symbol_table;

mod lexer;
mod parser;

mod bytecode;
mod ctranspiler;

#[derive(Debug)]
pub enum CompilationError {
  LexerError(lexer::LexerError),
  ParserError(parser::ParserError)
}

fn main() -> Result<(), CompilationError> {
  let args = args().collect::<Vec<String>>();

  if args.len() < 2 {
    panic!("Not enough arguments");
  }

  if args[1] == "clean" {
    let entries = fs::read_dir("out/").unwrap();

    // Iterate over the entries
    for entry in entries {
      let entry = entry.unwrap();
      let file_path = entry.path();

      if let Some(extension) = file_path.extension() {
        if ["log", "cpp", "exe"].contains(&extension.to_str().unwrap()) {
          fs::remove_file(&file_path).unwrap();
          println!("Deleted file: {:?}", file_path);
        }
      }
    }

    return Ok(())
  }

  if args[1] == "C" {
    let transpiled = ctranspiler::transpile_from_src(args[2].clone(), true)?;

    let out_filename = args[2].split("/").last().unwrap()
                                      .split(".").next().unwrap().to_string();

    {
      let mut out_file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(format!("./out/{}.cpp", out_filename))
        .unwrap();

      out_file.write_all(transpiled.as_bytes()).expect("Unable to write to file");
    }

    Command::new("g++")
      .arg(format!("./out/{}.cpp", out_filename))
      .arg("-o")
      .arg(format!("./out/{}", out_filename))
      .stderr(std::process::Stdio::inherit())
      .output()
      .expect("failed to execute process");

    if args.contains(&"run".to_string()) {
      Command::new(format!("./out/{}", out_filename))
        .output()
        .expect("failed to execute process");
    }

    if args.contains(&"clean".to_string()) {
      fs::remove_file(format!("./out/{}.cpp", out_filename)).unwrap();
    }

    return Ok(())
  }

  if args[1] == "bytecode" {
    /*let bytecode = bytecode_emmiter::make_bytecode_from_src(&args[2])?;

    let out_filename = args[2].split("/").last().unwrap()
                                      .split(".").next().unwrap().to_string();

    let mut out_file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(format!("./out/{}.vbit", out_filename))
        .unwrap();

    return Ok(())*/
  }

  if args[1] == "vbit" {
    let bytecode = todo!(); 
  }

  if args[1] == "vbit-from-vit" {
    let bytecode = match bytecode::executable_from_vit(read_src(&args[2])) {
      Some(bytecode) => bytecode,
      None => panic!("Failed to parse intermediate code"),
    };

    let out_filename = args[2].split("/").last().unwrap()
                                      .split(".").next().unwrap().to_string();

    let mut out_file = OpenOptions::new()
      .write(true)
      .truncate(true)
      .create(true)
      .open(format!("./out/{}.vbit", out_filename))
      .unwrap();

    out_file.write_all(&bytecode).expect("Unable to write to file");

    eprintln!("\x1b[0;32mBytecode successfully created!\x1b[0;0m");
  
    return Ok(())
  }

  if args[1] == "interpret-vbit" {
    let timer = std::time::Instant::now();
    interpreter::interpret(read_bytes(args[2].clone()));
    println!("Time taken: {}ms", timer.elapsed().as_secs_f64() * 1000.0);
  }

  Ok(())
}