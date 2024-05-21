use std::collections::VecDeque;

#[derive(Debug)]
pub enum ArgumentParserError {
    NotEnoughArguments,
    UnexpectedEndOfArguments,
    Conflict,
}

pub struct Arguments {
    pub help: bool,

    pub clean: bool,
    pub clean_all: bool,

    pub gen_c: bool,
    pub gen_asm: bool,

    pub show_tokens: bool,
    pub show_ast: bool,

    pub output: Option<String>,
    pub input: Option<String>,
}

pub fn parse_args(args: VecDeque<String>) -> Result<Arguments, ArgumentParserError> {
    if args.is_empty() {
        return Err(ArgumentParserError::NotEnoughArguments);
    }
    let mut idx = 0;
    let mut arguments = Arguments {
        help: false,

        clean: false,
        clean_all: false,

        gen_c: false,
        gen_asm: false,

        show_tokens: false,
        show_ast: false,

        output: None,
        input: None,
    };

    while idx < args.len() {
        match args[idx].as_str() {
            "help" => {
                idx += 1;
                arguments.help = true;
            }

            "clean" => {
                idx += 1;
                arguments.clean = true;
                if args.get(idx) == Some(&"all".to_string()) {
                    arguments.clean_all = true;
                    idx += 1;
                    continue;
                }
            }

            "-gen-asm" => arguments.gen_asm = true,
            "-gen-c" => arguments.gen_c = true,

            "-tokens" => arguments.show_tokens = true,
            "-ast" => arguments.show_ast = true,

            "-o" | "--output" => {
                idx += 1;
                if let Some(path) = args.get(idx) {
                    arguments.output = Some(path.to_string());
                    idx += 1;
                    continue;
                }
                return Err(ArgumentParserError::UnexpectedEndOfArguments);
            }
            "-i" | "--input" => {
                idx += 1;
                if let Some(path) = args.get(idx) {
                    arguments.input = Some(path.to_string());
                    idx += 1;
                    continue;
                }
                return Err(ArgumentParserError::UnexpectedEndOfArguments);
            }

            input => arguments.input = Some(input.to_string()),
        }

        idx += 1;
    }

    if (arguments.clean || arguments.clean_all) && (arguments.gen_c || arguments.gen_asm) {
        eprintln!("Clean commands will clean after generating C or Asm files");
        return Err(ArgumentParserError::Conflict);
    }

    Ok(arguments)
}
