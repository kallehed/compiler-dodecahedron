use parser::Soken;

// mod asm_backend;
mod ast_verify;
mod c_backend;
mod ir_gen;
mod lexer;
mod new_c_backend;
mod parser;

struct CompilerFlags {
    verbose: bool,
}
static mut FLAGS: CompilerFlags = CompilerFlags { verbose: false };

fn main() {
    println!("Hello, world!");

    // default name

    let mut file_name = "code.dode";

    // TODO: make compiler handle verbose flag

    // skip first, which just says name of compiler binary

    for arg in std::env::args().skip(1) {
        if arg.starts_with('-') {
            for flag in arg.chars().skip(1) {
                match flag {
                    'v' => {
                        unsafe { FLAGS.verbose = true };
                    }
                    nonexistant_flag => {
                        println!("ERROR: flag `{}` doesn't exist!", nonexistant_flag);
                        std::process::exit(1);
                    }
                }
            }
        } else {
            // it is a file name
            file_name = arg.leak();
        }
    }

    if !file_name.ends_with(".dode") {
        println!(
            "ERROR: file name must end in `.dode`! You supplied: `{}`",
            file_name
        );
        std::process::exit(1);
    }

    let source: &'static mut str = std::fs::read_to_string(file_name)
        .expect("file does not exist!")
        .leak();

    println!(" \n--- STARTING LEXING!");
    let (tokens, token_idx_to_char_range, mut ident_idx_to_string, mut int_stor) =
        lexer::generate_tokens(source, file_name);
    println!("tokens: {:?}", tokens);

    // add print_int ident and get it's ident idx
    let print_int_ident_idx =
        if let Some(ident) = ident_idx_to_string.iter().position(|&x| x == "print_int") {
            ident.try_into().unwrap()
        } else {
            let i = ident_idx_to_string.len();
            ident_idx_to_string.push("print_int");
            i.try_into().unwrap()
        };

    println!(" \n--- STARTING PARSING!");
    let (mut sokens, origins, mut functions) =
        parser::parse(&tokens, &token_idx_to_char_range, source, file_name);

    functions.insert(print_int_ident_idx, 1); // takes one argument

    println!("\n FINAL AST: {:?}", &sokens);

    println!("\n--- Type check AST: \n");
    ast_verify::run(
        &mut sokens,
        &origins,
        &ident_idx_to_string,
        &functions,
        source,
        file_name,
        &token_idx_to_char_range,
        &mut int_stor,
    );
    println!("After verifying: {:?}", &sokens);
    eprintln!("nbr of sokens: {}", sokens.len());
    let total_sok_before_filter = sokens.len();
    let nil_generated = sokens.iter().filter(|&&x| matches!(x, Soken::Nil)).count();
    let sokens: Vec<Soken> = sokens
        .into_iter()
        .filter(|&e| !matches!(e, Soken::Nil))
        .collect();
    println!("'\nAfter filtering Nil: {:?}", &sokens);
    println!(
        "\n Nil generated: {}, {:.1}% of sokens",
        nil_generated,
        (nil_generated as f64 / total_sok_before_filter as f64) * 100.0
    );

    {
        let c_code = c_backend::to_c_code(&sokens, &ident_idx_to_string, &functions, &int_stor);

        println!("\n--- Now printing C code: \n");
        println!("{}", c_code);

        {
            use std::io::Write;
            let mut file = std::fs::File::create("out.c").unwrap();
            file.write_all(c_code.as_bytes()).unwrap();
        }
    }
    // generate IR
    {
        let (ir, ir_functions, ident_to_func_idx) = ir_gen::get_ir(&sokens, &functions);
        println!("\n\n IR: {:?}", ir);

        // generate c from IR
        {
            let c_code = new_c_backend::gen_c(
                &ir,
                &ir_functions,
                &int_stor,
                ident_to_func_idx,
                print_int_ident_idx,
                &ident_idx_to_string,
            );

            println!("\n\n new C code: {:?}", c_code);
            {
                use std::io::Write;
                let mut file = std::fs::File::create("out.c").unwrap();
                file.write_all(c_code.as_bytes()).unwrap();
            }
        }
    }

    // generate assembly (NASM)
    /*{
    let asm = asm_backend::to_asm(&ast, &ident_idx_to_string);
    println!("\n--- Now printing ASM: \n");
    // println!("{}", asm);

    {
        use std::io::Write;
        let mut file = std::fs::File::create("out.asm").unwrap();
        file.write_all(asm.as_bytes()).unwrap();
    }
    }*/
}

const COMMENT_PREFIX: char = '#';

type IdentIdx = u16;

type Int = i64;

#[derive(Copy, Clone, Debug)]
pub enum Keyword {
    /// let
    CreateVar,
    If,
    Else,
    While,
    /// return 3;
    Return,
    /// {
    StartBlock,
    /// }
    EndBlock,
    /// += = -=
    Set(SetType),
    Equals,
    Plus,
    Minus,
    Multiply,
    Less,
    More,
    StartParen,
    EndParen,
    Comma,
    EndStatement,
    TypeIncoming,
    FunctionIncoming,
}

#[derive(Copy, Clone, Debug)]
pub enum SetType {
    Set,
    Add,
    Sub,
}

// TODO, better error reporting
/// marks an exclusive interval of source file with ^^^ and says `msg`.
fn mark_error_in_source(
    source: &str,
    file_name: &str,
    msg: &str,
    (start_wher, end_wher): (usize, usize),
) {
    let mut line = 1;
    let mut col = 1;
    let mut latest_start_of_newline = 0;
    for (idx, ch) in source.chars().enumerate() {
        if idx == start_wher {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
            latest_start_of_newline = idx + 1;
        } else {
            col += 1;
        }
    }
    // get string for line by iterating over chars again
    let line_str = {
        let mut end_of_line = 0;
        for (idx, ch) in source.chars().enumerate() {
            if idx > latest_start_of_newline && ch == '\n' {
                // found end of line
                end_of_line = idx;
                break;
            }
        }
        &source[latest_start_of_newline..end_of_line]
    };

    println!("\x1b[1m\x1b[31merror:\x1b[39m {}", msg);
    // file name, line and column
    println!("\x1b[34m  --> \x1b[0m {}:{}:{}", file_name, line, col);
    let start_string = format!("{} |", line);
    for _ in 0..start_string.len() - 1 {
        print!(" ");
    }
    println!("\x1b[1m\x1b[34m|");
    println!("{}\x1b[0m{}", start_string, line_str);
    // print arrows pointing to were the error is

    for at in 0..(col - 1 + start_string.len()) {
        print!(
            "{}",
            match at {
                a if a == start_string.len() - 1 => "\x1b[1m\x1b[34m|\x1b[0m",
                _ => " ",
            }
        );
    }
    print!("\x1b[1m\x1b[31m");
    for _ in 0..(end_wher - start_wher) {
        print!("^");
    }
    println!();
}
