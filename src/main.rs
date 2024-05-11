mod c_backend;
mod lexer;
mod parser;
mod type_check_ast;

fn main() {
    println!("Hello, world!");

    let file_name = {
        let arg = std::env::args().nth(1).unwrap_or("code.dode".to_string());
        println!("arg: {}", arg);
        arg
    };

    assert!(file_name.ends_with(".dode"));

    let source: &'static mut str = std::fs::read_to_string(&file_name)
        .expect("file does not exist!")
        .leak();

    println!(" \n--- STARTING LEXING!");
    let (tokens, token_idx_to_char_range, ident_idx_to_string) = lexer::generate_tokens(source);
    println!("tokens: {:?}", tokens);

    println!(" \n--- STARTING PARSING!");
    let (ast, mut functions) = parser::parse(
        &tokens,
        &token_idx_to_char_range,
        source,
        file_name.as_str(),
    );

    // add print_int function to identifiers and to ident_to_name thing. 
    // TODO: fix having predefined functions by initially including them in the identifiers
    // and having a mapping from them to identifiers. Then also add them to the functions
    // hashmap, we could even define it before, and hand it to parser::parse, with predefined
    // functions, so the user can't redefine print_int
    {
        if let Some(ident) = ident_idx_to_string.iter().position(|&x| x == "print_int") {
            functions.insert(ident.try_into().unwrap(), 1); // takes one argument
        }
    }

    println!("\n FINAL AST: {:?}", ast);

    println!("\n--- tree version: \n");
    parser::print_ast(&ast);

    println!("\n--- Type check AST: \n");
    type_check_ast::type_check_ast(&ast, &ident_idx_to_string, &functions, source, file_name.as_str(), &token_idx_to_char_range);

    //ast_interpreter::run_ast(&ast);
    let c_code = c_backend::to_c_code(&ast, &ident_idx_to_string);

    println!("\n--- Now printing C code: \n");
    println!("{}", c_code);

    {
        use std::io::Write;
        let mut file = std::fs::File::create("out.c").unwrap();
        file.write_all(c_code.as_bytes()).unwrap();
    }
}

const STRING_DELIMITER: char = '"';
const COMMENT_PREFIX: char = '#';

type IdentIdx = u16;

type Int = i64;

#[derive(Copy, Clone, Debug)]
pub enum Keyword {
    CreateVar,
    If,
    While,
    StartBlock,
    EndBlock,
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

    println!("\n --- ERROR IN PARSING ---");
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
