mod c_backend;
mod lexer;
mod parser;
mod type_check_ast;
//mod ast_interpreter;

fn main() {
    println!("Hello, world!");

    let file_name = {
        let arg = std::env::args().nth(1).unwrap_or("code.dode".to_string());
        println!("arg: {}", arg);
        arg
    };

    assert!(file_name.ends_with(".dode"));

    let content: &'static mut str = std::fs::read_to_string(&file_name)
        .expect("file does not exist!")
        .leak();

    println!(" \n--- STARTING LEXING!");
    let (tokens, token_idx_to_char_range, ident_idx_to_string) = lexer::generate_tokens(content);
    println!("tokens: {:?}", tokens);

    println!(" \n--- STARTING PARSING!");
    let ast = parser::parse(&tokens, &token_idx_to_char_range, content, file_name.as_str());
    println!("\n FINAL AST: {:?}", ast);

    println!("\n--- tree version: \n");
    parser::print_ast(&ast);

    println!("\n--- Correctness of AST pass: \n");
    //parser::check_that_ast_is_correct(&ast);

    println!("\n--- Type check AST: \n");

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

type IdentifierIdx = u16;

type Int = i64;

#[derive(Copy, Clone, Debug)]
pub enum Keyword {
    CreateVar,
    If,
    While,
    Type(Type),
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    // Integer
    Whole,
    // Float 64
    Real,
    String,
    Unit,
}

#[derive(Copy, Clone, Debug)]
pub enum SetType {
    Set,
    Add,
    Sub,
}
