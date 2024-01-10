mod lexer;
mod parser;
//mod ast_interpreter;

fn main() {
    println!("Hello, world!");

    let file_name = 'vall: {
        for arg in std::env::args().skip(1) {
            println!("arg: {}", arg);
            break 'vall arg;
        }
        "code.dode".to_string()
    };

    assert!(file_name.ends_with(".dode"));

    let content: &'static mut str = std::fs::read_to_string(file_name).expect("file does not exist!").leak();

    println!(" \n--- STARTING LEXING!");
    let (tokens, token_idx_to_char_nr) = lexer::generate_tokens(content);
    println!("tokens: {:?}", tokens);

    println!(" \n--- STARTING PARSING!");
    let ast = parser::parse(&tokens, &token_idx_to_char_nr, content);
    println!("\n FINAL AST: {:?}", ast);

    println!("\n tree version: \n");
    parser::print_ast(&ast);

    println!("\n Correctness of AST pass: \n");
    //parser::check_that_ast_is_correct(&ast);

    println!("\n Now interpreting ast: \n");

    //ast_interpreter::run_ast(&ast);
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
