mod lexer;
mod parser;
mod ast_interpreter;

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

    let content = std::fs::read_to_string(file_name).expect("file does not exist!");

    println!(" \n--- STARTING LEXING!");
    let tokens = lexer::generate_tokens(&content);
    println!("tokens: {:?}", tokens);

    println!(" \n--- STARTING PARSING!");
    let ast = parser::parse_block(&mut tokens.iter().peekable());
    println!("\n FINAL AST: {:?}", ast);

    println!("\n tree version: \n");
    parser::print_ast(&ast);

    println!("\n Correctness of AST pass: \n");
    parser::check_that_ast_is_correct(&ast);

    println!("\n Now interpreting ast: \n");

    ast_interpreter::run_ast(&ast);
}

const STRING_DELIMITER: char = '"';
const VARIABLE_PREFIX: char = '$';
const FUNCTION_PREFIX: char = '@';
const COMMENT_PREFIX: char = '#';

type VariableNameIdx = u8;
type FunctionNameIdx = u16;

type ASTBody = Vec<parser::ASTStatement>;
type Int = i64;

#[derive(Debug)]
pub enum Token {
    String(&'static str),
    Int(Int),
    Keyword(Keyword),
    VariableName(VariableNameIdx),
    FunctionName(FunctionNameIdx),
    NewLine,
}

#[derive(Copy, Clone, Debug)]
pub enum Keyword {
    CreateVar,
    If,
    While,
    Type(Type),
    // for ending a block
    End,
    // for running an expression
    Invoke,
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
