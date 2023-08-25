mod lexer;
mod parser;

fn main() {
    println!("Hello, world!");

    let file_name = 'vall: {
        for arg in std::env::args().skip(1) {
            println!("arg: {}", arg);
            break 'vall arg;
        }
        "please-give-file-to-compile".to_string()
    };

    assert!(file_name.ends_with(".dode"));

    let content = std::fs::read_to_string(file_name).expect("file does not exist!");

    println!(" --- STARTING LEXING!");
    let tokens = lexer::generate_tokens(&content);
    println!("tokens: {:?}", tokens);

    println!(" --- STARTING PARSING!");
    let ast = parser::parse_tokens(&tokens);
}

const STRING_DELIMITER: char = '"';
const VARIABLE_PREFIX: char = '$';

type VariableNameIdx = u8;
type FunctionNameIdx = u16;

#[derive(Debug)]
pub enum Token {
    String(&'static str),
    Number(i64),
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
    Set(SetType),
    Equals,
    Plus,
    Minus,
    Multiply,
    StartParen,
    EndParen,
    Comma,
}

#[derive(Copy, Clone, Debug)]
pub enum SetType {
    Set,
    Add,
    Sub,
}
