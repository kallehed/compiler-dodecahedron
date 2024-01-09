use core::num;
use std::collections::HashMap;

use crate::FunctionNameIdx;
use crate::Keyword;
use crate::SetType;
use crate::Token;
use crate::Type;
use crate::VariableNameIdx;
use crate::COMMENT_PREFIX;
use crate::FUNCTION_PREFIX;
use crate::STRING_DELIMITER;
use crate::VARIABLE_PREFIX;
use crate::Int;

fn is_part_of_keyword(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '=' || ch == '-' || ch == '+' || ch == '<' || ch == '>'
}

fn char_is_valid_var_name(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

struct Lexer<'a> {
    chars: std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'static>>>,
    source: &'a str,
    tokens: Vec<Token>,
    var_names_to_int: HashMap<&'static str, VariableNameIdx>,
    func_names_to_int: HashMap<&'static str, FunctionNameIdx>,
}

impl <'a> Lexer<'a> {

    fn new(content: &str) -> Self {
        Self {
            chars:  content.chars().enumerate().peekable(),
            source: content,
            tokens: Vec::new(),
            var_names_to_int: HashMap::new(),
            func_names_to_int: HashMap::new(),
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.chars.peek().copied()
    }

    fn eat(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    // call when you peeked the beginning of a number, this function returns that number
    fn lex_number(&mut self) -> Int {
        let (start_idx, _) = self.eat().unwrap();
        loop {
            if let Some((_, ch)) = self.peek() {
                if !ch.is_numeric() {
                    break;
                }
                self.eat();
            } else {
                panic!("ERROR: number at end of file??");
            }
        }
        let &(end_idx, _) = self.chars.peek().unwrap();

        let num_str = &self.source[start_idx..end_idx];
        num_str.parse::<_>().unwrap()
    }

    fn lex_keyword(&mut self) -> (usize, char) {
        for (idx, ch) in self.chars {
            if !is_part_of_keyword(ch) {
                return (idx, ch);
            }
        }
        panic!("ERROR: keyword at end of file??");
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn lex(&mut self) -> Vec<Token> {
        loop {
            let ch = match self.peek() {
                Some((_, ch)) => ch,
                None => break,
            };
            println!("char: {}", ch);

            match ch {
                COMMENT_PREFIX => { // don't care until newline
                    while self.peek().unwrap().1 != '\n' {
                        self.eat();
                    }
                    self.eat(); // eat newline
                }
                STRING_DELIMITER => {
                    // string
                    let (idx, _) = self.eat().unwrap();
                    while self.peek().unwrap().1 != STRING_DELIMITER {
                        self.eat();
                    }
                    let (end_idx, _) = self.eat().unwrap(); // eat the end string delimiter
                    self.add_token(Token::String(&self.source[(idx + 1)..end_idx].to_owned()));

                }
                chh if chh.is_numeric() => {
                    let (start_idx, _) = self.eat().unwrap();
                    while self.peek().unwrap().1.is_numeric() {
                        self.eat();
                    }

                    let &(mut end_idx, mut after_char) = self.chars.peek().unwrap();

                    if after_char == '.' { // floating point number
                        self.eat();
                        while self.peek().unwrap().1.is_numeric() {
                            self.eat();
                        }
                        let &(another_end_idx, another_after_char) = self.chars.peek().unwrap();
                        end_idx = another_end_idx;
                        after_char = another_after_char;
                    }
                    // TODO turn into floating point token if floating point

                    let num_str = &self.source[start_idx..end_idx];
                    let num = num_str.parse::<_>().unwrap();
                    self.add_token(Token::Int(num));
                }
                chh if chh.is_alphabetic() => {
                    // Can either be a keyword or a variable name or a function name
                    let (start_idx, _) = self.eat().unwrap();
                    let (end_idx, _) = self.lex_keyword();
                    let keyword_str = &self.source[start_idx..end_idx];
                    match STR_TO_KEYWORD.get(keyword_str) {
                        Some(&keyword) => self.add_token(Token::Keyword(keyword)),
                        None => {
                            // Error invalid keyword!
                            panic!("Invalid keyword: `{}`", keyword_str);
                        }
                    }
                }
                chh if is_part_of_keyword(chh) => {
                    // keywords of lang
                    // it is a KEYWORD!
                    

                    match STR_TO_KEYWORD.get(keyword_str) {
                        Some(&keyword) => add_token(Token::Keyword(keyword)),
                        None => {
                            // Error invalid keyword!
                            panic!("Invalid keyword: `{}`", keyword_str);
                        }
                    }
                    (idx, ch) = (new_idx, new_ch);
                }
                VARIABLE_PREFIX => {
                    // variables
                    for (new_idx, new_ch) in &mut chars {
                        if !char_is_valid_var_name(new_ch) {
                            let var_name = &content[idx..new_idx];
                            match var_names_to_int.get(var_name) {
                                Some(&int) => {
                                    // add token for already existing variable
                                    add_token(Token::VariableName(int));
                                }
                                None => {
                                    let int = var_names_to_int.len() as _;
                                    var_names_to_int.insert(var_name, int);
                                    add_token(Token::VariableName(int));
                                }
                            }
                            (idx, ch) = (new_idx, new_ch);
                            break;
                        }
                    }
                }
                // a function call
                FUNCTION_PREFIX => {
                    for (new_idx, new_ch) in &mut chars {
                        if !new_ch.is_ascii_lowercase() {
                            let func_name = &content[idx..new_idx];
                            match func_names_to_int.get(func_name) {
                                Some(&int) => {
                                    // add token for already existing variable
                                    add_token(Token::FunctionName(int));
                                }
                                None => {
                                    let int = func_names_to_int.len() as _;
                                    func_names_to_int.insert(func_name, int);
                                    add_token(Token::FunctionName(int));
                                }
                            }
                            (idx, ch) = (new_idx, new_ch);
                            break;
                        }
                    }
                }
                // other characters
                _ => {
                    match ch {
                        '\n' => add_token(Token::NewLine),
                        ',' => {
                            add_token(Token::Keyword(Keyword::EndParen));
                            add_token(Token::Keyword(Keyword::Comma));
                            add_token(Token::Keyword(Keyword::StartParen));
                        }
                        '(' => {
                            add_token(Token::Keyword(Keyword::StartParen));
                            add_token(Token::Keyword(Keyword::StartParen));
                        }
                        ')' => {
                            add_token(Token::Keyword(Keyword::EndParen));
                            add_token(Token::Keyword(Keyword::EndParen));
                        }
                        '*' => add_token(Token::Keyword(Keyword::Multiply)),
                        _ => (),
                    }
                    get_a_new_char = true;
                }
            }

            if get_a_new_char {
                (idx, ch) = match chars.next() {
                    Some(dat) => dat,
                    None => break,
                };
            }
        }

        println!(
            "vars: {:?}, funcs: {:?}",
            self.var_names_to_int, self.func_names_to_int
        );

        self.tokens
    }
}

const STR_TO_KEYWORD: HashMap<&str, Keyword> = {
    let mut map = HashMap::new();
    map.insert("let", Keyword::CreateVar);
    map.insert("int", Keyword::Type(Type::Whole));
    map.insert("REAL", Keyword::Type(Type::Real));
    map.insert("if", Keyword::If);
    map.insert("while", Keyword::While);
    map.insert("END", Keyword::End);
    map.insert("INVOKE", Keyword::Invoke);
    map.insert("=", Keyword::Set(SetType::Set));
    map.insert("+=", Keyword::Set(SetType::Add));
    map.insert("-=", Keyword::Set(SetType::Sub));
    map.insert("==", Keyword::Equals);
    map.insert("+", Keyword::Plus);
    map.insert("-", Keyword::Minus);
    map.insert("<", Keyword::Less);
    map.insert(">", Keyword::More);
    map
};

/// lexer
pub fn generate_tokens(content: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(content);
    lexer.lex()
}
