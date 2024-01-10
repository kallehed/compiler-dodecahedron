use core::panic;
use std::collections::HashMap;
use std::collections::HashSet;

use crate::IdentifierIdx;
use crate::Int;
use crate::Keyword;
use crate::SetType;
use crate::COMMENT_PREFIX;
use crate::STRING_DELIMITER;

#[derive(Debug, Copy, Clone)]
pub enum Token {
    String(&'static str),
    Int(Int),
    Keyword(Keyword),
    Identifier(IdentifierIdx),
}

/// do not use on first char of identifier
fn is_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

struct Lexer {
    chars: std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'static>>>,
    source: &'static str,
    tokens: Vec<Token>,
    identifier_to_int: HashMap<&'static str, IdentifierIdx>,
    token_idx_to_char_nr: Vec<(usize, usize)>,

    // constant members
    str_to_keyword: HashMap<&'static str, Keyword>,
    mathy_keywords: HashMap<&'static str, Keyword>,
    math_chars: HashSet<char>,
    single_char_repeatables: HashMap<char, Keyword>,
}

impl Lexer {
    fn new(content: &'static str) -> Self {
        // these have to be seperated by whitespace
        let str_to_keyword: HashMap<&str, Keyword> = {
            let mut map = HashMap::new();
            map.insert("let", Keyword::CreateVar);
            map.insert("if", Keyword::If);
            map.insert("while", Keyword::While);
            map.insert("fn", Keyword::FunctionIncoming);
            map
        };

        // these can be inbetween alphanumeric characters
        let mathy_keywords: HashMap<&str, Keyword> = {
            let mut map = HashMap::new();
            map.insert("=", Keyword::Set(SetType::Set));
            map.insert("+=", Keyword::Set(SetType::Add));
            map.insert("-=", Keyword::Set(SetType::Sub));
            map.insert("==", Keyword::Equals);
            map.insert("+", Keyword::Plus);
            map.insert("-", Keyword::Minus);
            map.insert("*", Keyword::Multiply);
            map.insert("<", Keyword::Less);
            map.insert(">", Keyword::More);
            map
        };

        // when lexing mathy keywords, these characters can occur
        let math_chars: HashSet<char> = {
            let mut set = HashSet::new();
            set.insert('=');
            set.insert('+');
            set.insert('-');
            set.insert('<');
            set.insert('>');
            set.insert('*');
            set.insert('/');
            set.insert('%');
            set
        };

        let single_char_repeatables: HashMap<char, Keyword> = {
            let mut set = HashMap::new();
            set.insert('{', Keyword::StartBlock);
            set.insert('}', Keyword::EndBlock);
            set.insert('(', Keyword::StartParen);
            set.insert(')', Keyword::EndParen);
            set.insert(',', Keyword::Comma);
            set.insert(';', Keyword::EndStatement);
            set.insert(':', Keyword::TypeIncoming);
            set
        };

        Self {
            chars: content.chars().enumerate().peekable(),
            source: content,
            tokens: Vec::new(),
            identifier_to_int: HashMap::new(),
            token_idx_to_char_nr: Vec::new(),

            str_to_keyword,
            mathy_keywords,
            math_chars,
            single_char_repeatables,
        }
    }

    fn report_incorrect_syntax(&self, msg: &str, start_wher: usize, _end_wher: usize) { // TODO print better error message
        let mut line = 1;
        let mut col = 1;
        for (idx, ch) in self.source.chars().enumerate() {
            if idx == start_wher {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        panic!("{} at line {}, col {}", msg, line, col);
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        self.chars.peek().copied()
    }

    fn eat(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn add_token(&mut self, token: Token, start_char_idx: usize, end_char_idx: usize) {
        println!("add token: {:?}", token);
        self.tokens.push(token);
        self.token_idx_to_char_nr.push((start_char_idx, end_char_idx));
    }

    fn add_identifier(&mut self, name: &'static str, start_char_idx: usize, end_char_idx: usize) {
        let idx = match self.identifier_to_int.get(name) {
            Some(&idx) => idx,
            None => {
                let idx = self.identifier_to_int.len() as IdentifierIdx;
                self.identifier_to_int.insert(name, idx);
                idx
            }
        };
        self.add_token(Token::Identifier(idx), start_char_idx, end_char_idx);
    }

    fn lex(&mut self) {
        loop {
            match match self.peek() {
                Some((_, ch)) => {
                    println!("char {}", ch);
                    ch
                }
                None => break,
            } {
                // # comment
                COMMENT_PREFIX => {
                    // don't care until newline
                    while self.peek().unwrap().1 != '\n' {
                        self.eat();
                    }
                    self.eat(); // eat newline
                }
                // ignore whitespace
                ch if ch.is_whitespace() => {
                    self.eat();
                }
                // parse string
                STRING_DELIMITER => {
                    // string
                    let (idx, _) = self.eat().unwrap();
                    while self.peek().unwrap().1 != STRING_DELIMITER {
                        self.eat();
                    }
                    let (end_idx, _) = self.eat().unwrap(); // eat the end string delimiter
                    let the_str = &self.source[(idx + 1)..end_idx];
                    self.add_token(Token::String(the_str), idx, end_idx);
                }
                // parse number
                ch if ch.is_numeric() => {
                    let (start_idx, _) = self.eat().unwrap();
                    while self.peek().unwrap().1.is_numeric() {
                        self.eat();
                    }

                    let &(mut end_idx, mut after_char) = self.chars.peek().unwrap();

                    if after_char == '.' {
                        // floating point number
                        self.eat();
                        while self.peek().unwrap().1.is_numeric() {
                            self.eat();
                        }
                        let &(another_end_idx, another_after_char) = self.chars.peek().unwrap();
                        end_idx = another_end_idx;
                        after_char = another_after_char;
                    }
                    // if the character after the number is a letter, error
                    if after_char.is_alphabetic() {
                        self.report_incorrect_syntax(
                            "Invalid number! Numbers cannot be followed by letters!",
                            start_idx,
                            end_idx,
                        );
                    }

                    // TODO turn into floating point token if floating point

                    let num_str = &self.source[start_idx..end_idx];
                    let num = num_str.parse::<_>().unwrap();
                    self.add_token(Token::Int(num), start_idx, end_idx);
                }

                // special characters like {}(),; that can be repeated without whitespace
                ch if self.single_char_repeatables.contains_key(&ch) => {
                    // special characters that can be repeated without whitespace
                    let (idx, _) = self.eat().unwrap();
                    self.add_token(Token::Keyword(
                        *self.single_char_repeatables.get(&ch).unwrap(),
                    ), idx, idx + 1);
                }
                // special math operators that can have names after them
                ch if self.math_chars.contains(&ch) => {
                    // keywords of lang
                    let (start_idx, _) = self.eat().unwrap();
                    while {
                        let chh = self.peek().unwrap().1;
                        self.math_chars.contains(&chh)
                    } {
                        self.eat();
                    }
                    let (end_idx, _) = self.peek().unwrap();
                    let keyword_str = &self.source[start_idx..end_idx];
                    match self.mathy_keywords.get(keyword_str) {
                        Some(&kw) => self.add_token(Token::Keyword(kw), start_idx, end_idx),
                        None => {
                            self.report_incorrect_syntax(
                                &format!("Invalid mathy keyword: `{}`", keyword_str),
                                start_idx, // Error invalid mathy keyword!
                                end_idx,
                            );
                        }
                    }
                }
                // identifier or keyword
                ch if ch.is_alphabetic() => {
                    // Can either be a keyword or a variable name or a function name
                    let (start_idx, _) = self.eat().unwrap();

                    while {
                        let c = self.peek().unwrap().1;
                        is_identifier_char(c)
                    } {
                        self.eat();
                    }
                    let (end_idx, _) = self.peek().unwrap();

                    // check if it is a keyword
                    let name_str = &self.source[start_idx..end_idx];
                    match self.str_to_keyword.get(name_str) {
                        Some(&keyword) => self.add_token(Token::Keyword(keyword), start_idx, end_idx),
                        None => {
                            // not a keyword, must be a identifier (variable or function name)
                            self.add_identifier(name_str, start_idx, end_idx);
                        }
                    }
                }
                // other characters
                ch => {
                    panic!("unrecognized character: {}", ch);
                }
            }
        }

        println!(
            "idents: {:?}",
            self.identifier_to_int
        );
    }
}

/// lexer
pub fn generate_tokens(content: &'static str) -> (Vec<Token>, Vec<(usize, usize)>) {
    let mut lexer = Lexer::new(content);
    lexer.lex();
    (lexer.tokens, lexer.token_idx_to_char_nr)
}
