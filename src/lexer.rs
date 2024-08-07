use std::collections::HashMap;
use std::collections::HashSet;

use crate::IdentIdx;
use crate::Int;
use crate::Keyword;
use crate::SetType;
use crate::COMMENT_PREFIX;

/// more high-level view of source code with respect to the grammar
#[derive(Debug, Copy, Clone)]
pub enum Token {
    /// index into `str_storage` of Lexer (not used yet)
    String(u16),
    Int(IntIdx),
    Keyword(Keyword),
    Identifier(IdentIdx),
}

/// do not use on first char of identifier
fn is_identifier_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

/// the program can have 65536 ints, in the program and u16 refers to a position
/// in the int_storage array. When lexing, if we find two ints with same value,
/// they will have the same index into the array.
/// TODO: Use hashset to speed up checking for duplicate number instead of O(n) lookup in int_storage

struct Lexer<'a> {
    chars: std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'static>>>,
    /// our append-only buffer
    tokens: Vec<Token>,
    identifier_to_int: HashMap<&'static str, IdentIdx>,
    token_idx_to_char_range: Vec<(usize, usize)>,
    start_char_idx: usize,
    /// keep balanced delimiters ([{}]) correct, usize is place for original delim
    balanced_delim_stack: Vec<(char, usize)>,
    /// string storage, tokens refer by idx to one of these
    str_storage: Vec<&'static str>,
    int_stor: IntStor,

    // constant members
    file_name: &'a str,
    source: &'static str,
    str_to_keyword: HashMap<&'static str, Keyword>,
    mathy_keywords: HashMap<&'static str, Keyword>,
    math_chars: HashSet<char>,
    single_char_repeatables: HashMap<char, Keyword>,
}

/// idx 0-255 are mapped directly to what number they are
/// can only hold 65536 ints in total
pub struct IntStor {
    /// shall only store unique Int
    int_storage: Vec<Int>,
    /// O(1) existance lookup
    exists: HashMap<Int, u16>,
}
/// idx into IntStor get method, can be constructed with 0-255 to get the number that the index is
#[derive(Debug, Copy, Clone)]
pub struct IntIdx(u16);
impl IntIdx {
    /// only 0-255 are okay to get for free, maps directly to the number it is
    pub fn new(a: u8) -> IntIdx {
        IntIdx(a as _)
    }
}

impl IntStor {
    /// doesn't allow duplicates, uses same index
    pub fn insert_num_get_idx(&mut self, e: Int) -> IntIdx {
        if let Some(&at) = self.exists.get(&e) {
            IntIdx(at)
        } else {
            let at = self.int_storage.len();
            self.int_storage.push(e);
            let at = at
                .try_into()
                .expect("User program has too many ints, more than 65536");
            self.exists.insert(e, at);
            IntIdx(at)
        }
    }
    pub fn get(&self, idx: IntIdx) -> Int {
        self.int_storage[idx.0 as usize]
    }
}

/// initialize lexer datastructures(should be compiletime but...) and lex
pub fn generate_tokens(
    content: &'static str,
    file_name: &str,
) -> (Vec<Token>, Vec<(usize, usize)>, Vec<&'static str>, IntStor) {
    let mut lexer = {
        // these have to be seperated by whitespace
        let str_to_keyword: HashMap<&str, Keyword> = {
            let mut map = HashMap::new();
            map.insert("if", Keyword::If);
            map.insert("else", Keyword::Else);
            map.insert("while", Keyword::While);
            map.insert("fn", Keyword::FunctionIncoming);
            map.insert("return", Keyword::Return);
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
            map.insert("!", Keyword::CreateConstVar);
            map.insert("!~", Keyword::CreateMutVar);
            map
        };

        // to START looking for `mathy_keywords`, we first notice one of these
        // when lexing mathy keywords, these characters can occur
        // also to know when the mathy_keyword ends, like: asd+=sdf;
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
            set.insert('!');
            set.insert('~');
            set
        };

        // Add [] later when needed
        let single_char_repeatables: HashMap<char, Keyword> = {
            let mut set = HashMap::new();
            set.insert('(', Keyword::StartParen);
            set.insert(')', Keyword::EndParen);
            set.insert('{', Keyword::StartBlock);
            set.insert('}', Keyword::EndBlock);
            set.insert(',', Keyword::Comma);
            set.insert(';', Keyword::EndStatement);
            set.insert(':', Keyword::TypeIncoming);
            set
        };

        Lexer {
            chars: content.chars().enumerate().peekable(),
            source: content,
            tokens: Vec::new(),
            identifier_to_int: HashMap::new(),
            token_idx_to_char_range: Vec::new(),
            start_char_idx: 0,
            balanced_delim_stack: Vec::new(),
            str_storage: Vec::new(),
            int_stor: IntStor {
                int_storage: Vec::new(),
                exists: HashMap::new(),
            },

            file_name,
            str_to_keyword,
            mathy_keywords,
            math_chars,
            single_char_repeatables,
        }
    };
    for i in 0..256 {
        lexer.int_stor.insert_num_get_idx(i);
    }
    lexer.lex();
    let mut identifier_idx_to_string = vec![""; lexer.identifier_to_int.len()];
    for (&string, &ident) in lexer.identifier_to_int.iter() {
        identifier_idx_to_string[ident as usize] = string;
    }

    // TODO: Check if it is best to error on first unclosed delimiter
    if let Some(&bal_del) = lexer.balanced_delim_stack.first() {
        lexer.report_incorrect_syntax_at("Delimiter not closed", bal_del.1, bal_del.1 + 1);
    }

    (
        lexer.tokens,
        lexer.token_idx_to_char_range,
        identifier_idx_to_string,
        lexer.int_stor,
    )
}

impl<'a> Lexer<'a> {
    fn report_incorrect_syntax(&mut self, msg: &str) {
        let start_wher = self.start_char_idx;
        let end_wher = self.peek().0;
        self.report_incorrect_syntax_at(msg, start_wher, end_wher);
    }
    fn report_incorrect_syntax_at(&mut self, msg: &str, start_wher: usize, end_wher: usize) -> ! {
        // TODO print better error message
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
        println!("{} at line {}, col {}", msg, line, col);

        println!("new error message: ");
        crate::mark_error_in_source(self.source, self.file_name, msg, (start_wher, end_wher));
        std::process::exit(1);
    }

    fn peek(&mut self) -> (usize, char) {
        self.chars.peek().copied().unwrap()
    }

    fn eat(&mut self) -> (usize, char) {
        self.chars.next().unwrap()
    }

    /// add new token
    fn push(&mut self, token: Token) {
        println!("add token: {:?}", token);
        self.tokens.push(token);
        let end = self.peek().0;
        self.token_idx_to_char_range
            .push((self.start_char_idx, end));
    }

    fn add_identifier(&mut self, name: &'static str) {
        let idx = match self.identifier_to_int.get(name) {
            Some(&idx) => idx,
            None => {
                let idx = self.identifier_to_int.len() as IdentIdx;
                self.identifier_to_int.insert(name, idx);
                idx
            }
        };
        self.push(Token::Identifier(idx));
    }
    /// get the str from source that starts where you started and ends where you are (in lex)
    fn get_str(&mut self) -> &'static str {
        let end = self.peek().0;
        &self.source[self.start_char_idx..end]
    }

    /// lex, but also check mismatched ({[...]})
    fn lex(&mut self) {
        while let Some((start_idx, ch)) = self.chars.next() {
            self.start_char_idx = start_idx;
            // println!("char {}", ch);
            match ch {
                COMMENT_PREFIX => {
                    while self.peek().1 != '\n' {
                        self.eat();
                    }
                    self.eat(); // eat newline
                }
                // ignore whitespace
                _ if ch.is_whitespace() => (),

                '"' => {
                    while self.eat().1 != '"' {} // eat last
                    let s = self.get_str(); // TODO: this is actually probably wrong and will include the "" in the string
                    self.push(Token::String(self.str_storage.len().try_into().unwrap()));
                    self.str_storage.push(s); // correct order here is important
                }
                // parse number
                _ if ch.is_numeric() => {
                    while self.peek().1.is_numeric() {
                        self.eat();
                    }

                    let (_, after_char) = self.peek();

                    // if the character after the number is a letter, error
                    if after_char.is_alphabetic() {
                        self.report_incorrect_syntax(
                            "Invalid number! Numbers cannot be followed by letters!",
                        );
                    }
                    // TODO turn into floating point token if floating point

                    let num_str = self.get_str();
                    let num = num_str.parse::<_>().unwrap();
                    // if number already exists in int_storage: reuse that
                    // TODO: have O(1) hashset to tell whether number is in int_storage
                    let e = self.int_stor.insert_num_get_idx(num);
                    self.push(Token::Int(e));
                }

                // special characters like {}(),; that can be repeated without whitespace
                _ if self.single_char_repeatables.contains_key(&ch) => {
                    // special characters that can be repeated without whitespace
                    fn flip_delim(e: char) -> char {
                        match e {
                            '(' => ')',
                            ')' => '(',
                            '[' => ']',
                            ']' => '[',
                            '{' => '}',
                            '}' => '{',
                            _ => unreachable!(),
                        }
                    }
                    match ch {
                        '(' | '[' | '{' => self.balanced_delim_stack.push((ch, start_idx)),
                        ')' | ']' | '}' => match self.balanced_delim_stack.pop() {
                            Some(correct) => {
                                let correct_ch = flip_delim(correct.0);
                                if ch != correct_ch {
                                    self.report_incorrect_syntax_at(
                                        &format!(
                                            "This delimiter not closed properly. Needs {} but found {}",
                                            correct_ch, ch
                                        ),
                                        correct.1,
                                        correct.1 + 1,
                                    );
                                }
                            }
                            None => {
                                self.report_incorrect_syntax("bad delimiting");
                            }
                        },
                        _ => (),
                    }
                    self.push(Token::Keyword(self.single_char_repeatables[&ch]));
                }
                // special math operators that can have names after them
                _ if self.math_chars.contains(&ch) => {
                    // keywords of lang
                    while {
                        let chh = self.peek().1;
                        self.math_chars.contains(&chh)
                    } {
                        self.eat();
                    }
                    let keyword_str = self.get_str();
                    match self.mathy_keywords.get(keyword_str) {
                        Some(&kw) => self.push(Token::Keyword(kw)),
                        None => {
                            self.report_incorrect_syntax(&format!(
                                "Invalid operator keyword: `{}`",
                                keyword_str
                            ));
                        }
                    }
                }
                // identifier or keyword, must start with alphabetic, then can be more
                _ if ch.is_alphabetic() => {
                    // Can either be a keyword or a variable name or a function name
                    while {
                        let c = self.peek().1;
                        is_identifier_char(c)
                    } {
                        self.eat();
                    }
                    // check if it is a keyword
                    let name_str = self.get_str();
                    match self.str_to_keyword.get(name_str) {
                        Some(&keyword) => self.push(Token::Keyword(keyword)),
                        None => {
                            // not a keyword, must be a identifier (variable or function name)
                            self.add_identifier(name_str);
                        }
                    }
                }
                // other characters
                _ => {
                    self.report_incorrect_syntax(&format!(
                        "Unrecognized character: `{}`, not part of grammar",
                        ch
                    ));
                }
            }
        }
        println!("idents: {:?}", self.identifier_to_int);
    }
}
