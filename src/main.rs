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

    let tokens = generate_tokens(&content);
    println!("tokens: {:?}", tokens);
}

const STRING_DELIMITER: char = '"';
const VARIABLE_PREFIX: char = '$';

type VariableNameIdx = u8;
type FunctionNameIdx = u16;

#[derive(Debug)]
enum Token {
    String(String),
    Number(i32),
    Keyword(Keyword),
    VariableName(VariableNameIdx),
    NewLine,
    FunctionName(FunctionNameIdx),
}

#[derive(Copy, Clone, Debug)]
enum Keyword {
    CreateVar,
    If,
    Else,
    Set,
    SetAdd,
    SetSub,
    Equals,
    Plus,
    Minus,
}

//const KEYWORDS: &[&'static str] = &["FORM", "IF", "ELSE"];

/// lexer
fn generate_tokens(content: &str) -> Vec<Token> {

    fn is_part_of_keyword(ch: char) -> bool {
        ch.is_ascii_uppercase() || ch == '=' || ch == '-' || ch == '+'
    }

    let str_to_keyword = {
        let mut map = std::collections::HashMap::new();
        map.insert("FORM", Keyword::CreateVar);
        map.insert("IF", Keyword::If);
        map.insert("===", Keyword::Set);
        map.insert("+==", Keyword::SetAdd);
        map.insert("-==", Keyword::SetSub);
        map.insert("=", Keyword::Equals);
        map.insert("+", Keyword::Plus);
        map.insert("-", Keyword::Minus);
        map.insert("(", Keyword::Minus);
        map.insert(")", Keyword::Minus);
        map
    };
    // to return
    let mut tokens: Vec<Token> = Vec::new();

    let mut var_names_to_int = std::collections::HashMap::<&str, VariableNameIdx>::new();
    let mut func_names_to_int = std::collections::HashMap::<&str, FunctionNameIdx>::new();

    let mut chars = content.chars().enumerate();
    type LexIterator<'a> = std::iter::Enumerate<std::str::Chars<'a>>;

    fn lex_number(chars: &mut LexIterator) -> (usize, char) {
        for (idx, ch) in chars {
            if !ch.is_numeric() {
                return (idx, ch);
            }
        }
        panic!("ERROR: number at end of file??");
    }

    fn lex_keyword(chars: &mut LexIterator) -> (usize, char) {
        for (idx, ch) in chars {
            if !is_part_of_keyword(ch) {
                return (idx, ch);
            }
        }
        panic!("ERROR: keyword at end of file??");
    }
    let (mut idx, mut ch) = match chars.next() {
        Some(dat) => dat,
        None => panic!("no file?!?!?!"),
    };

    loop {
        println!("char: {}", ch );

        let mut get_a_new_char = false; 
            
        if ch == STRING_DELIMITER {
            for (new_idx, new_ch) in &mut chars {
                if new_ch == STRING_DELIMITER {
                    let actual_string = content[(idx+1)..new_idx].to_string();
                    println!("lexed string: {}", actual_string);
                    tokens.push(Token::String(actual_string));
                    (idx, ch) = (new_idx, new_ch);
                    get_a_new_char = true;
                    break;
                }
            }
        } else if ch.is_numeric() {
            let (new_idx, new_ch) = lex_number(&mut chars);
            let num_str = &content[idx..new_idx];
            println!("got number: {}", num_str);
            tokens.push(Token::Number(num_str.parse::<_>().unwrap()));
            (idx, ch) = (new_idx, new_ch);
        } else if is_part_of_keyword(ch) {
            // it is a KEYWORD!
            let (new_idx, new_ch) = lex_keyword(&mut chars);
            let keyword_str = &content[idx..new_idx];
            println!("add keyword: {:?}", keyword_str);
            match str_to_keyword.get(keyword_str) {
                Some(&keyword) => tokens.push(Token::Keyword(keyword)),
                None => {
                    // Error invalid keyword!
                    panic!("Invalid keyword: `{}`", keyword_str);
                }
            }
            (idx, ch) = (new_idx, new_ch);
        } else if ch == VARIABLE_PREFIX {
            for (new_idx, new_ch) in &mut chars {
                if !new_ch.is_ascii_lowercase() {
                    let var_name = &content[idx..new_idx];
                    println!("got variableName: {}", var_name);
                    match var_names_to_int.get(var_name) {
                        Some(&int) => {
                            // add token for already existing variable
                            tokens.push(Token::VariableName(int));
                        }
                        None => {
                            let int = var_names_to_int.len() as _;
                            var_names_to_int.insert(var_name, int);
                            tokens.push(Token::VariableName(int));
                        }
                    }
                    (idx, ch) = (new_idx, new_ch);
                    break;
                }
            }
        } else if ch == '\n' {
            tokens.push(Token::NewLine);
            get_a_new_char = true;
        }
        else if ch == '@' {
            // a function call
            for (new_idx, new_ch) in &mut chars {
                if !new_ch.is_ascii_lowercase() {
                    let func_name = &content[idx..new_idx];
                    println!("got functionName: {}", func_name);
                    match func_names_to_int.get(func_name) {
                        Some(&int) => {
                            // add token for already existing variable
                            tokens.push(Token::FunctionName(int));
                        }
                        None => {
                            let int = var_names_to_int.len() as _;
                            func_names_to_int.insert(func_name, int);
                            tokens.push(Token::FunctionName(int));
                        }
                    }
                    (idx, ch) = (new_idx, new_ch);
                    break;
                }
            }
        }
        else {
            get_a_new_char = true;
        }

        if get_a_new_char {
            (idx, ch) = match chars.next() {
                Some(dat) => dat,
                None => break,
            };
        }
    }
    tokens
}
