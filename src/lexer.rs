use crate::FunctionNameIdx;
use crate::Keyword;
use crate::Token;
use crate::SetType;
use crate::VariableNameIdx;
use crate::STRING_DELIMITER;
use crate::VARIABLE_PREFIX;

/// lexer
pub fn generate_tokens(content: &str) -> Vec<Token> {
    fn is_part_of_keyword(ch: char) -> bool {
        ch.is_ascii_uppercase() || ch == '=' || ch == '-' || ch == '+'
    }
    fn char_is_valid_var_name(ch: char) -> bool {
        ch.is_ascii_lowercase() || ch == '_'
    }

    let str_to_keyword = {
        let mut map = std::collections::HashMap::new();
        map.insert("FORM", Keyword::CreateVar);
        map.insert("IF", Keyword::If);
        map.insert("WHILE", Keyword::While);
        map.insert("===", Keyword::Set(SetType::Set));
        map.insert("+==", Keyword::Set(SetType::Add));
        map.insert("-==", Keyword::Set(SetType::Sub));
        map.insert("=", Keyword::Equals);
        map.insert("+", Keyword::Plus);
        map.insert("-", Keyword::Minus);
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

    let mut add_token = |token| {
        println!("Added token: {:?}", token);
        tokens.push(token);
    };

    loop {
        println!("char: {}", ch);

        let mut get_a_new_char = false;

        match ch {
            STRING_DELIMITER => {
                // strings
                for (new_idx, new_ch) in &mut chars {
                    if new_ch == STRING_DELIMITER {
                        let actual_string = content[(idx + 1)..new_idx].to_string().leak();
                        add_token(Token::String(actual_string));
                        (idx, ch) = (new_idx, new_ch);
                        get_a_new_char = true;
                        break;
                    }
                }
            }
            chh if chh.is_numeric() => {
                // numbers
                let (new_idx, new_ch) = lex_number(&mut chars);
                let num_str = &content[idx..new_idx];
                add_token(Token::Number(num_str.parse::<_>().unwrap()));
                (idx, ch) = (new_idx, new_ch);
            }
            chh if is_part_of_keyword(chh) => {
                // keywords of lang
                // it is a KEYWORD!
                let (new_idx, new_ch) = lex_keyword(&mut chars);
                let keyword_str = &content[idx..new_idx];
                match str_to_keyword.get(keyword_str) {
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
            '@' => {
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
                    ',' => add_token(Token::Keyword(Keyword::Comma)),
                    '(' => add_token(Token::Keyword(Keyword::StartParen)),
                    ')' => add_token(Token::Keyword(Keyword::EndParen)),
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
        var_names_to_int, func_names_to_int
    );

    tokens
}
