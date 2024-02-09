#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Fun,
    Identifier(String),
    Colon,
    LeftParenthesis,
    RightParenthesis,
    StringLiteral(String),
    Print,
    End,
    Let,
    IntLiteral(i64),
    FloatLiteral(f64),
}

pub struct Scanner {
    source: String,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner { source }
    }

    pub fn scanning(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut buffer = String::new();
        let mut chars = self.source.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                '/' => {
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch == '/' {
                            while let Some(&next_ch) = chars.peek() {
                                if next_ch == '\n' {
                                    break;
                                }
                                chars.next();
                            }

                            continue;
                        }
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    while let Some(&next_ch) = chars.peek() {
                        if next_ch.is_ascii_alphabetic() || next_ch == '_' {
                            buffer.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    match buffer.as_str() {
                        "fun" => tokens.push(Token::Fun),
                        "print" => tokens.push(Token::Print),
                        "end" => tokens.push(Token::End),
                        "let" => tokens.push(Token::Let),
                        identifier => tokens.push(Token::Identifier(identifier.to_string())),
                    }

                    buffer.clear();
                }
                '0'..='9' => {
                    while let Some(&next_ch) = chars.peek() {
                        if next_ch.is_digit(10) || next_ch == '.' {
                            buffer.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    if buffer.contains('.') {
                        if let Ok(float_value) = buffer.parse::<f64>() {
                            tokens.push(Token::FloatLiteral(float_value));
                        } else {
                            panic!("floating is invalid!");
                        }
                    } else {
                        if let Ok(int_value) = buffer.parse::<i64>() {
                            tokens.push(Token::IntLiteral(int_value))
                        } else {
                            panic!("int is invalid!")
                        }
                    }

                    buffer.clear();
                }
                ':' => {
                    tokens.push(Token::Colon);
                    chars.next();
                }
                '(' => {
                    tokens.push(Token::LeftParenthesis);
                    chars.next();
                }
                ')' => {
                    tokens.push(Token::RightParenthesis);
                    chars.next();
                }
                '"' => {
                    chars.next();
                    while let Some(&next_ch) = chars.peek() {
                        if next_ch != '"' {
                            buffer.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    tokens.push(Token::StringLiteral(buffer.clone()));
                    buffer.clear();
                    chars.next();
                }
                ' ' | '\n' | '\t' => {
                    chars.next();
                }
                _ => {
                    chars.next();
                }
            }
        }
        tokens
    }
}