use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Tokens {
    FUN,
    IDENTIFIER(String),
    COLON,
    COMMA,
    DOT,
    LEFTPAREN,
    RIGHTPAREN,
    STRINGLITERAL(String),
    PRINT,
    END,
    LET,
    LEFTBRACK,
    RIGHTBRACK,
    INTLITERAL(i64),
    FLOATLITERAL(f64),
    BANG,
    BANGEQUAL,
    ASSIGNMENT,
    EQUALEQUAL,
    GREATER,
    GREATEREQUAL,
    LESS,
    LESSEQUAL,
    MINUS,
    PLUS,
    SLASH,
    STAR,
    AND,
    ELSE,
    FALSE,
    FOR,
    IF,
    NIL,
    OR,
    RETURN,
    TRUE,
    WHILE,
    EOF,
}

impl fmt::Display for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tokens::FUN => write!(f, "FUN"),
            Tokens::IDENTIFIER(name) => write!(f, "{}", name),
            Tokens::COLON => write!(f, ":"),
            Tokens::COMMA => write!(f, ","),
            Tokens::DOT => write!(f, "."),
            Tokens::LEFTPAREN => write!(f, "("),
            Tokens::RIGHTPAREN => write!(f, ")"),
            Tokens::STRINGLITERAL(value) => write!(f, "\"{}\"", value),
            Tokens::PRINT => write!(f, "PRINT"),
            Tokens::END => write!(f, "END"),
            Tokens::LET => write!(f, "LET"),
            Tokens::LEFTBRACK => write!(f, "["),
            Tokens::RIGHTBRACK => write!(f, "]"),
            Tokens::INTLITERAL(value) => write!(f, "{}", value),
            Tokens::FLOATLITERAL(value) => write!(f, "{}", value),
            Tokens::BANG => write!(f, "!"),
            Tokens::BANGEQUAL => write!(f, "!="),
            Tokens::ASSIGNMENT => write!(f, "="),
            Tokens::EQUALEQUAL => write!(f, "=="),
            Tokens::GREATER => write!(f, ">"),
            Tokens::GREATEREQUAL => write!(f, ">="),
            Tokens::LESS => write!(f, "<"),
            Tokens::LESSEQUAL => write!(f, "<="),
            Tokens::MINUS => write!(f, "-"),
            Tokens::PLUS => write!(f, "+"),
            Tokens::SLASH => write!(f, "/"),
            Tokens::STAR => write!(f, "*"),
            Tokens::AND => write!(f, "AND"),
            Tokens::ELSE => write!(f, "ELSE"),
            Tokens::FALSE => write!(f, "FALSE"),
            Tokens::FOR => write!(f, "FOR"),
            Tokens::IF => write!(f, "IF"),
            Tokens::NIL => write!(f, "NIL"),
            Tokens::OR => write!(f, "OR"),
            Tokens::RETURN => write!(f, "RETURN"),
            Tokens::TRUE => write!(f, "TRUE"),
            Tokens::WHILE => write!(f, "WHILE"),
            Tokens::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { source }
    }

    pub fn scanning(&mut self) -> Vec<Tokens> {
        let mut tokens: Vec<Tokens> = Vec::new();
        let mut buffer = String::new();
        let mut chars = self.source.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                '/' => {
                    chars.next();
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch == '/' {
                            while let Some(&next_ch) = chars.peek() {
                                if next_ch == '\n' {
                                    break;
                                }
                                chars.next();
                            }
                            chars.next();
                            continue;
                        } else {
                            tokens.push(Tokens::SLASH);
                        }
                    } else {
                        tokens.push(Tokens::SLASH);
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
                        "fun" => tokens.push(Tokens::FUN),
                        "print" => tokens.push(Tokens::PRINT),
                        "end" => tokens.push(Tokens::END),
                        "let" => tokens.push(Tokens::LET),
                        "if" => tokens.push(Tokens::IF),
                        "else" => tokens.push(Tokens::ELSE),
                        "return" => tokens.push(Tokens::RETURN),
                        "true" => tokens.push(Tokens::TRUE),
                        "false" => tokens.push(Tokens::FALSE),
                        "while" => tokens.push(Tokens::WHILE),
                        "for" => tokens.push(Tokens::FOR),
                        "and" => tokens.push(Tokens::AND),
                        "or" => tokens.push(Tokens::OR),
                        "nil" => tokens.push(Tokens::NIL),
                        identifier => tokens.push(Tokens::IDENTIFIER(identifier.to_string())),
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
                            tokens.push(Tokens::FLOATLITERAL(float_value));
                        } else {
                            panic!("floating is invalid!");
                        }
                    } else {
                        if let Ok(int_value) = buffer.parse::<i64>() {
                            tokens.push(Tokens::INTLITERAL(int_value))
                        } else {
                            panic!("int is invalid!")
                        }
                    }

                    buffer.clear();
                }
                ',' => {
                    tokens.push(Tokens::COMMA);
                    chars.next();
                }
                '.' => {
                    tokens.push(Tokens::DOT);
                    chars.next();
                }
                '=' => {
                    chars.next();
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch == '=' {
                            tokens.push(Tokens::EQUALEQUAL);
                            chars.next();
                        } else {
                            tokens.push(Tokens::ASSIGNMENT);
                        }
                    } else {
                        tokens.push(Tokens::ASSIGNMENT);
                    }
                }
                '!' => {
                    chars.next();
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch == '=' {
                            tokens.push(Tokens::BANGEQUAL);
                            chars.next();
                        } else {
                            tokens.push(Tokens::BANG);
                        }
                    } else {
                        tokens.push(Tokens::BANG);
                    }
                }
                '>' => {
                    chars.next();
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch == '=' {
                            tokens.push(Tokens::GREATEREQUAL);
                            chars.next();
                        } else {
                            tokens.push(Tokens::GREATER);
                        }
                    } else {
                        tokens.push(Tokens::GREATER);
                    }
                }
                '<' => {
                    chars.next();
                    if let Some(&next_ch) = chars.peek() {
                        if next_ch == '=' {
                            tokens.push(Tokens::LESSEQUAL);
                            chars.next();
                        } else {
                            tokens.push(Tokens::LESS);
                        }
                    } else {
                        tokens.push(Tokens::LESS);
                    }
                }
                ':' => {
                    tokens.push(Tokens::COLON);
                    chars.next();
                }
                '(' => {
                    tokens.push(Tokens::LEFTPAREN);
                    chars.next();
                }
                ')' => {
                    tokens.push(Tokens::RIGHTPAREN);
                    chars.next();
                }
                '[' => {
                    tokens.push(Tokens::LEFTBRACK);
                    chars.next();
                }
                ']' => {
                    tokens.push(Tokens::RIGHTBRACK);
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
                    tokens.push(Tokens::STRINGLITERAL(buffer.clone()));
                    buffer.clear();
                    chars.next();
                }
                '-' => {
                    tokens.push(Tokens::MINUS);
                    chars.next();
                }
                '+' => {
                    tokens.push(Tokens::PLUS);
                    chars.next();
                }
                '*' => {
                    tokens.push(Tokens::STAR);
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
        tokens.push(Tokens::EOF);
        tokens
    }
}
