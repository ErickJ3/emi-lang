use crate::scanner::Tokens;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number(i64),
    Float(f64),
    StringLiteral(String),
    Variable(String),
    UnaryOp {
        operator: Tokens,
        right: Box<Expr>,
    },
    BinaryOp {
        left: Box<Expr>,
        operator: Tokens,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    PrintStmt(Box<Expr>),
    LetStmt {
        identifier: String,
        value: Box<Expr>,
    },
    IfStmt {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    },
    Block(Vec<Expr>),
    WhileStmt {
        condition: Box<Expr>,
        body: Box<Expr>,
    },
    ForStmt {
        initialization: Box<Expr>,
        condition: Box<Expr>,
        increment: Box<Expr>,
        body: Box<Expr>,
    },
    Function {
        name: String,
        parameters: Vec<String>,
        body: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    ReturnStmt(Option<Box<Expr>>),
    Nil,
}

pub struct Parser<'a> {
    tokens: &'a [Tokens],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Tokens]) -> Self {
        Parser { tokens, current: 0 }
    }

    fn peek(&self) -> Option<&Tokens> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Option<&Tokens> {
        if !self.is_at_end() {
            let token = &self.tokens[self.current];
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek().is_none()
    }

    fn consume(&mut self, token_type: Tokens) -> Option<&Tokens> {
        if let Some(token) = self.peek() {
            if *token == token_type {
                self.advance()
            } else {
                None
            }
        } else {
            None
        }
    }

    fn error(&self, message: &str) {
        eprintln!("error: {}", message);
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        if let Some(token) = self.advance() {
            match token {
                Tokens::INTLITERAL(value) => Some(Expr::Number(*value)),
                Tokens::FLOATLITERAL(value) => Some(Expr::Float(*value)),
                Tokens::STRINGLITERAL(value) => Some(Expr::StringLiteral(value.clone())),
                Tokens::IDENTIFIER(name) => Some(Expr::Variable(name.clone())),
                Tokens::LEFTPAREN => {
                    let expr = self.parse_expression()?;
                    if self.consume(Tokens::RIGHTPAREN).is_none() {
                        self.error("expected ')' after expression.");
                        return None;
                    }
                    Some(Expr::Grouping(Box::new(expr)))
                }
                _ => {
                    eprintln!("expected expression. got: {:?}", token);
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse_print_statement(&mut self) -> Option<Expr> {
        if self.consume(Tokens::PRINT).is_none() {
            self.error("expected 'print' keyword.");
            return None;
        }

        let value = self.parse_expression()?;

        Some(Expr::PrintStmt(Box::new(value)))
    }

    fn parse_let_statement(&mut self) -> Option<Expr> {
        self.advance();

        let identifier = match self.peek() {
            Some(&Tokens::IDENTIFIER(ref name)) => name.clone(),
            _ => {
                self.error("expected identifier after 'let' keyword.");
                return None;
            }
        };

        self.advance();

        if self.consume(Tokens::ASSIGNMENT).is_none() {
            self.error("expected '=' after identifier.");
            return None;
        }

        if self.consume(Tokens::LET).is_some() {
            self.error("declaration is invalid");
            return None;
        }

        let value = self.parse_expression()?;

        Some(Expr::LetStmt {
            identifier,
            value: Box::new(value),
        })
    }

    fn parse_binary_expression(&mut self) -> Option<Expr> {
        let mut left = self.parse_primary()?;

        while let Some(operator) = self.peek().cloned().filter(|token| {
            matches!(
                token,
                Tokens::PLUS
                    | Tokens::MINUS
                    | Tokens::STAR
                    | Tokens::SLASH
                    | Tokens::GREATER
                    | Tokens::GREATEREQUAL
                    | Tokens::LESS
                    | Tokens::LESSEQUAL
                    | Tokens::EQUALEQUAL
                    | Tokens::BANGEQUAL
            )
        }) {
            self.advance();

            let right = self.parse_primary()?;

            left = Expr::BinaryOp {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Some(left)
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        if let Some(&Tokens::EOF) = self.peek() {
            return None;
        }

        if let Some(&Tokens::PRINT) = self.peek() {
            return self.parse_print_statement();
        }

        if let Some(&Tokens::LET) = self.peek() {
            return self.parse_let_statement();
        }

        if let Some(&Tokens::FUN) = self.peek() {
            println!()
        }

        self.parse_binary_expression()
    }

    pub fn parse(&mut self) -> Option<Expr> {
        let mut expressions = Vec::new();

        while let Some(expr) = self.parse_expression() {
            expressions.push(expr);

            if self.is_at_end() {
                break;
            }
        }

        if expressions.is_empty() {
            Some(Expr::Nil)
        } else {
            Some(Expr::Block(expressions))
        }
    }
}
