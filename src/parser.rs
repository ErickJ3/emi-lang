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
        while let Some(&Tokens::NEWLINE) = self.peek() {
            self.advance();
        }

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
        eprintln!("Error: {}", message);
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

    fn parse_unary(&mut self) -> Option<Expr> {
        let primary_expr = self.parse_primary()?;

        if let Some(operator) = self.consume(Tokens::BANG) {
            let right = Box::new(primary_expr);
            Some(Expr::UnaryOp {
                operator: operator.clone(),
                right,
            })
        } else if let Some(operator) = self.consume(Tokens::MINUS) {
            let right = Box::new(primary_expr);
            Some(Expr::UnaryOp {
                operator: operator.clone(),
                right,
            })
        } else {
            Some(primary_expr)
        }
    }

    fn parse_binary(&mut self, higher_precedence: fn(&Tokens) -> bool) -> Option<Expr> {
        let mut expr = self.parse_unary()?;

        while let Some(token) = self.peek() {
            if higher_precedence(token) {
                let operator = self.advance().unwrap().clone();
                let right = self.parse_unary()?;
                expr = self.finish_binary(expr, operator, right);
            } else {
                break;
            }
        }

        Some(expr)
    }

    fn parse_print_statement(&mut self) -> Option<Expr> {
        if self.consume(Tokens::PRINT).is_none() {
            self.error("expected 'print' keyword.");
            return None;
        }

        let expr = self.parse_expression()?;

        Some(Expr::PrintStmt(Box::new(expr)))
    }

    fn finish_binary(&self, left: Expr, operator: Tokens, right: Expr) -> Expr {
        Expr::BinaryOp {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        match self.peek() {
            Some(&Tokens::PRINT) => self.parse_print_statement(),
            _ => self.parse_binary(|token| {
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
            })
        }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        self.parse_expression()
    }
}
