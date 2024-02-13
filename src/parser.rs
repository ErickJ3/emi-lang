use std::fmt;

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
    SwapStmt {
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Number(value) => write!(f, "{}", value),
            Expr::Float(value) => write!(f, "{}", value),
            Expr::StringLiteral(value) => write!(f, "{}", value),
            Expr::Variable(name) => write!(f, "{}", name),
            Expr::UnaryOp { operator, right } => write!(f, "{}{}", operator, right),
            Expr::BinaryOp {
                left,
                operator,
                right,
            } => {
                write!(f, "{} {} {}", left, operator, right)
            }
            Expr::Grouping(expr) => write!(f, "({})", expr),
            Expr::PrintStmt(expr) => write!(f, "print {}", expr),
            Expr::LetStmt { identifier, value } => {
                write!(f, "let {} = {}", identifier, value)
            }
            Expr::SwapStmt { identifier, value } => {
                write!(f, "{} = {}", identifier, value)
            }
            Expr::IfStmt {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(
                    f,
                    "if {} then {} else {}",
                    condition,
                    then_branch,
                    else_branch.as_ref().unwrap_or(&Box::new(Expr::Nil))
                )
            }
            Expr::Block(expressions) => {
                for expr in expressions {
                    writeln!(f, "{}", expr)?;
                }
                Ok(())
            }
            Expr::WhileStmt { condition, body } => {
                write!(f, "while {} do {}", condition, body)
            }
            Expr::ForStmt {
                initialization,
                condition,
                increment,
                body,
            } => write!(
                f,
                "for {}; {}; {} do {}",
                initialization, condition, increment, body
            ),
            Expr::Function {
                name,
                parameters,
                body,
            } => write!(f, "fun {}({}) {{ {} }}", name, parameters.join(", "), body),
            Expr::Call { callee, arguments } => {
                write!(
                    f,
                    "{}({})",
                    callee,
                    arguments
                        .iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Expr::ReturnStmt(expr) => {
                if let Some(expr) = expr {
                    write!(f, "return {}", expr)
                } else {
                    write!(f, "return")
                }
            }
            Expr::Nil => write!(f, "nil"),
        }
    }
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
                Tokens::IDENTIFIER(name) => {
                    let name_cl = name.clone();
                    self.parse_identifier(&name_cl)
                }
                Tokens::LEFTPAREN => {
                    let expr = self.parse_expression()?;
                    if self.consume(Tokens::RIGHTPAREN).is_none() {
                        self.error("expected ')' after expression.");
                        return None;
                    }
                    Some(Expr::Grouping(Box::new(expr)))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn parse_identifier(&mut self, name: &str) -> Option<Expr> {
        if let Some(&Tokens::ASSIGNMENT) = self.peek() {
            self.advance();
            let value = self.parse_expression()?;
            Some(Expr::SwapStmt {
                identifier: name.to_owned(),
                value: Box::new(value),
            })
        } else {
            Some(Expr::Variable(name.to_owned()))
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

    fn parse_function_declaration(&mut self) -> Option<Expr> {
        if self.consume(Tokens::FUN).is_none() {
            self.error("expected 'fun' keyword for function declaration.");
            return None;
        }

        let name = match self.peek() {
            Some(&Tokens::IDENTIFIER(ref name)) => name.clone(),
            _ => {
                self.error("expected function name after 'fun' keyword.");
                return None;
            }
        };

        self.advance();

        if self.consume(Tokens::LEFTPAREN).is_none() {
            self.error("expected '(' after function name.");
            return None;
        }

        let mut parameters = Vec::new();
        while let Some(&Tokens::IDENTIFIER(ref param)) = self.peek() {
            parameters.push(param.clone());
            self.advance();
            if self.peek() != Some(&Tokens::COMMA) {
                break;
            }
            self.advance();
        }

        if self.consume(Tokens::RIGHTPAREN).is_none() {
            self.error("expected ')' after function parameters.");
            return None;
        }

        if self.consume(Tokens::COLON).is_none() {
            self.error("expected ':' after function parameters.");
            return None;
        }

        let body = match self.parse() {
            Some(Expr::Block(body)) => body,
            Some(expr) => vec![expr],
            None => {
                self.error("expected function body.");
                return None;
            }
        };

        Some(Expr::Function {
            name,
            parameters,
            body: Box::new(Expr::Block(body)),
        })
    }

    fn parse_call_expression(&mut self, callee: Expr) -> Option<Expr> {
        if self.consume(Tokens::LEFTPAREN).is_none() {
            self.error("expected '(' after function name.");
            return None;
        }

        let mut arguments = Vec::new();
        loop {
            if let Some(&Tokens::RIGHTPAREN) = self.peek() {
                break;
            }

            if let Some(expr) = self.parse_expression() {
                arguments.push(expr);
            } else {
                self.error("expected expression inside function call.");
                return None;
            }

            if self.peek() != Some(&Tokens::COMMA) {
                break;
            }
            self.advance();
        }

        if self.consume(Tokens::RIGHTPAREN).is_none() {
            self.error("expected ')' after function arguments.");
            return None;
        }

        Some(Expr::Call {
            callee: Box::new(callee),
            arguments,
        })
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
        match self.peek() {
            Some(&Tokens::EOF) => None,
            Some(&Tokens::PRINT) => self.parse_print_statement(),
            Some(&Tokens::LET) => self.parse_let_statement(),
            Some(&Tokens::FUN) => self.parse_function_declaration(),
            Some(&Tokens::IDENTIFIER(_))
                if self.tokens.get(self.current + 1) == Some(&Tokens::LEFTPAREN) =>
            {
                let callee_expr = self.parse_primary()?;
                self.parse_call_expression(callee_expr)
            }
            Some(&Tokens::MINUS) => {
                self.advance();
                let expr = self.parse_expression()?;
                Some(Expr::UnaryOp {
                    operator: Tokens::MINUS,
                    right: Box::new(expr),
                })
            }
            Some(&Tokens::PLUS) => {
                self.advance();
                let expr = self.parse_expression()?;
                Some(expr)
            }
            _ => self.parse_binary_expression(),
        }
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
