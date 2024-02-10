use crate::scanner::Tokens;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    LITERAL(Tokens),
    BINARY(Box<Expression>, Tokens, Box<Expression>),
    UNARY(Tokens, Box<Expression>),
    VARIABLE(String),
    CALL(String),
    BLOCK(Vec<Expression>),
    COMPARISON(Box<Expression>, Tokens, Box<Expression>),
    LOGICAL(Box<Expression>, Tokens, Box<Expression>),
}

pub struct Parser {
    tokens: Vec<Tokens>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Tokens>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Vec<Expression> {
        let mut expressions = Vec::new();
        while !self.is_at_end() {
            if let Some(expr) = self.expression() {
                expressions.push(expr);
            }
            self.advance();
        }
        expressions
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
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

    fn match_token(&mut self, expected: Tokens) -> bool {
        if self.is_at_end() {
            return false;
        }
        match &self.tokens[self.current] {
            token if *token == expected => {
                self.current += 1;
                true
            }
            _ => false,
        }
    }

    fn comparison(&mut self) -> Option<Expression> {
        let mut expr = self.addition()?;

        while self.match_token(Tokens::GREATER)
            || self.match_token(Tokens::GREATEREQUAL)
            || self.match_token(Tokens::LESS)
            || self.match_token(Tokens::LESSEQUAL)
            || self.match_token(Tokens::EQUALEQUAL)
            || self.match_token(Tokens::BANGEQUAL)
        {
            let operator = self.previous()?.clone();
            let right = self.addition()?;
            expr = Expression::COMPARISON(Box::new(expr), operator, Box::new(right));
        }

        Some(expr)
    }

    fn logical(&mut self) -> Option<Expression> {
        let mut expr = self.comparison()?;

        while self.match_token(Tokens::AND) || self.match_token(Tokens::OR) {
            let operator = self.previous()?.clone();
            let right = self.comparison()?;
            expr = Expression::LOGICAL(Box::new(expr), operator, Box::new(right));
        }

        Some(expr)
    }

    fn expression(&mut self) -> Option<Expression> {
        if let Some(expr) = self.addition() {
            Some(expr)
        } else {
            self.logical()
        }
    }

    fn addition(&mut self) -> Option<Expression> {
        let mut expr = self.multiplication()?;

        while self.match_token(Tokens::PLUS) || self.match_token(Tokens::MINUS) {
            let operator = self.previous()?.clone();
            let right = self.multiplication()?;
            expr = Expression::BINARY(Box::new(expr), operator, Box::new(right));
        }

        Some(expr)
    }

    fn multiplication(&mut self) -> Option<Expression> {
        let mut expr = self.unary()?;

        while self.match_token(Tokens::STAR) || self.match_token(Tokens::SLASH) {
            let operator = self.previous()?.clone();
            let right = self.unary()?;
            expr = Expression::BINARY(Box::new(expr), operator, Box::new(right));
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<Expression> {
        if self.match_token(Tokens::MINUS) {
            let operator = self.previous()?.clone();
            let right = self.unary()?;
            Some(Expression::UNARY(operator, Box::new(right)))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Expression> {
        let left = match &self.tokens.get(self.current)? {
            Tokens::INTLITERAL(_) | Tokens::FLOATLITERAL(_) => {
                let token = self.tokens[self.current].clone();
                self.current += 1;
                Some(Expression::LITERAL(token))
            }
            _ => None,
        };

        if self.current < self.tokens.len() {
            match &self.tokens[self.current] {
                Tokens::GREATER
                | Tokens::GREATEREQUAL
                | Tokens::LESS
                | Tokens::LESSEQUAL
                | Tokens::EQUALEQUAL
                | Tokens::BANGEQUAL => {
                    let operator = self.tokens[self.current].clone();
                    self.current += 1;
                    let right = self.primary()?;
                    match left {
                        Some(left_expr) => Some(Expression::BINARY(
                            Box::new(left_expr),
                            operator,
                            Box::new(right),
                        )),
                        None => None,
                    }
                }
                _ => left,
            }
        } else {
            left
        }
    }

    fn previous(&self) -> Option<&Tokens> {
        if self.current > 0 {
            Some(&self.tokens[self.current - 1])
        } else {
            None
        }
    }
}
