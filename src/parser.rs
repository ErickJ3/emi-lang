use crate::scanner::Token;

#[derive(Debug, PartialEq)]
pub enum AstNode {
    Program(Vec<AstNode>),
    FunctionDeclaration(String, Vec<AstNode>),
    PrintStatement(String),
    VariableDeclaration(String, VariableValue),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
}

#[derive(Debug, PartialEq)]
pub enum VariableValue {
    Str(String),
    Int(i64),
    Float(f64),
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> AstNode {
        self.program()
    }

    fn program(&mut self) -> AstNode {
        let mut program = vec![];

        while !self.is_at_end() {
            program.push(self.declaration());
        }

        AstNode::Program(program)
    }

    fn declaration(&mut self) -> AstNode {
        match self.peek().clone() {
            Token::Fun => self.function_declaration(),
            Token::Let => self.variable_declaration(),
            _ => panic!("Unexpected token: {:?}", self.peek()),
        }
    }

    fn function_declaration(&mut self) -> AstNode {
        self.consume(Token::Fun);
        let name = self.consume_identifier();
        self.consume(Token::LeftParenthesis);
        self.consume(Token::RightParenthesis);
        self.consume(Token::Colon);

        let mut body = Vec::new();
        if self.peek() != &Token::End {
            body.push(self.print_statement());
            self.consume(Token::End);
        }

        AstNode::FunctionDeclaration(name, body)
    }

    fn print_statement(&mut self) -> AstNode {
        self.consume(Token::Print);
        let message = match self.advance() {
            Token::StringLiteral(s) => s,
            _ => panic!("Expected string literal in print statement"),
        };
        AstNode::PrintStatement(message)
    }

    fn variable_declaration(&mut self) -> AstNode {
        self.consume(Token::Let);
        let name = self.consume_identifier();
        let value = match self.advance() {
            Token::StringLiteral(s) => VariableValue::Str(s),
            Token::IntLiteral(i) => VariableValue::Int(i),
            Token::FloatLiteral(f) => VariableValue::Float(f),
            _ => panic!("Expected value for variable declaration"),
        };
        AstNode::VariableDeclaration(name, value)
    }

    fn consume_identifier(&mut self) -> String {
        match self.advance() {
            Token::Identifier(identifier) => identifier,
            _ => panic!("Expected identifier"),
        }
    }

    fn consume(&mut self, expected: Token) {
        if self.peek() == &expected {
            self.advance();
        } else {
            panic!("Expected {:?} but found {:?}", expected, self.peek());
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }
}
