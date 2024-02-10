use crate::parser::Expression;
use crate::scanner::Tokens;

pub struct Interpreter;

impl Interpreter {
    pub fn interpret(expressions: Vec<Expression>) -> Option<f64> {
        for expr in expressions {
            let result = Interpreter::evaluate_expression(expr);
            if let Some(value) = result {
                println!("result: {}", value);
            } else {
                println!("error: Unable to interpret expression.");
                return None;
            }
        }
        None
    }

    fn evaluate_expression(expression: Expression) -> Option<f64> {
        match expression {
            Expression::LITERAL(token) => match token {
                Tokens::INTLITERAL(value) => Some(value as f64),
                Tokens::FLOATLITERAL(value) => Some(value),
                _ => {
                    println!("error: Unsupported literal token.");
                    None
                }
            },
            Expression::BINARY(left, operator, right) => {
                let left_val = Interpreter::evaluate_expression(*left)?;
                let right_val = Interpreter::evaluate_expression(*right)?;
                match operator {
                    Tokens::PLUS => Some(left_val + right_val),
                    Tokens::MINUS => Some(left_val - right_val),
                    Tokens::STAR => Some(left_val * right_val),
                    Tokens::SLASH => {
                        if right_val != 0.0 {
                            Some(left_val / right_val)
                        } else {
                            println!("error: Division by zero.");
                            None
                        }
                    }
                    _ => {
                        println!("error: Unsupported binary operator.");
                        None
                    }
                }
            }
            Expression::UNARY(operator, expr) => {
                let value = Interpreter::evaluate_expression(*expr)?;
                match operator {
                    Tokens::MINUS => Some(-value),
                    _ => {
                        println!("error: Unsupported unary operator.");
                        None
                    }
                }
            }
            _ => {
                println!("error: Unsupported expression type.");
                None
            }
        }
    }
}
