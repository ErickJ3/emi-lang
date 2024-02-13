use crate::{parser::Expr, scanner::Tokens};

pub struct Interpreter {
    variables: std::collections::HashMap<String, Expr>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: std::collections::HashMap::new(),
        }
    }

    pub fn interpret(&mut self, expr: Expr) -> Result<Expr, String> {
        match expr {
            Expr::Number(value) => Ok(Expr::Number(value)),
            Expr::Float(value) => Ok(Expr::Float(value)),
            Expr::StringLiteral(value) => Ok(Expr::StringLiteral(value)),
            Expr::Variable(name) => {
                if let Some(function_expr) = self.get_function_value(&name) {
                    Ok(function_expr.clone())
                } else {
                    Err(format!("variable '{}' not declared", name))
                }
            }
            Expr::Block(expressions) => {
                let mut results = Vec::new();
                for expr in expressions {
                    results.push(self.interpret(expr)?);
                }
                Ok(Expr::Block(results))
            }
            Expr::Grouping(expr) => self.interpret(*expr),
            Expr::UnaryOp { operator, right } => {
                let right_value = self.evaluate(*right)?;
                match operator {
                    Tokens::MINUS => self.apply_unary_minus(right_value),
                    Tokens::BANG => self.apply_bang(right_value),
                    _ => Err("invalid unary operator".to_string()),
                }
            }
            Expr::LetStmt { identifier, value } => {
                let identifier_fun = identifier.clone();

                if self.variables.contains_key(&identifier_fun) {
                    return Err(format!("variable '{}' has already been declared", identifier_fun));
                }

                let value = self.interpret(*value)?;
                self.variables.insert(identifier.clone(), value.clone());
                Ok(Expr::Nil)
            }
            Expr::Function {
                name,
                parameters,
                body,
            } => {
                println!("{}, {:?}, {:?}", name, parameters, body);

                Ok(Expr::Nil)
            }
            Expr::BinaryOp {
                left,
                operator,
                right,
            } => {
                let left_value = self.evaluate(*left)?;
                let right_value = self.evaluate(*right)?;

                match operator {
                    Tokens::PLUS => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::PLUS)
                    }
                    Tokens::MINUS => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::MINUS)
                    }
                    Tokens::STAR => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::STAR)
                    }
                    Tokens::SLASH => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::SLASH)
                    }
                    _ => Err("unsupported operator".to_string()),
                }
            }
            Expr::PrintStmt(expr) => self.interpret(*expr),
            _ => Err("unsupported expression".to_string()),
        }
    }

    pub fn evaluate(&mut self, expr: Expr) -> Result<Expr, String> {
        self.interpret(expr)
    }

    pub fn get_function_value(&self, name: &str) -> Option<&Expr> {
        self.variables.get(name)
    }

    fn apply_unary_minus(&self, expr: Expr) -> Result<Expr, String> {
        match expr {
            Expr::Number(num) => Ok(Expr::Number(-num)),
            Expr::Float(num) => Ok(Expr::Float(-num)),
            _ => Err("unary minus can only be applied to numbers".to_string()),
        }
    }

    fn apply_bang(&self, expr: Expr) -> Result<Expr, String> {
        Ok(Expr::Number(if self.is_truthy(expr) { 0 } else { 1 }))
    }

    fn apply_binary_operation(
        &mut self,
        left: &Expr,
        right: &Expr,
        operator: &Tokens,
    ) -> Result<Expr, String> {
        let left_value = self.interpret(left.clone())?;
        let right_value = self.interpret(right.clone())?;

        match (left_value, right_value, operator) {
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::PLUS) => {
                Ok(Expr::Number(left_num + right_num))
            }
            (Expr::Number(left_num), Expr::Float(right_num), Tokens::PLUS) => {
                Ok(Expr::Float(left_num as f64 + right_num))
            }
            (Expr::Float(left_num), Expr::Number(right_num), Tokens::PLUS) => {
                Ok(Expr::Float(left_num + right_num as f64))
            }
            (Expr::Float(left_num), Expr::Float(right_num), Tokens::PLUS) => {
                Ok(Expr::Float(left_num + right_num))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::MINUS) => {
                Ok(Expr::Number(left_num - right_num))
            }
            (Expr::Number(left_num), Expr::Float(right_num), Tokens::MINUS) => {
                Ok(Expr::Float(left_num as f64 - right_num))
            }
            (Expr::Float(left_num), Expr::Number(right_num), Tokens::MINUS) => {
                Ok(Expr::Float(left_num - right_num as f64))
            }
            (Expr::Float(left_num), Expr::Float(right_num), Tokens::MINUS) => {
                Ok(Expr::Float(left_num - right_num))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::STAR) => {
                Ok(Expr::Number(left_num * right_num))
            }
            (Expr::Number(left_num), Expr::Float(right_num), Tokens::STAR) => {
                Ok(Expr::Float(left_num as f64 * right_num))
            }
            (Expr::Float(left_num), Expr::Number(right_num), Tokens::STAR) => {
                Ok(Expr::Float(left_num * right_num as f64))
            }
            (Expr::Float(left_num), Expr::Float(right_num), Tokens::STAR) => {
                Ok(Expr::Float(left_num * right_num))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::SLASH) => {
                if right_num == 0 {
                    Err("division by zero".to_string())
                } else {
                    Ok(Expr::Number(left_num / right_num))
                }
            }
            (Expr::Number(left_num), Expr::Float(right_num), Tokens::SLASH) => {
                if right_num == 0.0 {
                    Err("division by zero".to_string())
                } else {
                    Ok(Expr::Float(left_num as f64 / right_num))
                }
            }
            (Expr::Float(left_num), Expr::Number(right_num), Tokens::SLASH) => {
                if right_num == 0 {
                    Err("division by zero".to_string())
                } else {
                    Ok(Expr::Float(left_num / right_num as f64))
                }
            }
            (Expr::Float(left_num), Expr::Float(right_num), Tokens::SLASH) => {
                if right_num == 0.0 {
                    Err("division by zero".to_string())
                } else {
                    Ok(Expr::Float(left_num / right_num))
                }
            }
            _ => Err("unsupported operator".to_string()),
        }
    }

    fn is_truthy(&self, _expr: Expr) -> bool {
        true
    }
}
