use crate::{parser::Expr, scanner::Tokens};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Value {
    Function(Vec<String>, Box<Expr>),
}

impl Value {
    pub fn function(parameters: Vec<String>, body: Box<Expr>) -> Self {
        Value::Function(parameters, body)
    }
}

pub struct Interpreter {
    variables: HashMap<String, Expr>,
    functions: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, expr: Expr) -> Result<Expr, String> {
        match expr {
            Expr::Number(value) => Ok(Expr::Number(value)),
            Expr::Float(value) => Ok(Expr::Float(value)),
            Expr::StringLiteral(value) => Ok(Expr::StringLiteral(value)),
            Expr::Variable(name) => {
                if let Some(variable_expr) = self.get_variable_value(&name) {
                    Ok(variable_expr.clone())
                } else if self.get_function_value(&name).is_some() {
                    Ok(Expr::Variable(name))
                } else {
                    Err(format!("variable or function '{}' not declared", name))
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
                    return Err(format!(
                        "variable '{}' has already been declared",
                        identifier_fun
                    ));
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
                if self.functions.contains_key(&name) {
                    return Err(format!("fun '{}' has already been declared", name));
                }

                let function_value = Value::function(parameters.clone(), Box::new(*body.clone()));
                self.functions.insert(name.clone(), function_value);
                Ok(Expr::Nil)
            }
            Expr::Call { callee, arguments } => {
                let callee_expr = self.interpret(*callee)?;
                match callee_expr {
                    Expr::Variable(name) => {
                        let function_value = match self.get_function_value(&name) {
                            Some(value) => value,
                            None => return Err(format!("undefined function '{}'", name)),
                        };

                        let (parameters, body) = match function_value {
                            Value::Function(params, b) => (params, b),
                        };

                        if arguments.len() != parameters.len() {
                            return Err(format!(
                                "wrong number of arguments for function '{}'",
                                name
                            ));
                        }

                        let mut new_scope = self.variables.clone();
                        for (param, arg) in parameters.iter().zip(arguments) {
                            new_scope.insert(param.clone(), arg.clone());
                        }

                        let mut interpreter = Interpreter {
                            variables: new_scope,
                            functions: self.functions.clone(),
                        };

                        let result = interpreter.interpret(*body.clone())?;

                        Ok(result)
                    }
                    _ => Err("only variables can be called as functions".to_string()),
                }
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

    fn get_variable_value(&self, name: &str) -> Option<&Expr> {
        self.variables.get(name)
    }

    pub fn get_function_value(&self, name: &str) -> Option<&Value> {
        self.functions.get(name)
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
