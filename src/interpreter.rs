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
            Expr::SwapStmt { identifier, value } => {
                let value = self.interpret(*value)?;
                if let Some(variable_expr) = self.variables.get_mut(&identifier) {
                    *variable_expr = value.clone();
                    Ok(Expr::Nil)
                } else {
                    Err(format!("variable '{}' not declared", identifier))
                }
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
                    Tokens::BANG => self.apply_bang(left_value),
                    Tokens::BANGEQUAL => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::BANGEQUAL)
                    }
                    Tokens::EQUALEQUAL => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::EQUALEQUAL)
                    }
                    Tokens::GREATER => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::GREATER)
                    }
                    Tokens::GREATEREQUAL => self.apply_binary_operation(
                        &left_value,
                        &right_value,
                        &Tokens::GREATEREQUAL,
                    ),
                    Tokens::LESS => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::LESS)
                    }
                    Tokens::LESSEQUAL => {
                        self.apply_binary_operation(&left_value, &right_value, &Tokens::LESSEQUAL)
                    }
                    _ => Err(format!("unsupported operator {:?}", operator)),
                }
            }
            Expr::IfStmt {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_value = self.evaluate(*condition)?;
                if self.is_truthy(condition_value) {
                    self.interpret(*then_branch)
                } else if let Some(else_expr) = else_branch {
                    self.interpret(*else_expr)
                } else {
                    Ok(Expr::Nil)
                }
            }
            Expr::PrintStmt(expr) => self.interpret(*expr),
            _ => Err(format!("unsupported expression: {}", expr)),
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
        match operator {
            Tokens::PLUS => self.apply_arithmetic_operation(left, right, |a, b| Ok(a + b)),
            Tokens::MINUS => self.apply_arithmetic_operation(left, right, |a, b| Ok(a - b)),
            Tokens::STAR => self.apply_arithmetic_operation(left, right, |a, b| Ok(a * b)),
            Tokens::SLASH => self.apply_arithmetic_operation(left, right, |a, b| {
                if b == 0.0 {
                    Err("division by zero".to_string())
                } else {
                    Ok(a / b)
                }
            }),
            Tokens::BANGEQUAL
            | Tokens::EQUALEQUAL
            | Tokens::GREATER
            | Tokens::GREATEREQUAL
            | Tokens::LESS
            | Tokens::LESSEQUAL => self.apply_comparison_operation(left, right, operator),
            _ => Err(format!("unsupported operator {:?}", operator)),
        }
    }

    fn apply_arithmetic_operation<F>(
        &mut self,
        left: &Expr,
        right: &Expr,
        operation: F,
    ) -> Result<Expr, String>
    where
        F: Fn(f64, f64) -> Result<f64, String>,
    {
        let left_value = self.evaluate(left.clone())?;
        let right_value = self.evaluate(right.clone())?;

        match (left_value, right_value) {
            (Expr::Number(left_num), Expr::Number(right_num)) => {
                operation(left_num as f64, right_num as f64)
                    .map(|result| Expr::Number(result as i64))
            }
            (Expr::Number(left_num), Expr::Float(right_num)) => {
                operation(left_num as f64, right_num).map(|result| Expr::Number(result as i64))
            }
            (Expr::Float(left_num), Expr::Number(right_num)) => {
                operation(left_num, right_num as f64).map(|result| Expr::Number(result as i64))
            }
            (Expr::Float(left_num), Expr::Float(right_num)) => {
                operation(left_num, right_num).map(|result| Expr::Number(result as i64))
            }
            _ => Err("arithmetic operation expects numeric operands".to_string()),
        }
    }

    fn apply_comparison_operation(
        &mut self,
        left: &Expr,
        right: &Expr,
        operator: &Tokens,
    ) -> Result<Expr, String> {
        let left_value = self.evaluate(left.clone())?;
        let right_value = self.evaluate(right.clone())?;

        match (left_value, right_value, operator) {
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::BANGEQUAL) => {
                Ok(Expr::Number((left_num != right_num) as i64))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::EQUALEQUAL) => {
                Ok(Expr::Number((left_num == right_num) as i64))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::GREATER) => {
                Ok(Expr::Number((left_num > right_num) as i64))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::GREATEREQUAL) => {
                Ok(Expr::Number((left_num >= right_num) as i64))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::LESS) => {
                Ok(Expr::Number((left_num < right_num) as i64))
            }
            (Expr::Number(left_num), Expr::Number(right_num), Tokens::LESSEQUAL) => {
                Ok(Expr::Number((left_num <= right_num) as i64))
            }
            (Expr::StringLiteral(left_str), Expr::StringLiteral(right_str), Tokens::BANGEQUAL) => {
                Ok(Expr::Number((left_str != right_str) as i64))
            }
            (Expr::StringLiteral(left_str), Expr::StringLiteral(right_str), Tokens::EQUALEQUAL) => {
                Ok(Expr::Number((left_str == right_str) as i64))
            }
            (Expr::StringLiteral(left_str), Expr::StringLiteral(right_str), Tokens::GREATER) => {
                Ok(Expr::Number((left_str > right_str) as i64))
            }
            (
                Expr::StringLiteral(left_str),
                Expr::StringLiteral(right_str),
                Tokens::GREATEREQUAL,
            ) => Ok(Expr::Number((left_str >= right_str) as i64)),
            (Expr::StringLiteral(left_str), Expr::StringLiteral(right_str), Tokens::LESS) => {
                Ok(Expr::Number((left_str < right_str) as i64))
            }
            (Expr::StringLiteral(left_str), Expr::StringLiteral(right_str), Tokens::LESSEQUAL) => {
                Ok(Expr::Number((left_str <= right_str) as i64))
            }
            _ => Err("comparison operation expects operands of compatible types".to_string()),
        }
    }

    fn is_truthy(&self, expr: Expr) -> bool {
        match expr {
            Expr::Nil => false,
            Expr::Number(num) if num == 0 => false,
            Expr::Float(num) if num == 0.0 => false,
            Expr::StringLiteral(s) if s.is_empty() => false,
            _ => true,
        }
    }
}
