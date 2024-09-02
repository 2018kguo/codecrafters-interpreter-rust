use std::fmt;

use anyhow::Result;

use crate::scanner::{Literal, Token, TokenType};

pub trait Accept<T> {
    fn accept(&self, visitor: &dyn Visitor<T>) -> T;
}

pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(LiteralExpr),
    Unary(Unary),
}

impl Accept<String> for Expr {
    fn accept(&self, visitor: &dyn Visitor<String>) -> String {
        match self {
            Expr::Binary(b) => b.accept(visitor),
            Expr::Grouping(g) => g.accept(visitor),
            Expr::Literal(l) => l.accept(visitor),
            Expr::Unary(u) => u.accept(visitor),
        }
    }
}

impl Accept<Result<Literal>> for Expr {
    fn accept(&self, visitor: &dyn Visitor<Result<Literal>>) -> Result<Literal> {
        match self {
            Expr::Binary(b) => b.accept(visitor),
            Expr::Grouping(g) => g.accept(visitor),
            Expr::Literal(l) => l.accept(visitor),
            Expr::Unary(u) => u.accept(visitor),
        }
    }
}

pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Accept<String> for Binary {
    fn accept(&self, visitor: &dyn Visitor<String>) -> String {
        visitor.visit_binary(self)
    }
}

impl Accept<Result<Literal>> for Binary {
    fn accept(&self, visitor: &dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_binary(self)
    }
}

pub struct Grouping {
    pub expression: Box<Expr>,
}

impl Accept<String> for Grouping {
    fn accept(&self, visitor: &dyn Visitor<String>) -> String {
        visitor.visit_grouping(self)
    }
}

impl Accept<Result<Literal>> for Grouping {
    fn accept(&self, visitor: &dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_grouping(self)
    }
}

pub struct LiteralExpr {
    pub value: Literal,
}

impl Accept<String> for LiteralExpr {
    fn accept(&self, visitor: &dyn Visitor<String>) -> String {
        visitor.visit_literal(self)
    }
}

impl Accept<Result<Literal>> for LiteralExpr {
    fn accept(&self, visitor: &dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_literal(self)
    }
}

pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Accept<String> for Unary {
    fn accept(&self, visitor: &dyn Visitor<String>) -> String {
        visitor.visit_unary(self)
    }
}

impl Accept<Result<Literal>> for Unary {
    fn accept(&self, visitor: &dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_unary(self)
    }
}

pub trait Visitor<T> {
    fn visit_binary(&self, binary: &Binary) -> T;
    fn visit_grouping(&self, grouping: &Grouping) -> T;
    fn visit_literal(&self, literal: &LiteralExpr) -> T;
    fn visit_unary(&self, unary: &Unary) -> T;
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter
    }

    pub fn print(&self, expr: &Expr) -> String {
        expr.accept(self)
    }

    fn parenthesize(&self, name: String, expressions: Vec<&Expr>) -> String {
        let mut result = String::new();
        result.push('(');
        result.push_str(&name);
        for expr in expressions {
            result.push(' ');
            result.push_str(&expr.accept(self));
        }
        result.push(')');
        result
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary(&self, binary: &Binary) -> String {
        self.parenthesize(
            binary.operator.lexeme.clone(),
            vec![&binary.left, &binary.right],
        )
    }

    fn visit_grouping(&self, grouping: &Grouping) -> String {
        self.parenthesize("group".to_string(), vec![&grouping.expression])
    }

    fn visit_literal(&self, literal: &LiteralExpr) -> String {
        if matches!(literal.value, Literal::Nil) {
            return "nil".to_string();
        }

        format!("{}", literal.value)
    }

    fn visit_unary(&self, unary: &Unary) -> String {
        self.parenthesize(unary.operator.lexeme.clone(), vec![&unary.right])
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

// Implement std::fmt::Display for RuntimeError
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at line {}", self.message, self.token.line,)
    }
}

// Implement std::error::Error for RuntimeError
impl std::error::Error for RuntimeError {}

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn interpret(&self, expression: Expr) -> Result<Literal> {
        self.evaluate(&expression)
    }

    fn evaluate(&self, expr: &Expr) -> Result<Literal> {
        expr.accept(self)
    }

    fn is_truthy(&self, literal: &Literal) -> bool {
        match literal {
            Literal::Nil => false,
            Literal::Boolean(b) => *b,
            _ => true,
        }
    }

    fn equals(&self, a: &Literal, b: &Literal) -> bool {
        if matches!(a, Literal::Nil) && matches!(b, Literal::Nil) {
            return true;
        }
        if matches!(a, Literal::Nil) {
            return false;
        }
        a == b
    }
}

impl Visitor<Result<Literal>> for Interpreter {
    fn visit_literal(&self, literal: &LiteralExpr) -> Result<Literal> {
        Ok(literal.value.clone())
    }

    fn visit_grouping(&self, grouping: &Grouping) -> Result<Literal> {
        self.evaluate(&grouping.expression)
    }

    fn visit_unary(&self, unary: &Unary) -> Result<Literal> {
        let right = self.evaluate(&unary.right)?;

        match unary.operator.token_type {
            TokenType::Minus => match right {
                Literal::Number(n) => Ok(Literal::Number(-n)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: unary.operator.clone(),
                    message: "Operand must be a number.".to_string(),
                })),
            },
            TokenType::Bang => Ok(Literal::Boolean(!self.is_truthy(&right))), 
            _ => Err(anyhow::anyhow!(RuntimeError {
                token: unary.operator.clone(),
                message: "Invalid unary operator.".to_string(),
            })),
        }
    }

    fn visit_binary(&self, binary: &Binary) -> Result<Literal> {
        let left = self.evaluate(&binary.left)?;
        let right = self.evaluate(&binary.right)?;

        match binary.operator.token_type {
            TokenType::Minus => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l - r)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be numbers.".to_string(),
                })),
            },
            TokenType::Slash => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l / r)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be numbers.".to_string(),
                })),
            },
            TokenType::Star => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l * r)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be numbers.".to_string(),
                })),
            },
            TokenType::Plus => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Number(l + r)),
                (Literal::String(l), Literal::String(r)) => {
                    Ok(Literal::String(format!("{}{}", l, r)))
                }
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be two numbers or two strings.".to_string(),
                })),
            },
            TokenType::Greater => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l > r)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be numbers.".to_string(),
                })),
            },
            TokenType::GreaterEqual => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l >= r)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be numbers.".to_string(),
                })),
            },
            TokenType::Less => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l < r)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be numbers.".to_string(),
                })),
            },
            TokenType::LessEqual => match (left, right) {
                (Literal::Number(l), Literal::Number(r)) => Ok(Literal::Boolean(l <= r)),
                _ => Err(anyhow::anyhow!(RuntimeError {
                    token: binary.operator.clone(),
                    message: "Operands must be numbers.".to_string(),
                })),
            },
            TokenType::BangEqual => Ok(Literal::Boolean(!self.equals(&left, &right))),
            TokenType::EqualEqual => Ok(Literal::Boolean(self.equals(&left, &right))),
            _ => Err(anyhow::anyhow!(RuntimeError {
                token: binary.operator.clone(),
                message: "Invalid binary operator.".to_string(),
            })),
        }
    }
}

pub fn stringify_literal(literal: &Literal) -> String {
    if matches!(literal, Literal::Number(_)) {
        let string = format!("{}", literal);
        if string.ends_with(".0") {
            return string.replace(".0", "");
        }
    }
    literal.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::TokenType;

    #[test]
    fn test_ast_printer() {
        let expression = Expr::Binary(Binary {
            left: Box::new(Expr::Unary(Unary {
                operator: Token {
                    token_type: TokenType::Minus,
                    lexeme: "-".to_string(),
                    literal: None,
                    line: 1,
                },
                right: Box::new(Expr::Literal(LiteralExpr {
                    value: Literal::Number(123.0),
                })),
            })),
            operator: Token {
                token_type: TokenType::Star,
                lexeme: "*".to_string(),
                literal: None,
                line: 1,
            },
            right: Box::new(Expr::Grouping(Grouping {
                expression: Box::new(Expr::Literal(LiteralExpr {
                    value: Literal::Number(45.67),
                })),
            })),
        });
        let ast_printer = AstPrinter::new();
        let ast_string = ast_printer.print(&expression);
        assert_eq!(ast_string, "(* (- 123.0) (group 45.67))");
    }
}
