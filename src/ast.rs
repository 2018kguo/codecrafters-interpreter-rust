use std::fmt;
use std::sync::{Arc, RwLock};

use crate::native_funcs::NATIVE_FUNCS;
use crate::scanner::UserFunction;
use crate::{
    environment::EnvironmentContext,
    scanner::{Callable, Literal, NativeFunction, Token, TokenType},
};
use anyhow::Result;

#[derive(Debug)]
pub enum ReturnError {
    ReturnValue(Literal),
}

impl fmt::Display for ReturnError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ReturnError")
    }
}

impl std::error::Error for ReturnError {}

pub trait Accept<T> {
    fn accept(&self, visitor: &mut dyn Visitor<T>) -> T;
}

pub trait StmtAccept<T> {
    fn accept(&self, visitor: &mut dyn StmtVisitor<T>) -> T;
}

// STATEMENTS
pub trait StmtVisitor<T> {
    fn visit_expression_stmt(&mut self, stmt: &ExpressionStmt) -> T;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> T;
    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> T;
    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> T;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> T;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> T;
    fn visit_function_stmt(&mut self, stmt: &FunctionStmt) -> T;
    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> T;
}

#[derive(Clone)]
pub enum Stmt {
    Expression(ExpressionStmt),
    Print(PrintStmt),
    Var(VarStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    Function(FunctionStmt),
    Return(ReturnStmt),
}

#[derive(Clone)]
pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
}

#[derive(Clone)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

#[derive(Clone)]
pub struct PrintStmt {
    pub expression: Expr,
}

#[derive(Clone)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

#[derive(Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Clone)]
pub struct FunctionStmt {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Clone)]
pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Option<Expr>,
}

// technically this is supposed to return null but going to just use string as a placeholder
impl StmtAccept<Result<()>> for Stmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        match self {
            Stmt::Expression(e) => e.accept(visitor),
            Stmt::Print(e) => e.accept(visitor),
            Stmt::Var(e) => e.accept(visitor),
            Stmt::Block(e) => e.accept(visitor),
            Stmt::If(e) => e.accept(visitor),
            Stmt::While(e) => e.accept(visitor),
            Stmt::Function(e) => e.accept(visitor),
            Stmt::Return(e) => e.accept(visitor),
        }
    }
}

impl StmtAccept<Result<()>> for ExpressionStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_expression_stmt(self)
    }
}

impl StmtAccept<Result<()>> for PrintStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_print_stmt(self)
    }
}

impl StmtAccept<Result<()>> for VarStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_var_stmt(self)
    }
}

impl StmtAccept<Result<()>> for BlockStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_block_stmt(self)
    }
}

impl StmtAccept<Result<()>> for IfStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_if_stmt(self)
    }
}

impl StmtAccept<Result<()>> for WhileStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_while_stmt(self)
    }
}

impl StmtAccept<Result<()>> for FunctionStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_function_stmt(self)
    }
}

impl StmtAccept<Result<()>> for ReturnStmt {
    fn accept(&self, visitor: &mut dyn StmtVisitor<Result<()>>) -> Result<()> {
        visitor.visit_return_stmt(self)
    }
}

// EXPRESSIONS
#[derive(Clone)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(LiteralExpr),
    Unary(Unary),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Call(Call),
}

impl Accept<String> for Expr {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        match self {
            Expr::Binary(b) => b.accept(visitor),
            Expr::Grouping(g) => g.accept(visitor),
            Expr::Literal(l) => l.accept(visitor),
            Expr::Unary(u) => u.accept(visitor),
            Expr::Variable(v) => v.accept(visitor),
            Expr::Assign(a) => a.accept(visitor),
            Expr::Logical(l) => l.accept(visitor),
            Expr::Call(c) => c.accept(visitor),
        }
    }
}

impl Accept<Result<Literal>> for Expr {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        match self {
            Expr::Binary(b) => b.accept(visitor),
            Expr::Grouping(g) => g.accept(visitor),
            Expr::Literal(l) => l.accept(visitor),
            Expr::Unary(u) => u.accept(visitor),
            Expr::Variable(v) => v.accept(visitor),
            Expr::Assign(a) => a.accept(visitor),
            Expr::Logical(l) => l.accept(visitor),
            Expr::Call(c) => c.accept(visitor),
        }
    }
}

#[derive(Clone)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Accept<String> for Binary {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_binary(self)
    }
}

impl Accept<Result<Literal>> for Binary {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_binary(self)
    }
}

#[derive(Clone)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

impl Accept<String> for Grouping {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_grouping(self)
    }
}

impl Accept<Result<Literal>> for Grouping {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_grouping(self)
    }
}

#[derive(Clone)]
pub struct LiteralExpr {
    pub value: Literal,
}

impl Accept<String> for LiteralExpr {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_literal(self)
    }
}

impl Accept<Result<Literal>> for LiteralExpr {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_literal(self)
    }
}

#[derive(Clone)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Accept<String> for Unary {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_unary(self)
    }
}

impl Accept<Result<Literal>> for Unary {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_unary(self)
    }
}

#[derive(Clone)]
pub struct Variable {
    pub name: Token,
}

impl Accept<String> for Variable {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_variable_expr(self)
    }
}

impl Accept<Result<Literal>> for Variable {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_variable_expr(self)
    }
}

#[derive(Clone)]
pub struct Assign {
    pub name: Token,
    pub value: Box<Expr>,
}

impl Accept<Result<Literal>> for Assign {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_assign(self)
    }
}

impl Accept<String> for Assign {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_assign(self)
    }
}

#[derive(Clone)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Accept<Result<Literal>> for Logical {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_logical(self)
    }
}

impl Accept<String> for Logical {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_logical(self)
    }
}

#[derive(Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

impl Accept<Result<Literal>> for Call {
    fn accept(&self, visitor: &mut dyn Visitor<Result<Literal>>) -> Result<Literal> {
        visitor.visit_call(self)
    }
}

impl Accept<String> for Call {
    fn accept(&self, visitor: &mut dyn Visitor<String>) -> String {
        visitor.visit_call(self)
    }
}

pub trait Visitor<T> {
    fn visit_binary(&mut self, binary: &Binary) -> T;
    fn visit_grouping(&mut self, grouping: &Grouping) -> T;
    fn visit_literal(&mut self, literal: &LiteralExpr) -> T;
    fn visit_unary(&mut self, unary: &Unary) -> T;
    fn visit_variable_expr(&mut self, variable: &Variable) -> T;
    fn visit_assign(&mut self, assign: &Assign) -> T;
    fn visit_logical(&mut self, logical: &Logical) -> T;
    fn visit_call(&mut self, call: &Call) -> T;
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter
    }

    pub fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }

    fn parenthesize(&mut self, name: String, expressions: Vec<&Expr>) -> String {
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
    fn visit_binary(&mut self, binary: &Binary) -> String {
        self.parenthesize(
            binary.operator.lexeme.clone(),
            vec![&binary.left, &binary.right],
        )
    }

    fn visit_grouping(&mut self, grouping: &Grouping) -> String {
        self.parenthesize("group".to_string(), vec![&grouping.expression])
    }

    fn visit_literal(&mut self, literal: &LiteralExpr) -> String {
        if matches!(literal.value, Literal::Nil) {
            return "nil".to_string();
        }

        format!("{}", literal.value)
    }

    fn visit_unary(&mut self, unary: &Unary) -> String {
        self.parenthesize(unary.operator.lexeme.clone(), vec![&unary.right])
    }

    fn visit_variable_expr(&mut self, variable: &Variable) -> String {
        variable.name.lexeme.clone()
    }

    fn visit_assign(&mut self, assign: &Assign) -> String {
        format!("{} = {}", assign.name.lexeme, assign.value.accept(self))
    }

    fn visit_logical(&mut self, logical: &Logical) -> String {
        self.parenthesize(
            logical.operator.lexeme.clone(),
            vec![&logical.left, &logical.right],
        )
    }

    fn visit_call(&mut self, call: &Call) -> String {
        format!(
            "{}({})",
            call.callee.accept(self),
            call.arguments
                .iter()
                .map(|arg| arg.accept(self))
                .collect::<Vec<String>>()
                .join(", ")
        )
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

pub struct Interpreter {
    pub environment_context: EnvironmentContext,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env_context = EnvironmentContext::new();
        // add native functions to the environment
        for func in NATIVE_FUNCS {
            env_context
                .define(
                    func.0,
                    Literal::Callable(Callable::Native(NativeFunction::new(
                        func.2,
                        func.1,
                        func.0.to_string(),
                    ))),
                )
                .unwrap();
        }

        Interpreter {
            environment_context: env_context,
        }
    }

    pub fn new_with_environment(environment_context: EnvironmentContext) -> Self {
        Interpreter {
            environment_context,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<()> {
        for statement in statements {
            self.execute(&statement)?;
        }
        Ok(())
    }

    pub fn interpret_expression(&mut self, expression: Expr) -> Result<Literal> {
        self.evaluate(&expression)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Literal> {
        expr.accept(self)
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        stmt.accept(self)
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

    pub fn execute_block(&mut self, statements: &Vec<Stmt>) -> Result<()> {
        self.environment_context.begin_scope();
        for statement in statements {
            self.execute(statement)?;
        }
        self.environment_context.exit_scope()?;
        Ok(())
    }
}

impl StmtVisitor<Result<()>> for Interpreter {
    fn visit_expression_stmt(&mut self, stmt: &ExpressionStmt) -> Result<()> {
        self.evaluate(&stmt.expression)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Result<()> {
        let value = self.evaluate(&stmt.expression)?;
        println!("{}", stringify_literal(&value));
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Result<()> {
        let mut value = Literal::Nil;
        if let Some(expr) = &stmt.initializer {
            value = self.evaluate(expr)?;
        }

        self.environment_context.define(&stmt.name.lexeme, value)?;
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Result<()> {
        self.execute_block(&stmt.statements)?;
        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Result<()> {
        let condition_value = self.evaluate(&stmt.condition)?;
        if self.is_truthy(&condition_value) {
            self.execute(&stmt.then_branch)?;
        } else if let Some(else_branch) = &stmt.else_branch {
            self.execute(else_branch)?;
        }
        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Result<()> {
        let mut while_condition = self.evaluate(&stmt.condition)?;
        while self.is_truthy(&while_condition) {
            self.execute(&stmt.body)?;
            while_condition = self.evaluate(&stmt.condition)?;
        }
        Ok(())
    }

    fn visit_function_stmt(&mut self, stmt: &FunctionStmt) -> Result<()> {
        let cloned_environment = self.environment_context.clone();
        // Create the environment in the Arc<RwLock>
        let shared_env = Arc::new(RwLock::new(cloned_environment.clone()));

        // Create function with shared environment
        let function = Callable::UserDefined(UserFunction::new(stmt.clone(), shared_env.clone()));

        // Define the function in the shared environment first (for recursion)
        shared_env
            .write()
            .unwrap()
            .define(&stmt.name.lexeme, Literal::Callable(function.clone()))?;

        // Define in current environment
        self.environment_context
            .define(&stmt.name.lexeme, Literal::Callable(function))?;

        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<()> {
        let mut value = Literal::Nil;
        if let Some(expr) = &stmt.value {
            value = self.evaluate(expr)?;
        }
        Err(ReturnError::ReturnValue(value).into())
    }
}

impl Visitor<Result<Literal>> for Interpreter {
    fn visit_literal(&mut self, literal: &LiteralExpr) -> Result<Literal> {
        Ok(literal.value.clone())
    }

    fn visit_grouping(&mut self, grouping: &Grouping) -> Result<Literal> {
        self.evaluate(&grouping.expression)
    }

    fn visit_unary(&mut self, unary: &Unary) -> Result<Literal> {
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

    fn visit_binary(&mut self, binary: &Binary) -> Result<Literal> {
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

    fn visit_variable_expr(&mut self, variable: &Variable) -> Result<Literal> {
        self.environment_context.get(&variable.name)
    }

    fn visit_assign(&mut self, assign: &Assign) -> Result<Literal> {
        let value = self.evaluate(&assign.value)?;
        self.environment_context
            .assign(&assign.name, value.clone())?;
        Ok(value)
    }

    fn visit_logical(&mut self, logical: &Logical) -> Result<Literal> {
        let left = self.evaluate(&logical.left)?;

        if logical.operator.token_type == TokenType::Or {
            if self.is_truthy(&left) {
                return Ok(left);
            }
        } else if !self.is_truthy(&left) {
            return Ok(left);
        }

        self.evaluate(&logical.right)
    }

    fn visit_call(&mut self, call: &Call) -> Result<Literal> {
        let callee = self.evaluate(&call.callee)?;

        let mut arguments = Vec::new();
        for argument in &call.arguments {
            arguments.push(self.evaluate(argument)?);
        }

        match callee {
            Literal::Callable(c) => {
                if arguments.len() != c.arity() {
                    return Err(anyhow::anyhow!(RuntimeError {
                        token: call.paren.clone(),
                        message: format!(
                            "Expected {} arguments but got {}.",
                            c.arity(),
                            arguments.len()
                        ),
                    }));
                }
                c.call(self, &arguments)
            }
            _ => Err(anyhow::anyhow!(RuntimeError {
                token: call.paren.clone(),
                message: "Can only call functions and classes.".to_string(),
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
        let mut ast_printer = AstPrinter::new();
        let ast_string = ast_printer.print(&expression);
        assert_eq!(ast_string, "(* (- 123.0) (group 45.67))");
    }
}
