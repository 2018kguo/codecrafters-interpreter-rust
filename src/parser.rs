use anyhow::Result;

use crate::ast::{
    Assign, Binary, BlockStmt, ExpressionStmt, Grouping, LiteralExpr, PrintStmt, Stmt, Unary,
    VarStmt, Variable,
};
use crate::{
    ast::Expr,
    scanner::{Literal, Token, TokenType},
};

pub struct Parser {
    pub tokens: Vec<Token>,
    pub current: usize,
    pub had_error: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
            had_error: false,
        }
    }

    fn match_tokens(&mut self, token_types: Vec<TokenType>) -> bool {
        for token_type in token_types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            let declaration = self.declaration();
            if declaration.is_err() {
                self.had_error = true;
                self.synchronize();
                continue;
            } else {
                statements.push(declaration?);
            }
        }
        Ok(statements)
    }

    pub fn parse_expression(&mut self) -> Result<Expr> {
        self.expression()
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token> {
        if self.check(token_type) {
            return Ok(self.advance());
        }

        let token = self.peek().clone();
        self.error(&token, message);
        Err(anyhow::anyhow!(message.to_string()))
    }

    fn error(&mut self, token: &Token, message: &str) {
        if token.token_type == TokenType::Eof {
            self.report(token.line, " at end", message);
        } else {
            self.report(
                token.line,
                format!(" at '{}'", token.lexeme).as_str(),
                message,
            );
        }
    }

    fn report(&mut self, line: usize, location: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", line, location, message);
        self.had_error = true;
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self.match_tokens(vec![TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self
            .consume(TokenType::Identifier, "Expect variable name.")?
            .clone();

        let initializer = if self.match_tokens(vec![TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::Var(VarStmt {
            name: name.clone(),
            initializer,
        }))
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.match_tokens(vec![TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_tokens(vec![TokenType::LeftBrace]) {
            return self.block();
        }
        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print(PrintStmt { expression: value }))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Expression(ExpressionStmt { expression: value }))
    }

    fn block(&mut self) -> Result<Stmt> {
        let mut statements: Vec<Stmt> = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            let declaration = self.declaration()?;
            statements.push(declaration);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(Stmt::Block(BlockStmt { statements }))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.equality()?;

        if self.match_tokens(vec![TokenType::Equal]) {
            let equals = self.previous().clone();
            // recursively parse the left side, check if its a variable afterwards.
            // we first parse the right side before we check the left side in order
            // to report on the right side first if there is an error.
            let value = self.assignment()?;

            if let Expr::Variable(var) = expr {
                return Ok(Expr::Assign(Assign {
                    name: var.name,
                    value: Box::new(value),
                }));
            }

            self.error(&equals, "Invalid assignment target.");
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while self.match_tokens(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while self.match_tokens(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while self.match_tokens(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while self.match_tokens(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.match_tokens(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary {
                operator,
                right: Box::new(right),
            }));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr> {
        if self.match_tokens(vec![TokenType::False]) {
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::Boolean(false),
            }));
        }
        if self.match_tokens(vec![TokenType::True]) {
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::Boolean(true),
            }));
        }
        if self.match_tokens(vec![TokenType::Nil]) {
            return Ok(Expr::Literal(LiteralExpr {
                value: Literal::Nil,
            }));
        }

        if self.match_tokens(vec![TokenType::Number, TokenType::String]) {
            return Ok(Expr::Literal(LiteralExpr {
                value: self.previous().literal.as_ref().unwrap().clone(),
            }));
        }

        if self.match_tokens(vec![TokenType::Identifier]) {
            return Ok(Expr::Variable(Variable {
                name: self.previous().clone(),
            }));
        }

        if self.match_tokens(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Grouping {
                expression: Box::new(expr),
            }));
        }

        self.had_error = true;
        Err(anyhow::anyhow!("Expect expression."))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }
}
