#![allow(dead_code)]
use anyhow::Result;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use crate::ast::{FunctionStmt, Interpreter, ReturnError};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut m = HashMap::new();
        m.insert("and", TokenType::And);
        m.insert("class", TokenType::Class);
        m.insert("else", TokenType::Else);
        m.insert("false", TokenType::False);
        m.insert("for", TokenType::For);
        m.insert("fun", TokenType::Fun);
        m.insert("if", TokenType::If);
        m.insert("nil", TokenType::Nil);
        m.insert("or", TokenType::Or);
        m.insert("print", TokenType::Print);
        m.insert("return", TokenType::Return);
        m.insert("super", TokenType::Super);
        m.insert("this", TokenType::This);
        m.insert("true", TokenType::True);
        m.insert("var", TokenType::Var);
        m.insert("while", TokenType::While);
        m
    };
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let repr = match self {
            TokenType::LeftParen => "LEFT_PAREN",
            TokenType::RightParen => "RIGHT_PAREN",
            TokenType::LeftBrace => "LEFT_BRACE",
            TokenType::RightBrace => "RIGHT_BRACE",
            TokenType::Comma => "COMMA",
            TokenType::Dot => "DOT",
            TokenType::Minus => "MINUS",
            TokenType::Plus => "PLUS",
            TokenType::Semicolon => "SEMICOLON",
            TokenType::Slash => "SLASH",
            TokenType::Star => "STAR",
            TokenType::Bang => "BANG",
            TokenType::BangEqual => "BANG_EQUAL",
            TokenType::Equal => "EQUAL",
            TokenType::EqualEqual => "EQUAL_EQUAL",
            TokenType::Greater => "GREATER",
            TokenType::GreaterEqual => "GREATER_EQUAL",
            TokenType::Less => "LESS",
            TokenType::LessEqual => "LESS_EQUAL",
            TokenType::Identifier => "IDENTIFIER",
            TokenType::String => "STRING",
            TokenType::Number => "NUMBER",
            TokenType::And => "AND",
            TokenType::Class => "CLASS",
            TokenType::Else => "ELSE",
            TokenType::False => "FALSE",
            TokenType::Fun => "FUN",
            TokenType::For => "FOR",
            TokenType::If => "IF",
            TokenType::Nil => "NIL",
            TokenType::Or => "OR",
            TokenType::Print => "PRINT",
            TokenType::Return => "RETURN",
            TokenType::Super => "SUPER",
            TokenType::This => "THIS",
            TokenType::True => "TRUE",
            TokenType::Var => "VAR",
            TokenType::While => "WHILE",
            TokenType::Eof => "EOF",
        };
        write!(f, "{}", repr)
    }
}

pub type LoxFuncPtr = fn(&Interpreter, &Vec<Literal>) -> Result<Literal>;

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    UserDefined(UserFunction),
    Native(NativeFunction),
}

impl Callable {
    pub fn arity(&self) -> usize {
        match self {
            Callable::Native(native) => native.arity,
            Callable::UserDefined(user) => user.func_declaration.params.len(),
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Literal>) -> Result<Literal> {
        match self {
            Callable::Native(native) => native.call(interpreter, arguments),
            Callable::UserDefined(user) => user.call(interpreter, arguments),
        }
    }

    pub fn name(&self) -> String {
        match self {
            Callable::Native(native) => native.name.clone(),
            Callable::UserDefined(user) => user.func_declaration.name.lexeme.clone(),
        }
    }
}

#[derive(Clone)]
pub struct UserFunction {
    pub func_declaration: Box<FunctionStmt>,
    pub closure: Closure,
}

impl PartialEq for UserFunction {
    fn eq(&self, other: &Self) -> bool {
        self.func_declaration.name.lexeme == other.func_declaration.name.lexeme
    }
}

impl fmt::Debug for UserFunction {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "<fn {}>", self.func_declaration.name.lexeme)
    }
}

#[derive(Clone)]
pub struct Closure {
    environment_id: usize,
}

impl Closure {
    pub fn new(environment_id: usize) -> Closure {
        Closure { environment_id }
    }
}

impl UserFunction {
    pub fn new(func_declaration: FunctionStmt, closure: Closure) -> UserFunction {
        UserFunction {
            func_declaration: Box::new(func_declaration),
            closure,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: &[Literal]) -> Result<Literal> {
        let current_environment_id_before_call =
            interpreter.environment_context.current_environment;

        // Need to pass in the environment id of the closure otherwise we'll just use the
        // environment at the time of calling the function instead of at the time of defining it.
        // Each environment is still mutable so we can't just clone environments when passing them into
        // functions.
        let new_interpreter = interpreter;
        new_interpreter.environment_context.current_environment = self.closure.environment_id;

        // Now that we've entered the same environment as the closure, we create the new environment for the function args
        new_interpreter.environment_context.begin_scope();

        for (index, param) in self.func_declaration.params.iter().enumerate() {
            new_interpreter
                .environment_context
                .define(&param.lexeme, arguments[index].clone())?;
        }

        let result = new_interpreter.execute_block(&self.func_declaration.body);

        new_interpreter.environment_context.exit_scope()?;

        // need to mutate current_environment to what it was before we called the function instead
        // since we mutated it to match the closure
        new_interpreter.environment_context.current_environment =
            current_environment_id_before_call;

        match result {
            Ok(_) => Ok(Literal::Nil),
            Err(e) => {
                match e.downcast::<ReturnError>() {
                    Ok(ReturnError::ReturnValue(return_val)) => Ok(return_val),
                    Err(e) => Err(e), // Not a ReturnError
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    pub func_ptr: LoxFuncPtr,
}

impl NativeFunction {
    pub fn new(arity: usize, func_ptr: LoxFuncPtr, name: String) -> NativeFunction {
        NativeFunction {
            arity,
            func_ptr,
            name,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter, arguments: &Vec<Literal>) -> Result<Literal> {
        (self.func_ptr)(interpreter, arguments)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Callable(Callable),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(n) => {
                Display::fmt(n, f)?;
                if (n % 1.0).abs() < f64::EPSILON {
                    f.write_str(".0")?;
                }
                Ok(())
            }
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Callable(c) => write!(f, "<fn {}>", c.name()),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize,
}

pub struct Scanner {
    tokens: Vec<Token>,
    source: String,
    start: usize,
    current: usize,
    line: usize,
    had_error: bool,
}

pub struct ScanResult {
    pub tokens: Vec<Token>,
    pub had_error: bool,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            tokens: Vec::new(),
            source,
            start: 0,
            current: 0,
            line: 1,
            had_error: false,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<ScanResult> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }
        self.tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            literal: None,
            line: self.line,
        });
        Ok(ScanResult {
            tokens: self.tokens.clone(),
            had_error: self.had_error,
        })
    }

    pub fn token_structure(&self) {
        for token in &self.tokens {
            let literal_string = match &token.literal {
                Some(literal) => format!("{}", literal),
                None => "null".to_string(),
            };
            println!("{} {} {}", token.token_type, token.lexeme, literal_string);
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.chars().count()
    }

    fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, location: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", line, location, message);
        self.had_error = true;
    }

    fn scan_token(&mut self) -> Result<()> {
        match self.advance() {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => match self.match_char('=') {
                true => self.add_token(TokenType::BangEqual),
                false => self.add_token(TokenType::Bang),
            },
            '=' => match self.match_char('=') {
                true => self.add_token(TokenType::EqualEqual),
                false => self.add_token(TokenType::Equal),
            },
            '<' => match self.match_char('=') {
                true => self.add_token(TokenType::LessEqual),
                false => self.add_token(TokenType::Less),
            },
            '>' => match self.match_char('=') {
                true => self.add_token(TokenType::GreaterEqual),
                false => self.add_token(TokenType::Greater),
            },
            '/' => {
                if self.match_char('/') {
                    // comment goes until the end of the line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '"' => self.string(),
            c => {
                if self.is_digit(c) {
                    self.number();
                } else if self.is_alphanumeric(c) {
                    self.identifier();
                } else {
                    self.error(self.line, &format!("Unexpected character: {}", c))
                }
            }
        }
        Ok(())
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_literal(token_type, None);
    }

    fn add_token_literal(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let text = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start)
            .collect();
        self.tokens.push(Token {
            token_type,
            lexeme: text,
            literal,
            line: self.line,
        });
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        //eprintln!("peeking at {}", self.current);
        //eprintln!("source: {}, {}", self.source, self.source.len());
        self.source.chars().nth(self.current).unwrap()
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error(self.line, "Unterminated string.");
            return;
        }

        // cover the closing "
        self.advance();

        // trim the surrounding quotes
        let value: String = self
            .source
            .chars()
            .skip(self.start + 1)
            .take(self.current - self.start - 2)
            .collect();
        self.add_token_literal(TokenType::String, Some(Literal::String(value)));
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_digit(c) || self.is_alpha(c)
    }

    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        // look for a fractional part
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            // consume the "."
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let value_string: String = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start)
            .collect();
        let value: f64 = value_string.parse().unwrap();
        self.add_token_literal(TokenType::Number, Some(Literal::Number(value)));
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.chars().count() {
            return '\0';
        }
        self.source.chars().nth(self.current + 1).unwrap()
    }

    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }

        let text: String = self
            .source
            .chars()
            .skip(self.start)
            .take(self.current - self.start)
            .collect();
        let token_type = KEYWORDS
            .get(&text.as_str())
            .unwrap_or(&TokenType::Identifier);
        self.add_token(token_type.clone());
    }
}
