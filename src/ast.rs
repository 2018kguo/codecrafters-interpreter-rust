use crate::scanner::{Literal, Token};

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

pub struct Grouping {
    pub expression: Box<Expr>,
}

impl Accept<String> for Grouping {
    fn accept(&self, visitor: &dyn Visitor<String>) -> String {
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

pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

impl Accept<String> for Unary {
    fn accept(&self, visitor: &dyn Visitor<String>) -> String {
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
        result.push_str("(");
        result.push_str(&name);
        for expr in expressions {
            result.push_str(" ");
            result.push_str(&expr.accept(self));
        }
        result.push_str(")");
        result
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_binary(&self, binary: &Binary) -> String {
        return self.parenthesize(
            binary.operator.lexeme.clone(),
            vec![&binary.left, &binary.right],
        );
    }

    fn visit_grouping(&self, grouping: &Grouping) -> String {
        return self.parenthesize("group".to_string(), vec![&grouping.expression]);
    }

    fn visit_literal(&self, literal: &LiteralExpr) -> String {
        if matches!(literal.value, Literal::Nil) {
            return "nil".to_string();
        }

        return format!("{}", literal.value);
    }

    fn visit_unary(&self, unary: &Unary) -> String {
        return self.parenthesize(unary.operator.lexeme.clone(), vec![&unary.right]);
    }
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
