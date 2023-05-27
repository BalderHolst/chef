use crate::ast::lexer::{Token, TokenKind};
use crate::ast::{
    Expression, ExpressionKind, NumberExpression, 
    Statement, StatementKind,
    BinaryOperator, BinaryOperatorKind
};

use super::BinaryExpression;


pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    fn consume(&mut self) -> Option<&Token> {

        // Skip Whitespace
        while self.current()?.kind == TokenKind::Whitespace {
            self.cursor += 1;
        }

        self.cursor += 1;
        self.tokens.get(self.cursor - 1)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression()?;
        dbg!(&expr);
        Some(Statement::new(StatementKind::Expression(expr)))
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_binary_expression(0)
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let token = self.consume()?;
        match token.kind {
            TokenKind::Plus => Some(BinaryOperator::new(BinaryOperatorKind::Plus)),
            TokenKind::Minus => Some(BinaryOperator::new(BinaryOperatorKind::Minus)),
            TokenKind::Asterisk => Some(BinaryOperator::new(BinaryOperatorKind::Multiply)),
            TokenKind::Slash => Some(BinaryOperator::new(BinaryOperatorKind::Divide)),
            _ => None,
        }
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> Option<Expression> {
        let mut left = self.parse_primary_expression()?;
        while let Some(operator) = self.parse_binary_operator() {
            if operator.precedence() < precedence {
                break;
            }
            let right = self.parse_binary_expression(operator.precedence())?;
            left = Expression::new(ExpressionKind::Binary(BinaryExpression::new(Box::new(left), Box::new(right), operator)));
        }
        Some(left)
    }

    fn parse_primary_expression(&mut self) -> Option<Expression> {
        let token = self.consume()?;
        if let TokenKind::Number(number) = token.kind {
            return Some(Expression::new(ExpressionKind::Number(NumberExpression::new(number))))
        }
        panic!("No primary expression could be parsed from token: {:?}", token);
    }
}

impl Iterator for Parser {
    type Item = Statement;
    fn next(&mut self) -> Option<Self::Item> {
        self.parse_statement()
    }
}
