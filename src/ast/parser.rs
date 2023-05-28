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

    fn current(&mut self) -> Option<&Token> {

        // Skip Whitespace
        while self.tokens.get(self.cursor)?.kind == TokenKind::Whitespace {
            self.cursor += 1;
        }

        self.tokens.get(self.cursor)
    }

    fn consume(&mut self) -> Option<&Token> {
        self.current()?; // Skip Whitespace
        println!("{} : {:?}", self.cursor.clone(), self.current().clone());
        self.cursor += 1;
        self.tokens.get(self.cursor - 1)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        self.current()?;
        let expr = self.parse_expression()?;
        Some(Statement::new(StatementKind::Expression(expr)))
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_binary_expression()
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let token = self.current()?;
        match token.kind {
            TokenKind::Plus => Some(BinaryOperator::new(BinaryOperatorKind::Plus)),
            TokenKind::Minus => Some(BinaryOperator::new(BinaryOperatorKind::Minus)),
            TokenKind::Asterisk => Some(BinaryOperator::new(BinaryOperatorKind::Multiply)),
            TokenKind::Slash => Some(BinaryOperator::new(BinaryOperatorKind::Divide)),
            _ => None,
        }
    }

    fn parse_binary_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_binary_expression_part(None, 0)?;
        loop {
            match self.parse_binary_expression_part(Some(expr.clone()), 0) {
                Some(e) => { expr = e; },
                None => { break; }
            }
        };
        Some(expr)
    }

    fn parse_binary_expression_part(&mut self, primary_expr: Option<Expression>, precedence: u8) -> Option<Expression> {


        let mut left: Expression;
        if primary_expr.is_none() {
            left = self.parse_primary_expression()?;
            self.consume().unwrap();
        }
        else {
            self.current()?; // Return None if no tokens left
            left = primary_expr.unwrap();
        }

        if let Some(operator) = self.parse_binary_operator() {
            let op_precedence = operator.precedence();
            if op_precedence >= precedence {
                self.consume();
                let right = self.parse_binary_expression_part(None, op_precedence)?;
                left = Expression::new(ExpressionKind::Binary(BinaryExpression::new(Box::new(left.clone()), Box::new(right), operator)));
            }
        }
        return Some(left);
    }

    fn parse_primary_expression(&mut self) -> Option<Expression> {
        let token = self.current()?;
        if let TokenKind::Number(number) = token.kind {
            return Some(Expression::new(ExpressionKind::Number(NumberExpression::new(number))))
        }
        println!("No primary expression could be parsed from token: {:?}", token);
        None
    }
}

impl Iterator for Parser {
    type Item = Statement;
    fn next(&mut self) -> Option<Self::Item> {
        self.parse_statement()
    }
}
