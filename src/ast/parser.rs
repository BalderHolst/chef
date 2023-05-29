use crate::ast::lexer::{Token, TokenKind};
use crate::ast::{
    Expression, ExpressionKind, NumberExpression, ParenthesizedExpression, Variable,
    Statement, StatementKind,
    BinaryOperator, BinaryOperatorKind,
    VariableType,
};

use super::{BinaryExpression, Block};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self { 
        Self {
            tokens: tokens.iter().filter(
                |token| token.kind != TokenKind::Whitespace
            ).map(|token| token.clone()).collect(),
            cursor: 0,
        }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    fn consume(&mut self) -> Option<&Token> {
        self.current()?; // Skip Whitespace
        println!("{} : {:?}", self.cursor.clone(), self.current().clone());
        self.cursor += 1;
        self.tokens.get(self.cursor - 1)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.current()?.kind {
            TokenKind::Word(word) => {
                match word.as_str() {
                    "block" => self.parse_block(),
                    _ => self.parse_expression_statement()
                }
            },
            _ => {
                self.parse_expression_statement()
            }
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression()?;
        let end_token = self.consume().expect("Expected end token at the end of expression statement.");
        if end_token.kind != TokenKind::Semicolon {
            panic!("Expected ; at the end of expression statement.")
        }
        Some(Statement::new(StatementKind::Expression(expr)))
    }

    fn parse_argument(&mut self) -> Option<Variable> {
        let name = if let TokenKind::Word(s) = &self.consume()?.kind { s.clone() } 
        else { panic!("Could not find variable name.") };

        if self.consume()?.kind != TokenKind::Colon { panic!("Found no colon in argument definition.") };

        todo!()
    }

    fn parse_block(&mut self) -> Option<Statement> {
        self.consume().unwrap();
        let mut name: String;
        let mut inputs: Vec<Variable> = vec![];
        let mut outputs: Vec<Variable> = vec![];
        let mut statements: Vec<Statement> = vec![];

        name = if let TokenKind::Word(s) = &self.consume()?.kind { s.clone() } 
        else { panic!("No name for block was given") };

        if self.consume()?.kind != TokenKind::LeftParen { panic!("Did not find LEFT input paren for block") }

        while let Some(variable) = self.parse_argument() {
            inputs.push(variable);
        }

        if self.consume()?.kind != TokenKind::RightParen { panic!("Did not find RIGHT input paren for block") }
        if self.consume()?.kind != TokenKind::RightArrow { panic!("Did not find rightarrow in block definition") }
        if self.consume()?.kind != TokenKind::LeftParen { panic!("Did not find LEFT output paren for block") }

        while let Some(variable) = self.parse_argument() {
            outputs.push(variable);
        }

        Some(Statement::new(StatementKind::Block(Block::new(name, inputs, outputs, statements))))
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
        }
        else { // TODO prettify
            self.parse_binary_operator()?; // Return None if next token is not an operator
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
        let token = self.current()?.clone();
        match token.kind {
            TokenKind::Number(number) => {
                self.consume().unwrap();
                Some(Expression::new(ExpressionKind::Number(NumberExpression::new(number))))
            },
            TokenKind::Word(word) => {
                self.consume().unwrap();
                let t: Option<VariableType> = None; // type
                let current_token = self.current();
                if current_token.is_some() && current_token.unwrap().kind == TokenKind::Colon {
                    todo!("Parse type");
                }
                Some(Expression::new(ExpressionKind::Variable(Variable::new(word, t))))
            }
            TokenKind::LeftParen => {
                self.consume().unwrap();
                let inner = self.parse_expression()?;
                let expr = Some(Expression::new(ExpressionKind::Parenthesized(ParenthesizedExpression::new(Box::new(inner)))));
                let end_token = self.consume().expect("Expected paren end token");
                if end_token.kind != TokenKind::RightParen {
                    panic!("Could not find end paren.")
                }
                expr
            }
            _ => {
                println!("No primary expression could be parsed from token: {:?}", token);
                None
            }
        }
    }
}

impl Iterator for Parser {
    type Item = Statement;
    fn next(&mut self) -> Option<Self::Item> {
        self.parse_statement()
    }
}
