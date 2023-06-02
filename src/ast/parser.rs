use std::cmp::min;
use std::rc::Rc;

use crate::ast::lexer::{Token, TokenKind};
use crate::ast::{
    Expression, ExpressionKind, NumberExpression, ParenthesizedExpression, Variable,
    Statement, StatementKind, Block,
    BinaryExpression, BinaryOperator, BinaryOperatorKind,
    VariableType,
};
use crate::diagnostics::DiagnosticsBagRef;

use super::Assignment;

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    scopes: Vec<Vec<Rc<Variable>>>,
    diagnostics_bag: DiagnosticsBagRef,
}

impl Parser {
    pub fn new(diagnostics_bag: DiagnosticsBagRef, tokens: Vec<Token>) -> Self { 
        Self {
            tokens: tokens.iter().filter(
                |token| token.kind != TokenKind::Whitespace
            ).map(|token| token.clone()).collect(),
            cursor: 0,
            scopes: vec![vec![]],
            diagnostics_bag,
        }
    }

    fn peak(&self, offset: isize) -> &Token {
        self.tokens.get(
            min(
                (self.cursor as isize + offset) as usize, 
                self.tokens.len() - 1
                )
            ).unwrap()
    }

    fn current(&self) -> &Token {
        self.peak(0)
    }

    fn consume(&mut self) -> &Token {
        println!("{} : {:?}", self.cursor.clone(), self.current().clone());
        self.cursor += 1;
        self.peak(-1)
    }

    fn consume_and_check(&mut self, expected: TokenKind) -> &Token {
        let token = self.current();
        if token.kind != expected {
            self.diagnostics_bag.borrow_mut().report_unexpected_token(token, expected);
        }
        self.consume()
    }

    fn search_scope(&self, name: &str) -> Option<Rc<Variable>> {
        let mut rev_scopes = self.scopes.clone();
        rev_scopes.reverse();
        for scope in rev_scopes {
            for var in scope {
                if var.name == name {
                    return Some(var);
                }
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.scopes.push(vec![]);
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn add_to_scope(&mut self, var: Rc<Variable>) {
        self.scopes.last_mut().unwrap().push(var);
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.current().kind {
            TokenKind::Word(word) => {
                let kind = match word.as_str() {
                    "block" => StatementKind::Block(self.parse_block()?),
                    _ => StatementKind::Assignment(self.parse_assignment_statement()?),
                };
                Some(Statement::new(kind))
            },
            TokenKind::End => {
                None
            }
            _ => {
                self.diagnostics_bag.borrow_mut().report_error(
                    self.current(),
                    "A statement cannot begin with this token."
                    );
                self.consume();
                Some(Statement::new(StatementKind::Error))
            }
        }
    }

    fn parse_assignment_statement(&mut self) -> Option<Assignment> {
        let variable: Rc<Variable>;
        if let ExpressionKind::Variable(v) = self.parse_primary_expression()?.kind {
            variable = v;
        }
        else { panic!("expected variable.") }

        if self.current().kind != TokenKind::Equals {
            self.diagnostics_bag.borrow_mut().report_unexpected_token(self.current(), TokenKind::Equals);
        }
        self.consume();

        let expr = self.parse_expression()?;
        let end_token = self.consume();
        if end_token.kind != TokenKind::Semicolon {
            panic!("Expected ; at the end of expression statement.")
        }
        Some(Assignment::new(variable, expr))
    }

    fn parse_variable_type(&mut self) -> VariableType {
        if let TokenKind::Word(word) = self.consume().kind.clone() {
            match word.as_str() {
                "int" => {
                    match self.current().kind {
                        TokenKind::LeftParen => {
                            self.consume();
                            let type_token = self.consume().clone();
                                let end_token = self.consume();
                                if end_token.kind != TokenKind::RightParen {
                                    panic!("Expexted right paren after type.")
                                }
                            if let TokenKind::Word(word) = type_token.kind.clone() {
                                VariableType::Int(word)
                            }
                            else {
                                panic!("Variable types should be words not {:?}", type_token);
                            }
                        }
                        _ => {
                            VariableType::Any
                        }
                    }
                }
                _ => {
                    panic!("Unknown type: {}", word);
                }
            }
        }
        else {
            panic!("Expected word as variable type.")
        }
    }

    fn parse_argument(&mut self) -> Option<Variable> {
        let current_token_kind  = self.current().kind.clone();
        let name = if let TokenKind::Word(s) = current_token_kind { 
            self.consume();
            s.clone() 
        } 
        else if TokenKind::RightParen == current_token_kind {
            return None;
        }
        else { panic!("Could not find variable name.") };

        if self.consume().kind != TokenKind::Colon { panic!("Found no colon in argument definition.") };
        let t = self.parse_variable_type();

        if self.current().kind == TokenKind::Comma {
            self.consume();
        }
        Some(Variable::new(name, t))
    }

    fn parse_block(&mut self) -> Option<Block> {
        self.consume();
        self.enter_scope();

        let name: String;
        let mut inputs: Vec<Rc<Variable>> = vec![];
        let mut outputs: Vec<Rc<Variable>> = vec![];
        let mut statements: Vec<Statement> = vec![];

        name = if let TokenKind::Word(s) = &self.consume().kind { s.clone() } 
        else { panic!("No name for block was given") };

        self.consume_and_check(TokenKind::LeftParen);

        while let Some(variable) = self.parse_argument() {
            let var_ref = Rc::new(variable);
            self.add_to_scope(var_ref.clone());
            inputs.push(var_ref);
            if self.current().kind == TokenKind::Comma {
                self.consume();
            }
        }

        self.consume_and_check(TokenKind::RightParen);
        self.consume_and_check(TokenKind::RightArrow);
        self.consume_and_check(TokenKind::LeftParen);

        while let Some(variable) = self.parse_argument() {
            let var_ref = Rc::new(variable);
            self.add_to_scope(var_ref.clone());
            outputs.push(var_ref);
            if self.current().kind == TokenKind::Comma {
                self.consume();
            }
        }

        self.consume_and_check(TokenKind::RightParen);

        self.consume_and_check(TokenKind::LeftCurly);

        while let Some(statement) = self.parse_statement() {
            statements.push(statement);
        }

        self.consume_and_check(TokenKind::RightCurly);

        self.exit_scope();

        Some(Block::new(name, inputs, outputs, statements))
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_binary_expression()
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperator> {
        let token = self.current();
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
        let token = self.current().clone();
        match token.kind {
            TokenKind::Number(number) => {
                self.consume();
                Some(Expression::new(ExpressionKind::Number(NumberExpression::new(number))))
            },
            TokenKind::Word(word) => {
                self.consume();
                let current_token = self.current();

                if let Some(var) = self.search_scope(&word) {
                    return Some(Expression::new(ExpressionKind::Variable(var)));
                }
                else if current_token.kind == TokenKind::Colon {
                    self.consume();
                    let t = self.parse_variable_type();
                    let var = Rc::new(Variable::new(word, t));
                    self.add_to_scope(var.clone());
                    return Some(Expression::new( ExpressionKind::Variable(var)))
                }
                else {
                    panic!("No type for variable.")
                };
            }
            TokenKind::LeftParen => {
                self.consume();
                let inner = self.parse_expression()?;
                let expr = Some(Expression::new(ExpressionKind::Parenthesized(ParenthesizedExpression::new(Box::new(inner)))));
                let end_token = self.consume();
                if end_token.kind != TokenKind::RightParen {
                    panic!("Could not find end paren.")
                }
                expr
            }
            _ => {
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
