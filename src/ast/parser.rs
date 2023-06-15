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
use crate::text::TextSpan;

use super::{Assignment, PickExpression, BlockLinkExpression};

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    scopes: Vec<Vec<Rc<Variable>>>,
    blocks: Vec<Rc<Block>>,
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
            blocks: vec![],
            diagnostics_bag,
        }
    }

    fn peak(&self, mut offset: isize) -> &Token {
        if self.cursor as isize + offset < 0 { offset = 0; }
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

    fn consume_if(&mut self, token_kind: TokenKind) {
        if self.current().kind == token_kind {
            self.consume();
        }
    }

    fn consume_and_check(&mut self, expected: TokenKind) -> Result<TokenKind, ()> {
        let token = self.consume().clone();
        let is_correct = token.kind == expected;
        if !is_correct {
            self.diagnostics_bag.borrow_mut().report_unexpected_token(&token, expected);
            Err(())
        }
        else {
            Ok(token.kind)
        }
    }

    fn check_and_consume_if_is(&mut self, expected: TokenKind) -> bool {
        let token = self.current();
        let cond = token.kind == expected;
        if cond {
            self.consume();
        }
        else {
            self.diagnostics_bag.borrow_mut().report_unexpected_token(token, expected);
        }
        cond
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

    fn search_blocks(&self, name: &str) -> Option<Rc<Block>> {
        for block in &self.blocks {
            if block.name.as_str() == name {
                return Some(block.clone());
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

    fn next_statement(&mut self) -> Option<Statement> {
        println!("New statement!");
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Word(word) => {
                let kind = match word.as_str() {
                    "block" => match self.parse_block() {
                        Ok(block) => {
                            self.blocks.push(Rc::new(block.clone()));
                            StatementKind::Block(block)
                        },
                        Err(_) => { return None; },
                    },
                    "out" => {
                        self.consume();
                        let expr = self.parse_expression().unwrap(); // TODO handle error
                        self.consume_and_check(TokenKind::Semicolon)
                            .expect("Could not find semicolon"); // TODO handle error
                        StatementKind::Out(expr)
                    }
                    "int" => {
                        self.diagnostics_bag.borrow_mut().report_error(&start_token.span, "A variable cannot be named \"int\"");
                        self.consume();
                        StatementKind::Error
                    }
                    _ => self.parse_assignment_statement().ok()?,
                };
                Some(Statement::new(kind, TextSpan::new(
                            start_token.span.start, 
                            self.peak(-1).span.end,
                            start_token.span.file
                            )))
            },
            TokenKind::End => None,
            TokenKind::RightCurly => None,
            _ => {
                let token = self.current();
                self.diagnostics_bag.borrow_mut().report_error(
                    &token.span,
                    &format!("A statement cannot begin with a `{}` token", token.kind)
                    );
                self.consume();
                Some(Statement::new(StatementKind::Error, TextSpan::new(
                        start_token.span.start,
                        self.peak(-1).span.end,
                        start_token.span.file
                        )))
            }
        }
    }

    fn consume_bad_statement(&mut self) {
        loop {
            let curr_kind = &self.current().kind;
            if curr_kind == &TokenKind::Semicolon || curr_kind == &TokenKind::End { break; }
            self.consume();
        }
        self.consume();
    }

    fn parse_assignment_statement(&mut self) -> Result<StatementKind, ()> {
        let variable: Rc<Variable>;
        if let ExpressionKind::Variable(v) = self.parse_primary_expression()?.kind {
            variable = v;
        }
        else { 
            self.consume_bad_statement();
            return Ok(StatementKind::Error);
        }

        if self.current().kind != TokenKind::Equals {
            self.diagnostics_bag.borrow_mut().report_unexpected_token(self.current(), TokenKind::Equals);
            self.consume_bad_statement();
            return Ok(StatementKind::Error);
        }
        self.consume();

        let expr = self.parse_expression()?;
        self.check_and_consume_if_is(TokenKind::Semicolon);
        Ok(StatementKind::Assignment(Assignment::new(variable, expr)))
    }

    fn parse_variable_type(&mut self) -> Result<VariableType, ()> {
        match self.consume().kind.clone() {
            TokenKind::Word(start_word) => {
                match start_word.as_str() {
                    "int" => {
                        match self.current().kind {
                            TokenKind::LeftParen => {
                                self.consume();
                                let type_token = self.consume().clone();
                                self.consume_and_check(TokenKind::RightParen)?;
                                if let TokenKind::Word(word) = type_token.kind.clone() {
                                    Ok(VariableType::Int(word))
                                }
                                else {
                                    self.diagnostics_bag.borrow_mut().report_unexpected_token(&type_token, TokenKind::Word("".to_string()));
                                    Ok(VariableType::Error)
                                }
                            }
                            _ => {
                                Ok(VariableType::Any)
                            }
                        }
                    },
                    "all" => Ok(VariableType::All),
                    w => {
                        self.diagnostics_bag.borrow_mut().report_error(&self.peak(-1).span, &format!("Unknown type `{}`.", w.clone()));
                        Ok(VariableType::Error)
                    }
                }
            },
            _ => Err(())
        }
    }

    fn parse_arguments(&mut self) -> Result<Vec<Rc<Variable>>, ()> {
        let mut arguments: Vec<Rc<Variable>> = vec![];
        loop {
            let name = if let TokenKind::Word(s) = self.current().kind.clone() { 
                self.consume();
                s
            }
            else { 
                return Ok(arguments);
            };
            self.consume_and_check(TokenKind::Colon)?;
            let t = self.parse_variable_type()?;
            let var_ref = Rc::new(Variable::new(name, t));
            self.add_to_scope(var_ref.clone());
            arguments.push(var_ref);
            self.consume_if(TokenKind::Comma);
        }
    }

    fn parse_outputs(&mut self) -> Result<Vec<VariableType>, ()> {
        let mut outputs: Vec<VariableType> = vec![];
        loop {
            if self.current().kind == TokenKind::RightParen {
                break;
            }
            self.consume_if(TokenKind::Comma);
            outputs.push(self.parse_variable_type()?);
        }
        Ok(outputs)
    }

    fn parse_block_link_arguments(&mut self) -> Result<Vec<Expression>, ()> {
        let mut inputs: Vec<Expression> = vec![];
        self.consume_and_check(TokenKind::LeftParen)?;
        loop {
            if self.current().kind == TokenKind::Comma { self.consume(); }
            if self.current().kind == TokenKind::RightParen { break; }
            inputs.push(self.parse_expression()?);
        }
        self.consume_and_check(TokenKind::RightParen)?;
        Ok(inputs)
    }

    fn parse_block(&mut self) -> Result<Block, ()> {
        self.consume();
        self.enter_scope();

        let name: String;
        let mut statements: Vec<Statement> = vec![];

        name = if let TokenKind::Word(s) = &self.consume().kind { s.clone() }
        else { 
            self.diagnostics_bag.borrow_mut().report_error(&self.peak(-1).span, "No name for `block` was given.");
            "".to_string()
        };

        self.consume_and_check(TokenKind::LeftParen)?;

        let inputs = self.parse_arguments()?;

        self.consume_and_check(TokenKind::RightParen)?;
        self.consume_and_check(TokenKind::RightArrow)?;
        self.consume_and_check(TokenKind::LeftParen)?;

        let outputs = self.parse_outputs()?;

        self.consume_and_check(TokenKind::RightParen)?;

        self.consume_and_check(TokenKind::LeftCurly)?;

        while let Some(statement) = self.next_statement() {
            statements.push(statement);
        }

        self.consume_and_check(TokenKind::RightCurly)?;

        self.exit_scope();

        Ok(Block::new(name, inputs, outputs, statements))
    }

    fn parse_expression(&mut self) -> Result<Expression, ()> {
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

    fn parse_binary_expression(&mut self) -> Result<Expression, ()> {
        let mut expr = self.parse_binary_expression_part(None, 0).ok_or(())?;
        loop {
            match self.parse_binary_expression_part(Some(expr.clone()), 0) {
                Some(e) => { expr = e; },
                None => { break; }
            }
        };
        Ok(expr)
    }

    fn parse_binary_expression_part(&mut self, primary_expr: Option<Expression>, precedence: u8) -> Option<Expression> {

        let mut left: Expression;
        if primary_expr.is_none() {
            left = self.parse_primary_expression().ok()?;
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
                left = Expression::new(
                    ExpressionKind::Binary(BinaryExpression::new(Box::new(left.clone()),
                    Box::new(right), operator))
                    );
            }
        }
        return Some(left);
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ()> {
        let token = self.current().clone();
        match &token.kind {
            TokenKind::Number(number) => {
                self.consume();
                Ok(Expression::new(ExpressionKind::Number(NumberExpression::new(number.clone()))))
            },
            TokenKind::Word(word) => {
                self.consume();
                let current_token = self.current();

                if let Some(var) = self.search_scope(&word) { // If is defined variable
                    if self.current().kind == TokenKind::LeftSquare {
                        self.consume();
                        if let TokenKind::Word(signal) = self.consume().kind.clone() {
                            self.consume_and_check(TokenKind::RightSquare)?;
                            return Ok(Expression::new(ExpressionKind::Pick(
                                        PickExpression::new(signal.to_string(), var)
                                        )));
                        }
                        return Ok(Expression::new(ExpressionKind::Error))
                    }
                    return Ok(Expression::new(ExpressionKind::Variable(var)));
                }
                else if current_token.kind == TokenKind::Colon { // If is variable definition
                    self.consume();
                    let t = self.parse_variable_type()?;
                    let var = Rc::new(Variable::new(word.to_string(), t));
                    self.add_to_scope(var.clone());
                    return Ok(Expression::new( ExpressionKind::Variable(var)))
                }
                else if let Some(block) = self.search_blocks(&word) {
                    return self.parse_block_link(block);
                }
                else {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &token.span,
                        &format!("Variable `{}` not defined.", word)
                        );
                    return Ok(Expression::new( ExpressionKind::Error));
                };
            }
            TokenKind::LeftParen => {
                self.consume();
                let inner = self.parse_expression()?;
                let expr = Expression::new(ExpressionKind::Parenthesized(ParenthesizedExpression::new(Box::new(inner))));
                self.consume_and_check(TokenKind::RightParen)?;
                Ok(expr)
            }
            _ => {
                Err(())
            }
        }
    }

    fn parse_block_link(&mut self, block: Rc<Block>) -> Result<Expression, ()> {
        let inputs = self.parse_block_link_arguments()?;
        let block_expr = BlockLinkExpression::new(block, inputs);
        Ok(Expression::new(ExpressionKind::BlockLink(block_expr)))
    }
}

impl Iterator for Parser {
    type Item = Statement;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_statement()
    }
}
