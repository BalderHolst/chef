//! Module for parsing a tokenstream into an abstract syntax tree.

use std::cmp::min;
use std::rc::Rc;

use crate::cli::Opts;
use crate::ast::lexer::{Token, TokenKind};
use crate::ast::{
    Expression, ExpressionKind, NumberExpression, ParenthesizedExpression, Variable,
    Statement, StatementKind, Block,
    BinaryExpression, BinaryOperator, BinaryOperatorKind,
    VariableType,
};
use crate::diagnostics::DiagnosticsBagRef;
use crate::text::TextSpan;

use super::lexer::Lexer;
use super::{Assignment, PickExpression, BlockLinkExpression};

/// The parser. The parser can be used as an iterator to get statements one at a time.
pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    scopes: Vec<Vec<Rc<Variable>>>,
    blocks: Vec<Rc<Block>>,
    diagnostics_bag: DiagnosticsBagRef,
    options: Rc<Opts>,
}

impl Parser {

    /// Create a new [Parser].
    pub fn new(tokens: Vec<Token>, diagnostics_bag: DiagnosticsBagRef, options: Rc<Opts>) -> Self { 
        Self {
            tokens: tokens.iter().filter(
                |token| token.kind != TokenKind::Whitespace
            ).map(|token| token.clone()).collect(),
            cursor: 0,
            scopes: vec![vec![]],
            blocks: vec![],
            diagnostics_bag,
            options,
        }
    }

    pub fn from_lexer(lexer: Lexer, diagnostics_bag: DiagnosticsBagRef, options: Rc<Opts>) -> Self { 
        Self {
            tokens: lexer.filter(
                |token| token.kind != TokenKind::Whitespace
            ).map(|token| token.clone()).collect(),
            cursor: 0,
            scopes: vec![vec![]],
            blocks: vec![],
            diagnostics_bag,
            options,
        }
    }

    /// Peak at a token around the current cursor position with an offset.
    fn peak(&self, mut offset: isize) -> &Token {
        if self.cursor as isize + offset < 0 { offset = 0; }
        self.tokens.get(
            min(
                (self.cursor as isize + offset) as usize, 
                self.tokens.len() - 1
                )
            ).unwrap()
    }

    /// Get the current token.
    fn current(&self) -> &Token {
        self.peak(0)
    }

    /// Set the cursor position.
    fn rewind_to(&mut self, cursor_position: usize) {
        self.cursor = cursor_position;
    }

    /// Return the current token and move to the next token.
    fn consume(&mut self) -> &Token {
        if self.options.verbose {
            println!("{} : {:?}", self.cursor.clone(), self.current().clone());
        }
        self.cursor += 1;
        self.peak(-1)
    }

    /// Consume only if the token is of a certain kind.
    fn consume_if(&mut self, token_kind: TokenKind) {
        if self.current().kind == token_kind {
            self.consume();
        }
    }

    /// Consume and error if the token is not what was expected.
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

    /// Search the current scope for a variable.
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

    /// Search for a block by name.
    fn search_blocks(&self, name: &str) -> Option<Rc<Block>> {
        for block in &self.blocks {
            if block.name.as_str() == name {
                return Some(block.clone());
            }
        }
        None
    }

    /// Enter a new scope.
    fn enter_scope(&mut self) {
        self.scopes.push(vec![]);
    }

    /// Exit the current scope.
    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop().unwrap();
        }
    }

    /// Add a variable to the current scope.
    fn add_to_scope(&mut self, var: Rc<Variable>) {
        self.scopes.last_mut().unwrap().push(var);
    }

    /// Parse next statement. Return `None` of none are left.
    fn next_statement(&mut self) -> Option<Statement> {
        if self.options.verbose {
            println!("New statement!");
        }
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
                        let statement_start = self.current().span.start;
                        if let Ok(expr) = self.parse_expression() { // TODO handle error
                            self.consume_and_check(TokenKind::Semicolon)
                                .expect("Could not find semicolon"); // TODO handle error
                            StatementKind::Out(expr)
                        }
                        else {
                            self.consume_bad_statement();
                            let statement_end = self.peak(-1).span.end;
                            let text = self.peak(-1).span.text.clone();
                            self.diagnostics_bag.borrow_mut().report_error(
                                &TextSpan::new(statement_start, statement_end, text), "Bad output expression"); // TODO use error message here
                            StatementKind::Error
                        }
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
                            start_token.span.text.clone()
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
                        start_token.span.text.clone()
                        )))
            }
        }
    }

    /// Consume token until a the end of the current statement.
    fn consume_bad_statement(&mut self) {
        loop {
            let curr_kind = &self.current().kind;
            if curr_kind == &TokenKind::Semicolon || curr_kind == &TokenKind::End { break; }
            self.consume();
        }
        self.consume();
    }

    /// Parse variable assignment statement.
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
        self.consume_and_check(TokenKind::Semicolon)?;
        Ok(StatementKind::Assignment(Assignment::new(variable, expr)))
    }

    /// Parse variable type.
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

    /// Parse arguments for `block` definition.
    fn parse_block_arguments(&mut self) -> Result<Vec<Rc<Variable>>, ()> {
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
            let var_start = self.current().span.clone();
            let t = self.parse_variable_type()?;
            let var_ref = Rc::new(Variable::new(name, t,
                    TextSpan::from_spans( var_start, self.peak(-1).span.clone())));
            self.add_to_scope(var_ref.clone());
            arguments.push(var_ref);
            self.consume_if(TokenKind::Comma);
        }
    }

    /// Parse outputs for `block` definition.
    fn parse_block_outputs(&mut self) -> Result<Vec<VariableType>, ()> {
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

    /// Parse arguments for `block` links.
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

    /// Parse chef `block`.
    fn parse_block(&mut self) -> Result<Block, ()> {
        let start_token = self.consume().clone(); // Consume "block" word
        debug_assert_eq!(start_token.kind, TokenKind::Word("block".to_string()));

        self.enter_scope();

        let name: String;
        let mut statements: Vec<Statement> = vec![];

        name = if let TokenKind::Word(s) = &self.consume().kind { s.clone() }
        else { 
            self.diagnostics_bag.borrow_mut().report_error(&self.peak(-1).span, "No name for `block` was given.");
            "".to_string()
        };

        self.consume_and_check(TokenKind::LeftParen)?;

        let inputs = self.parse_block_arguments()?;

        self.consume_and_check(TokenKind::RightParen)?;
        self.consume_and_check(TokenKind::RightArrow)?;
        self.consume_and_check(TokenKind::LeftParen)?;

        let outputs = self.parse_block_outputs()?;

        self.consume_and_check(TokenKind::RightParen)?;

        self.consume_and_check(TokenKind::LeftCurly)?;

        while let Some(statement) = self.next_statement() {
            statements.push(statement);
        }

        self.consume_and_check(TokenKind::RightCurly)?;

        self.exit_scope();

        Ok(Block::new(name, inputs, outputs, statements, TextSpan {
            start: start_token.span.start,
            end: self.peak(-1).span.end,
            text: start_token.span.text 
        }))
    }

    /// Parse chef expression.
    fn parse_expression(&mut self) -> Result<Expression, ()> {
        self.parse_binary_expression(None)
    }

    /// Returns the operator if the current token is an operator
    fn get_binary_operator(&mut self) -> Option<BinaryOperator> {
        let token = self.current();
        match token.kind {
            TokenKind::Plus => Some(BinaryOperator::new(BinaryOperatorKind::Plus)),
            TokenKind::Minus => Some(BinaryOperator::new(BinaryOperatorKind::Minus)),
            TokenKind::Asterisk => Some(BinaryOperator::new(BinaryOperatorKind::Multiply)),
            TokenKind::Slash => Some(BinaryOperator::new(BinaryOperatorKind::Divide)),
            _ => None,
        }
    }

    /// Parse a binary expression.
    fn parse_binary_expression(&mut self, mut left: Option<Expression>) -> Result<Expression, ()> {
        if left.is_none() {
            let left = self.parse_primary_expression()?;
            return self.parse_binary_expression(Some(left));
        }

        let left_operator = if let Some(op) = self.get_binary_operator() {
            self.consume();
            op
        }
        else { 
            self.diagnostics_bag.borrow_mut().report_error(&self.current().span, "Expected operator.");
            return Err(());
        };

        // Store the current cursor position in case we need to jump back and parse the right
        // expression differently.
        let cursor_pos = self.cursor;
        let mut right = self.parse_primary_expression()?;

        if let Some(right_operator) = self.get_binary_operator() {
            if right_operator.precedence() > left_operator.precedence() {
                self.rewind_to(cursor_pos);
                right = self.parse_binary_expression(None)?;
            }
        }

        left = Some(Expression::new(ExpressionKind::Binary(
                BinaryExpression {
                    left: Box::new(left.clone().unwrap()),
                    right: Box::new(right.clone()),
                    operator: left_operator
                }
                ),
                TextSpan { start: left.clone().unwrap().span.start, end: right.span.end, text: left.unwrap().span.text }
                ));

        if self.get_binary_operator().is_some() {
            self.parse_binary_expression(left)
        }
        else {
            Ok(left.unwrap())
        }
    }

    /// Parse a primary expression.
    fn parse_primary_expression(&mut self) -> Result<Expression, ()> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Number(number) => {
                self.consume();
                Ok(Expression::new(ExpressionKind::Number(NumberExpression::new(number.clone() as i32)),
                                   TextSpan::from_spans(start_token.span, self.peak(-1).span.clone())
                                   ))
            },
            TokenKind::Word(word) => {
                self.consume();
                let current_token = self.current();

                if let Some(var) = self.search_scope(&word) { // If is defined variable
                    if self.current().kind == TokenKind::LeftSquare {
                        self.consume();
                        if let TokenKind::Word(signal) = self.consume().kind.clone() {
                            self.consume_and_check(TokenKind::RightSquare)?;
                            return Ok({
                                let kind = ExpressionKind::Pick(PickExpression::new(signal.to_string(), var));
                                let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                                Expression { kind, span }
                            });
                        }
                        return Ok({
                            let kind = ExpressionKind::Error;
                            let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                            Expression { kind, span }
                        })
                    }
                    return Ok({
                        let kind = ExpressionKind::Variable(var);
                        let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        Expression { kind, span }
                    });
                }
                else if current_token.kind == TokenKind::Colon { // If is variable definition
                    self.consume();
                    let t = self.parse_variable_type()?;
                    let var = Rc::new(Variable::new(word.to_string(), t, TextSpan {
                        start: start_token.span.start, end: self.peak(-1).span.end, text: start_token.span.text.clone()
                    }));
                    self.add_to_scope(var.clone());
                    return Ok({
                        let kind = ExpressionKind::Variable(var);
                        let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        Expression { kind, span }
                    })
                }
                else if let Some(block) = self.search_blocks(&word) {
                    let block_link_expr = self.parse_block_link(block)?;
                    return Ok({
                        let kind = ExpressionKind::BlockLink(block_link_expr);
                        let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        Expression { kind, span }
                    });
                }
                else {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &start_token.span,
                        &format!("Variable `{}` not defined.", word)
                        );
                    return Ok({
                        let kind = ExpressionKind::Error;
                        let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        Expression { kind, span }
                    });
                };
            }
            TokenKind::LeftParen => {
                self.consume();
                let inner = self.parse_expression()?;
                let expr = {
                    let kind = ExpressionKind::Parenthesized(ParenthesizedExpression::new(Box::new(inner)));
                    let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                    Expression { kind, span }
                };
                self.consume_and_check(TokenKind::RightParen)?;
                Ok(expr)
            }
            _ => {
                Err(())
            }
        }
    }

    /// Parse a chef block link.
    fn parse_block_link(&mut self, block: Rc<Block>) -> Result<BlockLinkExpression, ()> {
        let inputs = self.parse_block_link_arguments()?;
        Ok(BlockLinkExpression::new(block, inputs))
    }
}

impl Iterator for Parser {
    type Item = Statement;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_statement()
    }
}

#[test]
fn parse_binary_expression() {
    use super::*;

    let code = "1+2*3-4";
    let (text, bag, lexer) = Lexer::new_bundle(code);

    let expected_expr = Expression {
        kind: ExpressionKind::Binary(BinaryExpression {
            left: Box::new(Expression {
                kind: ExpressionKind::Number(NumberExpression { number: 1 }),
                span: TextSpan::new(0, 1, text.clone())
            }),
            right: Box::new(Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    left: Box::new(Expression {
                        kind: ExpressionKind::Binary(BinaryExpression {
                            left: Box::new(Expression {
                                kind: ExpressionKind::Number(NumberExpression { number: 2 }),
                                span: TextSpan { start: 2, end: 3, text: text.clone() }
                            }), 
                            right: Box::new(Expression {
                                kind: ExpressionKind::Number(NumberExpression { number: 3 }),
                                span: TextSpan { start: 4, end: 5, text: text.clone() }
                            }),
                            operator: BinaryOperator { kind: BinaryOperatorKind::Multiply }
                        }),
                        span: TextSpan { start: 2, end: 5, text: text.clone() }
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::Number(NumberExpression { number: 4 }),
                        span: TextSpan { start: 6, end: 7, text: text.clone() }
                    }),
                    operator: BinaryOperator {
                        kind: BinaryOperatorKind::Minus
                    }
                }),
                span: TextSpan { start: 2, end: 7, text: text.clone() }
            }),
            operator: BinaryOperator { kind: BinaryOperatorKind::Plus }
        }),
        span: TextSpan::new(0, 7, text.clone())
    };

    let mut parser = Parser::from_lexer(lexer, bag, Rc::new(crate::cli::Opts::default()));
    let parsed_expr = parser.parse_binary_expression(None).unwrap();


    assert_eq!(parsed_expr, expected_expr);
}
