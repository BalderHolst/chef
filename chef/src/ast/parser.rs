//! Module for parsing a token stream into an abstract syntax tree.

use std::cmp::min;
use std::rc::Rc;

use crate::ast::lexer::{Token, TokenKind};
use crate::ast::{
    BinaryExpression, BinaryOperator, BinaryOperatorKind, Block, Expression, ExpressionKind,
    IntExpression, ParenthesizedExpression, Statement, StatementKind, Variable, VariableType,
};
use crate::cli::Opts;
use crate::diagnostics::{CompilationError, DiagnosticsBagRef};
use crate::text::TextSpan;

use super::lexer::Lexer;
use super::{
    Assignment, BlockLinkExpression, PickExpression, VariableRef, VariableSignalType,
    WhenExpression,
};

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
            tokens: tokens
                .iter()
                .filter(|token| token.kind != TokenKind::Whitespace)
                .cloned()
                .collect(),
            cursor: 0,
            scopes: vec![vec![]],
            blocks: vec![],
            diagnostics_bag,
            options,
        }
    }

    pub fn _from_lexer(
        lexer: Lexer,
        diagnostics_bag: DiagnosticsBagRef,
        options: Rc<Opts>,
    ) -> Self {
        Self {
            tokens: lexer
                .filter(|token| token.kind != TokenKind::Whitespace)
                .collect(),
            cursor: 0,
            scopes: vec![vec![]],
            blocks: vec![],
            diagnostics_bag,
            options,
        }
    }

    /// Peak at a token around the current cursor position with an offset.
    fn peak(&self, mut offset: isize) -> &Token {
        if self.cursor as isize + offset < 0 {
            offset = 0;
        }
        self.tokens
            .get(min(
                (self.cursor as isize + offset) as usize,
                self.tokens.len() - 1,
            ))
            .unwrap()
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
            println!("{} : {:?}", self.cursor.clone(), &self.current().kind);
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
    fn consume_and_check(&mut self, expected: TokenKind) -> Result<TokenKind, CompilationError> {
        let token = self.consume().clone();
        let is_correct = token.kind == expected;
        if !is_correct {
            Err(CompilationError::new_unexpected_token(token, expected))
        } else {
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
                            Ok(StatementKind::Block(block))
                        }
                        Err(e) => {
                            self.diagnostics_bag
                                .borrow_mut()
                                .report_compilation_error(e);
                            return None;
                        }
                    },
                    "out" => {
                        self.consume();
                        let statement_start = self.current().span.start;
                        if let Ok(expr) = self.parse_expression() {
                            // TODO handle error
                            self.consume_and_check(TokenKind::Semicolon)
                                .expect("Could not find semicolon"); // TODO handle error
                            Ok(StatementKind::Out(expr))
                        } else {
                            self.consume_bad_statement();
                            let statement_end = self.peak(-1).span.end;
                            let text = self.peak(-1).span.text.clone();
                            self.diagnostics_bag.borrow_mut().report_error(
                                &TextSpan::new(statement_start, statement_end, text),
                                "Bad output expression",
                            ); // TODO use error message here
                            Ok(StatementKind::Error)
                        }
                    }
                    "int" => {
                        self.diagnostics_bag
                            .borrow_mut()
                            .report_error(&start_token.span, "A variable cannot be named \"int\"");
                        self.consume();
                        Ok(StatementKind::Error)
                    }
                    _ => match self.peak(1).kind.clone() {
                        TokenKind::Equals => self.parse_assignment_statement(),
                        TokenKind::Colon => self.parse_assignment_statement(),
                        _ => match self.parse_expression() {
                            Ok(expr) => Ok(StatementKind::Out(expr)),
                            Err(e) => Err(e),
                        },
                    },
                };
                let kind = match kind {
                    Ok(k) => k,
                    Err(e) => {
                        self.diagnostics_bag
                            .borrow_mut()
                            .report_compilation_error(e);
                        return None;
                    }
                };
                Some(Statement::new(
                    kind,
                    TextSpan::new(
                        start_token.span.start,
                        self.peak(-1).span.end,
                        start_token.span.text.clone(),
                    ),
                ))
            }
            TokenKind::End => None,
            TokenKind::RightCurly => None,
            _ => {
                // Assume statement to be an `out` statement if nothing else.
                let out_expr = match self.parse_expression() {
                    Ok(expr) => expr,
                    Err(e) => {
                        self.diagnostics_bag
                            .borrow_mut()
                            .report_compilation_error(e);
                        return None;
                    }
                };
                Some(Statement::new(
                    StatementKind::Out(out_expr),
                    TextSpan::from_spans(start_token.span, self.peak(-1).span.clone()),
                ))
            }
        }
    }

    /// Consume token until a the end of the current statement.
    fn consume_bad_statement(&mut self) {
        loop {
            let curr_kind = &self.current().kind;
            if curr_kind == &TokenKind::Semicolon || curr_kind == &TokenKind::End {
                break;
            }
            self.consume();
        }
        self.consume();
    }

    /// Parse variable assignment statement.
    fn parse_assignment_statement(&mut self) -> Result<StatementKind, CompilationError> {
        let start_span = self.current().span.clone();
        let variable = if let ParsedVariable::Def(v) = self.parse_variable()? {
            Rc::new(v)
        } else {
            self.diagnostics_bag.borrow_mut().report_error(
                &TextSpan::from_spans(start_span, self.peak(-1).span.clone()),
                "Variables cannot be reassigned.",
            );
            self.consume_bad_statement();
            return Ok(StatementKind::Error);
        };

        if self.current().kind != TokenKind::Equals {
            self.diagnostics_bag
                .borrow_mut()
                .report_unexpected_token(self.current(), TokenKind::Equals);
            self.consume_bad_statement();
            return Ok(StatementKind::Error);
        }
        self.consume(); // Consume equals

        self.add_to_scope(variable.clone());

        let expr = self.parse_expression()?;
        self.consume_and_check(TokenKind::Semicolon)?;
        Ok(StatementKind::Assignment(Assignment::new(variable, expr)))
    }

    /// Parse signal part of variable type
    ///
    /// Example:
    /// ```text
    /// i: int(signal) = ...
    ///       ^^^^^^^^ - this part
    /// ```
    fn parse_variable_type_signal(&mut self) -> Result<VariableSignalType, CompilationError> {
        match self.current().kind {
            TokenKind::LeftParen => {
                self.consume();
                let type_token = self.consume().clone();
                self.consume_and_check(TokenKind::RightParen)?;
                if let TokenKind::Word(word) = type_token.kind.clone() {
                    Ok(VariableSignalType::Signal(word))
                } else {
                    Err(CompilationError::new_unexpected_token(
                        type_token,
                        TokenKind::Word("".to_string()),
                    ))
                }
            }
            _ => Ok(VariableSignalType::Any),
        }
    }

    fn parse_variable(&mut self) -> Result<ParsedVariable, CompilationError> {
        let start_token = self.current().clone();
        let name = match &start_token.kind {
            TokenKind::Word(w) => Ok(w),
            _ => Err(CompilationError::new_unexpected_token(
                start_token.clone(),
                TokenKind::Word("".to_string()),
            )),
        }?;
        self.consume(); // Consume name

        if self.current().kind != TokenKind::Colon {
            let var = self.search_scope(name).ok_or(CompilationError::new(
                format!("Variable `{}` not defined.", name),
                start_token.span.clone(),
            ))?;
            return Ok(ParsedVariable::Ref(VariableRef::new(var, start_token.span)));
        }

        self.consume_and_check(TokenKind::Colon)?;
        let type_ = self.parse_variable_type()?;

        Ok(ParsedVariable::Def(Variable::new(
            name.to_owned(),
            type_,
            TextSpan::from_spans(start_token.span, self.peak(-1).span.clone()),
        )))
    }

    /// Parse variable type.
    fn parse_variable_type(&mut self) -> Result<VariableType, CompilationError> {
        let token = self.consume();
        match token.kind.clone() {
            TokenKind::Word(start_word) => match start_word.as_str() {
                "int" => Ok(VariableType::Int(self.parse_variable_type_signal()?)),
                "bool" => Ok(VariableType::Bool(self.parse_variable_type_signal()?)),
                "all" => Ok(VariableType::All),
                w => Err(CompilationError::new(
                    format!("Unknown type `{}`.", w),
                    token.span.clone(),
                )),
            },
            _ => Err(CompilationError::new(
                format!("Expected variable type to be word, not `{}`", token.kind),
                token.span.clone(),
            )),
        }
    }

    /// Parse arguments for `block` definition.
    fn parse_block_arguments(&mut self) -> Result<Vec<Rc<Variable>>, CompilationError> {
        let mut arguments: Vec<Rc<Variable>> = vec![];
        while self.current().kind != TokenKind::RightParen {
            let var = match self.parse_variable()? {
                ParsedVariable::Def(v) => Ok(v),

                // TODO: this will never happen as parse_variable_type til return an error on undefined variables.
                ParsedVariable::Ref(v) => Err(CompilationError::new(
                    "Please give variable a type.".to_string(),
                    v.span.clone(),
                )),
            }?;
            let var_ref = Rc::new(var);
            self.add_to_scope(var_ref.clone());
            arguments.push(var_ref);
            self.consume_if(TokenKind::Comma);
        }
        Ok(arguments)
    }

    /// Parse outputs for `block` definition.
    fn parse_block_outputs(&mut self) -> Result<VariableType, CompilationError> {
        self.parse_variable_type()
    }

    /// Parse arguments for `block` links.
    fn parse_block_link_arguments(&mut self) -> Result<Vec<Expression>, CompilationError> {
        let mut inputs: Vec<Expression> = vec![];
        self.consume_and_check(TokenKind::LeftParen)?;
        loop {
            if self.current().kind == TokenKind::Comma {
                self.consume();
            }
            if self.current().kind == TokenKind::RightParen {
                break;
            }
            inputs.push(self.parse_expression()?);
        }
        self.consume_and_check(TokenKind::RightParen)?;
        Ok(inputs)
    }

    fn parse_statement_list_expression(&mut self) -> Result<Vec<Statement>, CompilationError> {
        self.consume_and_check(TokenKind::LeftCurly)?;

        let mut statements: Vec<Statement> = vec![];
        while let Some(statement) = self.next_statement() {
            statements.push(statement);
        }

        if !statements.is_empty() {
            for statement in &statements[..statements.len() - 1] {
                if let StatementKind::Out(_) = statement.kind {
                    return Err(CompilationError::new(
                        "Output statements have to be last.".to_string(),
                        statement.span.clone(),
                    ));
                }
            }
        }

        self.consume_and_check(TokenKind::RightCurly)?;

        Ok(statements)
    }

    /// Parse chef `block`.
    fn parse_block(&mut self) -> Result<Block, CompilationError> {
        let start_token = self.consume().clone(); // Consume "block" word
        debug_assert_eq!(start_token.kind, TokenKind::Word("block".to_string()));

        self.enter_scope();

        let name: String = if let TokenKind::Word(s) = &self.consume().kind {
            s.clone()
        } else {
            self.diagnostics_bag
                .borrow_mut()
                .report_error(&self.peak(-1).span, "No name for `block` was given.");
            "".to_string()
        };

        self.consume_and_check(TokenKind::LeftParen)?;

        let inputs = self.parse_block_arguments()?;

        self.consume_and_check(TokenKind::RightParen)?;
        self.consume_and_check(TokenKind::RightArrow)?;

        let output = self.parse_block_outputs()?;

        let statements = self.parse_statement_list_expression()?;

        self.exit_scope();

        Ok(Block::new(
            name,
            inputs,
            output,
            statements,
            TextSpan {
                start: start_token.span.start,
                end: self.peak(-1).span.end,
                text: start_token.span.text,
            },
        ))
    }

    /// Parse chef expression.
    fn parse_expression(&mut self) -> Result<Expression, CompilationError> {
        match &self.current().kind {
            TokenKind::Word(word) => match word.as_str() {
                "when" => self.parse_when_expression(),
                _ => self.parse_binary_expression(None),
            },
            _ => self.parse_binary_expression(None),
        }
    }

    fn parse_when_expression(&mut self) -> Result<Expression, CompilationError> {
        let start_token = self.consume().clone(); // Consume "when" token
        let condition = self.parse_expression()?;

        let mut statements = self.parse_statement_list_expression()?;

        let out_statement = statements.pop();
        let out_expr = match out_statement {
            Some(last_statement) => match last_statement.kind {
                StatementKind::Out(e) => e,
                _ => {
                    return Err(CompilationError {
                        desctiption: "`when` expressions should end with an `out` expression"
                            .to_string(),
                        span: TextSpan::from_spans(start_token.span, self.peak(-1).span.clone()),
                    })
                }
            },
            None => {
                return Err(CompilationError {
                    desctiption: "`when` expressions should contain an `out` expression"
                        .to_string(),
                    span: TextSpan::from_spans(start_token.span, self.peak(-1).span.clone()),
                })
            }
        };

        Ok(Expression::new(
            ExpressionKind::When(WhenExpression {
                condition: Box::new(condition),
                statements,
                out: Box::new(out_expr),
            }),
            TextSpan::from_spans(start_token.span, self.peak(-1).span.clone()),
        ))
    }

    /// Returns the operator if the current token is an operator
    fn get_binary_operator(&mut self) -> Option<BinaryOperator> {
        let token = self.current();
        match token.kind {
            TokenKind::Plus => Some(BinaryOperator::new(BinaryOperatorKind::Plus)),
            TokenKind::Minus => Some(BinaryOperator::new(BinaryOperatorKind::Minus)),
            TokenKind::Asterisk => Some(BinaryOperator::new(BinaryOperatorKind::Multiply)),
            TokenKind::Slash => Some(BinaryOperator::new(BinaryOperatorKind::Divide)),
            TokenKind::LargerThan => Some(BinaryOperator::new(BinaryOperatorKind::LargerThan)),
            TokenKind::LargerThanEquals => {
                Some(BinaryOperator::new(BinaryOperatorKind::LargerThanOrEqual))
            }
            TokenKind::LessThan => Some(BinaryOperator::new(BinaryOperatorKind::LessThan)),
            TokenKind::LessThanEquals => {
                Some(BinaryOperator::new(BinaryOperatorKind::LessThanOrEqual))
            }
            TokenKind::DoubleEquals => Some(BinaryOperator::new(BinaryOperatorKind::Equals)),
            TokenKind::BangEquals => Some(BinaryOperator::new(BinaryOperatorKind::NotEquals)),
            _ => None,
        }
    }

    /// Parse a binary expression.
    fn parse_binary_expression(
        &mut self,
        mut left: Option<Expression>,
    ) -> Result<Expression, CompilationError> {
        if left.is_none() {
            let left = self.parse_primary_expression()?;
            return self.parse_binary_expression(Some(left));
        }

        let left_operator = if let Some(op) = self.get_binary_operator() {
            self.consume();
            op
        } else {
            return Ok(left.unwrap());
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

        left = Some(Expression::new(
            ExpressionKind::Binary(BinaryExpression {
                left: Box::new(left.clone().unwrap()),
                right: Box::new(right.clone()),
                operator: left_operator.clone(),
                return_type: left_operator.return_type(),
            }),
            TextSpan {
                start: left.clone().unwrap().span.start,
                end: right.span.end,
                text: left.unwrap().span.text,
            },
        ));

        if self.get_binary_operator().is_some() {
            self.parse_binary_expression(left)
        } else {
            Ok(left.unwrap())
        }
    }

    /// Parse a primary expression.
    fn parse_primary_expression(&mut self) -> Result<Expression, CompilationError> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Number(number) => {
                self.consume();
                Ok(Expression::new(
                    ExpressionKind::Int(IntExpression::new(*number as i32)),
                    TextSpan::from_spans(start_token.span, self.peak(-1).span.clone()),
                ))
            }
            TokenKind::Word(word) => {
                // TODO remove all the `return statements`
                self.consume();
                let current_token = self.current();

                if word == "true" {
                    return Ok(Expression::bool(true, self.peak(-1).span.clone()));
                } else if word == "false" {
                    return Ok(Expression::bool(false, self.peak(-1).span.clone()));
                }
                // If is defined variable
                else if let Some(var) = self.search_scope(word) {
                    if self.current().kind == TokenKind::LeftSquare {
                        self.consume();
                        if let TokenKind::Word(signal) = self.consume().kind.clone() {
                            self.consume_and_check(TokenKind::RightSquare)?;
                            return Ok({
                                let kind = ExpressionKind::Pick(PickExpression::new(signal, var));
                                let span = TextSpan::from_spans(
                                    start_token.span,
                                    self.peak(-1).span.clone(),
                                );
                                Expression { kind, span }
                            });
                        }
                        return Ok({
                            let kind = ExpressionKind::Error;
                            let span =
                                TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                            Expression { kind, span }
                        });
                    }
                    return Ok({
                        let span =
                            TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        let var_ref = VariableRef::new(var, span.clone());
                        let kind = ExpressionKind::VariableRef(var_ref); // TODO
                        Expression { kind, span }
                    });
                } else if current_token.kind == TokenKind::Colon {
                    // If is variable definition
                    self.consume();
                    let t = self.parse_variable_type()?;
                    let var = Rc::new(Variable::new(
                        word.to_string(),
                        t,
                        TextSpan {
                            start: start_token.span.start,
                            end: self.peak(-1).span.end,
                            text: start_token.span.text.clone(),
                        },
                    ));
                    self.add_to_scope(var.clone());
                    return Ok({
                        let kind = ExpressionKind::VariableDef(var);
                        let span =
                            TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        Expression { kind, span }
                    });
                } else if let Some(block) = self.search_blocks(word) {
                    let block_link_expr = self.parse_block_link(block)?;
                    return Ok({
                        let kind = ExpressionKind::BlockLink(block_link_expr);
                        let span =
                            TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        Expression { kind, span }
                    });
                } else {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &start_token.span,
                        &format!("Variable `{}` not defined.", word),
                    );
                    return Ok({
                        let kind = ExpressionKind::Error;
                        let span =
                            TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                        Expression { kind, span }
                    });
                };
            }
            TokenKind::LeftParen => {
                self.consume();
                let inner = self.parse_expression()?;
                let expr = {
                    let kind = ExpressionKind::Parenthesized(ParenthesizedExpression::new(
                        Box::new(inner),
                    ));
                    let span = TextSpan::from_spans(start_token.span, self.peak(-1).span.clone());
                    Expression { kind, span }
                };
                self.consume_and_check(TokenKind::RightParen)?;
                Ok(expr)
            }
            _ => Err(CompilationError::new(
                format!(
                    "Primary expressions can not start with `{}`.",
                    start_token.kind
                ),
                start_token.span,
            )),
        }
    }

    /// Parse a chef block link.
    fn parse_block_link(
        &mut self,
        block: Rc<Block>,
    ) -> Result<BlockLinkExpression, CompilationError> {
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

enum ParsedVariable {
    Def(Variable),
    Ref(VariableRef),
}

#[test]
fn parse_binary_expression() {
    use super::*;

    let code = "1+2*3-4";
    let (text, bag, lexer) = Lexer::new_bundle(code);

    let expected_expr = Expression {
        kind: ExpressionKind::Binary(BinaryExpression {
            return_type: ExpressionReturnType::Int,
            left: Box::new(Expression {
                kind: ExpressionKind::Int(IntExpression { number: 1 }),
                span: TextSpan::new(0, 1, text.clone()),
            }),
            right: Box::new(Expression {
                kind: ExpressionKind::Binary(BinaryExpression {
                    return_type: ExpressionReturnType::Int,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Binary(BinaryExpression {
                            return_type: ExpressionReturnType::Int,
                            left: Box::new(Expression {
                                kind: ExpressionKind::Int(IntExpression { number: 2 }),
                                span: TextSpan {
                                    start: 2,
                                    end: 3,
                                    text: text.clone(),
                                },
                            }),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Int(IntExpression { number: 3 }),
                                span: TextSpan {
                                    start: 4,
                                    end: 5,
                                    text: text.clone(),
                                },
                            }),
                            operator: BinaryOperator {
                                kind: BinaryOperatorKind::Multiply,
                            },
                        }),
                        span: TextSpan {
                            start: 2,
                            end: 5,
                            text: text.clone(),
                        },
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::Int(IntExpression { number: 4 }),
                        span: TextSpan {
                            start: 6,
                            end: 7,
                            text: text.clone(),
                        },
                    }),
                    operator: BinaryOperator {
                        kind: BinaryOperatorKind::Minus,
                    },
                }),
                span: TextSpan {
                    start: 2,
                    end: 7,
                    text: text.clone(),
                },
            }),
            operator: BinaryOperator {
                kind: BinaryOperatorKind::Plus,
            },
        }),
        span: TextSpan::new(0, 7, text.clone()),
    };

    let opts = crate::cli::Opts::new_test();
    let mut parser = Parser::_from_lexer(lexer, bag, Rc::new(opts));
    let parsed_expr = parser.parse_binary_expression(None).unwrap();

    assert_eq!(parsed_expr, expected_expr);
}
