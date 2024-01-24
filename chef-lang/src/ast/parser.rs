//! Module for parsing a token stream into an abstract syntax tree.

use std::cmp::min;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use crate::ast::lexer::{Token, TokenKind};
use crate::ast::{
    BinaryExpression, BinaryOperator, Expression, ExpressionKind, Mutation,
    ParenthesizedExpression, Statement, StatementKind, Variable, VariableType,
};
use crate::cli::Opts;
use crate::diagnostics::{CompilationError, CompilationResult, DiagnosticsBag, DiagnosticsBagRef};
use crate::text::{SourceText, TextSpan};

use super::lexer::Lexer;
use super::{python_macro, IndexExpression};
use super::{
    Assignment, AssignmentKind, Block, BlockLinkExpression, MutationOperator, PickExpression,
    VariableRef, VariableSignalType, WhenExpression,
};

// TODO: Add example
/// A list of statements with an optional return expression at its end.
#[derive(Debug, Clone, PartialEq)]
pub struct StatementList {
    pub statements: Vec<Statement>,
    pub out: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
enum ScopedItem {
    Var(Rc<Variable>),
    Const(Constant),
}

#[derive(Debug, Clone, PartialEq)]
enum Constant {
    Int(i32),
    Bool(bool),
}

enum Directive {
    Block(Block),
    Constant,
    Unknown,
}

/// The parser. The parser can be used as an iterator to get statements one at a time.
pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    scopes: Vec<HashMap<String, ScopedItem>>,
    blocks: Vec<Rc<Block>>,
    diagnostics_bag: DiagnosticsBagRef,
    options: Rc<Opts>,
    next_blocks: VecDeque<Block>,
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
            scopes: vec![HashMap::new()],
            blocks: vec![],
            diagnostics_bag,
            options,
            next_blocks: VecDeque::new(),
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
            scopes: vec![HashMap::new()],
            blocks: vec![],
            diagnostics_bag,
            options,
            next_blocks: VecDeque::new(),
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

    /// Return the current token and move to the next token.
    fn consume(&mut self) -> &Token {
        if self.options.verbose {
            println!("{} : {:?}", self.cursor.clone(), &self.current().kind);
        }

        self.cursor += 1;
        self.peak(-1)
    }

    /// Comsume a token and expect it to be a word. Return the word as a slice.
    fn consume_word(&mut self) -> CompilationResult<&str> {
        let token = self.consume();
        if let TokenKind::Word(word) = &token.kind {
            Ok(word.as_str())
        } else {
            Err(CompilationError::new_localized(
                format!("Expected 'word' but found '{}'.", token.kind),
                token.span.clone(),
            ))
        }
    }

    fn consume_number(&mut self) -> CompilationResult<u16> {
        let token = self.consume();
        if let TokenKind::Number(n) = &token.kind {
            Ok(*n)
        } else {
            Err(CompilationError::new_localized(
                format!("Expected 'word' but found '{}'.", token.kind),
                token.span.clone(),
            ))
        }
    }

    /// Consume only if the token is of a certain kind.
    fn consume_if(&mut self, token_kind: TokenKind) {
        if self.current().kind == token_kind {
            self.consume();
        }
    }

    /// Consume and error if the token is not what was expected.
    fn consume_and_expect(&mut self, expected: TokenKind) -> Result<TokenKind, CompilationError> {
        let token = self.consume().clone();
        let is_correct = token.kind == expected;
        if !is_correct {
            Err(CompilationError::new_unexpected_token(token, expected))
        } else {
            Ok(token.kind)
        }
    }

    /// Search the current scope for a variable.
    fn search_scope(&self, name: &str) -> Option<ScopedItem> {
        let mut rev_scopes = self.scopes.clone();
        rev_scopes.reverse();
        for scope in rev_scopes {
            for (item_name, item) in scope {
                if item_name == name {
                    return Some(item);
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
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope.
    fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop().unwrap();
        }
    }

    /// Add a scope item to the current scope.
    fn add_to_scope(&mut self, name: String, item: ScopedItem) {
        self.scopes.last_mut().unwrap().insert(name, item);
    }

    fn add_var_to_scope(&mut self, var: Rc<Variable>) {
        self.add_to_scope(var.name.clone(), ScopedItem::Var(var))
    }

    fn next_directive(&mut self) -> Option<Directive> {
        // Return queued blocks first if any
        if let Some(b) = self.next_blocks.pop_front() {
            return Some(Directive::Block(b));
        }

        // Stop if at the end of token stream
        if self.current().is_end() {
            return None;
        }

        let directive = self.parse_directive();

        match directive {
            Ok(d) => Some(d),
            Err(e) => {
                self.diagnostics_bag
                    .borrow_mut()
                    .report_compilation_error(e);
                None
            }
        }
    }

    fn parse_directive(&mut self) -> CompilationResult<Directive> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Word(word) => match word.as_str() {
                "block" => {
                    let block = self.parse_block()?;
                    self.blocks.push(Rc::new(block.clone()));
                    Ok(Directive::Block(block))
                }
                "import" => {
                    let blocks = self.parse_import()?;
                    self.next_blocks.extend(blocks);
                    match self.next_blocks.pop_front() {
                        Some(b) => Ok(Directive::Block(b)),
                        None => self.parse_directive(),
                    }
                }
                "const" => self.parse_constant(),
                _ => {
                    let token = self.consume();
                    Err(CompilationError::new_localized(
                        format!("Unknown keyword '{}'.", word),
                        token.span.clone(),
                    ))
                }
            },
            _ => {
                self.diagnostics_bag.borrow_mut().report_error(
                    &start_token.span,
                    &format!("Unknown keyword '{}'", start_token.kind),
                );
                self.consume();
                Ok(Directive::Unknown)
            }
        }
    }

    /// Parse next statement. Return `None` of none are left.
    // TODO: Simplify type if possible
    fn parse_statement(&mut self) -> Option<CompilationResult<Statement>> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Word(word) => {
                let kind = match word.as_str() {
                    "out" => {
                        self.consume();
                        let statement_start = self.current().span.start;
                        if let Ok(expr) = self.parse_expression() {
                            if let Err(e) = self.consume_and_expect(TokenKind::Semicolon) {
                                self.diagnostics_bag
                                    .borrow_mut()
                                    .report_compilation_error(e);
                                return None;
                            }
                            Ok(StatementKind::Out(expr))
                        } else {
                            self.consume_bad_statement();
                            let statement_end = self.peak(-1).span.end;
                            let text = self.peak(-1).span.text.clone();
                            self.diagnostics_bag.borrow_mut().report_error(
                                &TextSpan::new(statement_start, statement_end, text),
                                "Bad output expression",
                            ); // TODO: use error message here
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
                    "when" => {
                        let when_expr = match self.parse_when_expression() {
                            Ok(expr) => expr,
                            Err(e) => return Some(Err(e)),
                        };

                        match &self.current().kind {
                            TokenKind::Semicolon => {
                                self.consume();
                                Ok(StatementKind::Expression(when_expr))
                            }
                            _ => Ok(StatementKind::Out(when_expr)),
                        }
                    }
                    // Operate
                    var if self.peak(1).kind == TokenKind::Bang => match self.search_scope(var) {
                        Some(ScopedItem::Var(var)) => self.parse_variable_operation_statement(var),
                        Some(ScopedItem::Const(_)) => Err(CompilationError::new_localized(
                            "Constants have can not be operated upon.",
                            start_token.span.clone(),
                        )),
                        None => todo!(),
                    },
                    _ if self.is_at_assignment_statment() => self.parse_assignment_statement(),
                    _ if self.is_at_mutation_statment() => self.parse_mutation_statement(),
                    _ => match self.parse_expression() {
                        Ok(expr) => Ok(StatementKind::Out(expr)),
                        Err(e) => Err(e),
                    },
                };
                let kind = match kind {
                    Ok(k) => k,
                    Err(e) => {
                        self.consume_bad_statement();
                        self.diagnostics_bag
                            .borrow_mut()
                            .report_compilation_error(e);
                        return None;
                    }
                };
                Some(Ok(Statement::new(
                    kind,
                    TextSpan::new(
                        start_token.span.start,
                        self.peak(-1).span.end,
                        start_token.span.text.clone(),
                    ),
                )))
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
                Some(Ok(Statement::new(
                    StatementKind::Out(out_expr),
                    self.get_span_from(&start_token.span),
                )))
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

    /// Returns true if the cursor is at the begining of an mutation statement.
    fn is_at_mutation_statment(&self) -> bool {
        matches!(
            &self.peak(1).kind,
            &TokenKind::PlusEquals
                | &TokenKind::MinusEquals
                | &TokenKind::AsteriskEquals
                | &TokenKind::SlashEquals
        )
    }

    /// Returns true if the cursor is at the begining of an assignment statement.
    fn is_at_assignment_statment(&self) -> bool {
        matches!(&self.peak(1).kind, TokenKind::Colon | TokenKind::Equals)
    }

    /// Parse variable assignment statement.
    fn parse_assignment_statement(&mut self) -> Result<StatementKind, CompilationError> {
        let start_span = self.current().span.clone();
        let mut variable = match self.parse_variable()? {
            ParsedVariable::Def(v) => v,
            _ => {
                self.diagnostics_bag.borrow_mut().report_error(
                    &self.get_span_from(&start_span),
                    "Variables cannot be reassigned.",
                );
                self.consume_bad_statement();
                return Ok(StatementKind::Error);
            }
        };

        match &variable.type_ {
            VariableType::Var(_) => {
                self.consume_and_expect(TokenKind::Semicolon)?;

                // `var` type variable is always zero initialized, because memory cells in factorio
                // cannot be assigned values.
                let var = Rc::new(variable);
                self.add_var_to_scope(var.clone());
                let zero_expr = Expression::number(0, self.get_span_from(&start_span));
                return Ok(StatementKind::Assignment(Assignment::new(
                    var,
                    zero_expr,
                    AssignmentKind::Var,
                )));
            }
            VariableType::Counter(_) => {
                self.consume_and_expect(TokenKind::Semicolon)?;

                // `counter` type variable is always zero initialized, because memory cells in factorio
                // cannot be assigned values.
                let var = Rc::new(variable);
                self.add_var_to_scope(var.clone());
                let zero_expr = Expression::number(0, self.get_span_from(&start_span));
                return Ok(StatementKind::Assignment(Assignment::new(
                    var,
                    zero_expr,
                    AssignmentKind::Counter,
                )));
            }
            VariableType::Register(_) => {
                let var = Rc::new(variable.clone());
                self.add_var_to_scope(var.clone());
                self.consume_and_expect(TokenKind::Equals)?;
                let input = self.parse_expression()?;
                self.consume_and_expect(TokenKind::Semicolon)?;
                return Ok(StatementKind::Assignment(Assignment::new(
                    var,
                    input,
                    AssignmentKind::Register,
                )));
            }
            VariableType::Bool(_) => {}
            VariableType::Int(_) => {}
            VariableType::All => {}
            VariableType::ConstInt(_) => {}
            VariableType::ConstBool(_) => {}
        };

        if self.current().kind != TokenKind::Equals {
            self.diagnostics_bag
                .borrow_mut()
                .report_unexpected_token(self.current(), TokenKind::Equals);
            self.consume_bad_statement();
            return Ok(StatementKind::Error);
        }
        self.consume(); // Consume equals

        let expr = self.parse_expression()?;

        // TODO: This check should probably be done by the type checker
        match &expr.kind {
            ExpressionKind::Bool(v) => {
                variable.type_ = {
                    if let VariableType::Bool(_) = variable.type_ {
                        VariableType::ConstBool(*v)
                    } else {
                        return Err(CompilationError::new_localized(
                            format!("Can not assign variable `{}` of type `{}` to expression returning `bool` type.",
                                    variable.name,
                                    variable.type_
                                    ), TextSpan::from_spans(&variable.span, &expr.span)));
                    }
                }
            }
            ExpressionKind::Int(v) => {
                variable.type_ = {
                    if let VariableType::Int(_) = variable.type_ {
                        VariableType::ConstInt(*v)
                    } else {
                        return Err(CompilationError::new_localized(
                            format!("Can not assign variable `{}` of type `{}` to expression returning `int` type.",
                                    variable.name,
                                    variable.type_
                                    ), TextSpan::from_spans(&variable.span, &expr.span)));
                    }
                }
            }
            ExpressionKind::Negative(e) => {
                variable.type_ = {
                    if let ExpressionKind::Int(n) = e.kind {
                        // TODO: This check is copy pasted from above and should definetely be handled
                        // somewhere else.
                        if let VariableType::Int(_) = variable.type_ {
                            VariableType::ConstInt(n)
                        } else {
                            return Err(CompilationError::new_localized(
                            format!("Can not assign variable `{}` of type `{}` to expression returning `int` type.",
                                    variable.name,
                                    variable.type_
                                    ), TextSpan::from_spans(&variable.span, &expr.span)));
                        }
                    } else {
                        return Err(CompilationError::new_localized(
                            "Negative values must be integers.",
                            TextSpan::from_spans(&variable.span, &expr.span),
                        ));
                    }
                }
            }
            ExpressionKind::Binary(_) => {}
            ExpressionKind::Parenthesized(_) => {}
            ExpressionKind::Pick(_) => {}
            ExpressionKind::Index(_) => {}
            ExpressionKind::VariableRef(_) => {}
            ExpressionKind::BlockLink(_) => {}
            ExpressionKind::When(_) => {}
            ExpressionKind::Error => {}
        }

        let var_ref = Rc::new(variable);
        self.add_var_to_scope(var_ref.clone());

        self.consume_and_expect(TokenKind::Semicolon)?;
        Ok(StatementKind::Assignment(Assignment::new(
            var_ref,
            expr,
            AssignmentKind::Sig,
        )))
    }

    fn consume_mutation_operator(&mut self) -> Result<MutationOperator, CompilationError> {
        let token = self.consume();
        match token.kind.clone() {
            TokenKind::PlusEquals => Ok(MutationOperator::Add),
            TokenKind::MinusEquals => Ok(MutationOperator::Subtract),
            k => Err(CompilationError::new_localized(
                format!("Expected mutation operator, found `{}`.", k),
                token.span.clone(),
            )),
        }
    }

    /// Parse variable mutation statement.
    fn parse_mutation_statement(&mut self) -> Result<StatementKind, CompilationError> {
        let start_span = self.current().span.clone();
        let var_ref = if let ParsedVariable::Ref(v) = self.parse_variable()? {
            v
        } else {
            self.diagnostics_bag.borrow_mut().report_error(
                &self.get_span_from(&start_span),
                "Only defined variables can be mutated.",
            );
            self.consume_bad_statement();
            return Ok(StatementKind::Error);
        };

        let var = if let VariableType::Var(_) = var_ref.var.type_.clone() {
            var_ref
        } else {
            self.diagnostics_bag.borrow_mut().report_error(
                &self.get_span_from(&start_span),
                &format!(
                    "Only `var` type variables can be mutated. Found variable of type: `{}`",
                    var_ref.var.type_
                ),
            );
            self.consume_bad_statement();
            return Ok(StatementKind::Error);
        };

        let op = self.consume_mutation_operator()?;
        let expr = self.parse_expression()?;
        self.consume_and_expect(TokenKind::Semicolon)?;

        Ok(StatementKind::Mutation(Mutation::new(var, op, expr)))
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
                let word = self.consume_word()?.to_string();
                self.consume_and_expect(TokenKind::RightParen)?;
                Ok(VariableSignalType::Signal(word))
            }
            _ => Ok(VariableSignalType::Any),
        }
    }

    fn parse_variable(&mut self) -> Result<ParsedVariable, CompilationError> {
        let start_token = self.current().clone();

        let name = self.consume_word()?.to_string();

        // If variable reference
        if self.current().kind != TokenKind::Colon {
            let item = self
                .search_scope(&name)
                .ok_or(CompilationError::new_localized(
                    format!("Variable `{}` not defined.", name),
                    start_token.span.clone(),
                ))?;
            Ok(match item {
                ScopedItem::Var(v) => ParsedVariable::Ref(VariableRef::new(v, start_token.span)),
                ScopedItem::Const(c) => ParsedVariable::Const(c),
            })
        } else {
            // If variable definition
            self.consume_and_expect(TokenKind::Colon)?;

            let type_ = self.parse_variable_type()?;

            Ok(ParsedVariable::Def(Variable::new(
                name.to_owned(),
                type_,
                self.get_span_from(&start_token.span),
            )))
        }
    }

    /// Parse variable type.
    fn parse_variable_type(&mut self) -> Result<VariableType, CompilationError> {
        let token = self.consume();
        match token.kind.clone() {
            TokenKind::Word(start_word) => match start_word.as_str() {
                "bool" => Ok(VariableType::Bool(self.parse_variable_type_signal()?)),
                "int" => Ok(VariableType::Int(self.parse_variable_type_signal()?)),
                "var" => Ok(VariableType::Var(self.parse_variable_type_signal()?)),
                "all" => Ok(VariableType::All),
                "counter" => {
                    self.consume_and_expect(TokenKind::LeftParen)?;
                    let sig_token = self.consume();
                    let type_ = if let TokenKind::Word(t) = &sig_token.kind {
                        VariableSignalType::Signal(t.clone())
                    } else {
                        return Err(CompilationError::new_localized(
                            format!("Expected signal for counter, found: `{}`", sig_token.kind),
                            sig_token.span.clone(),
                        ));
                    };
                    self.consume_and_expect(TokenKind::Colon)?;
                    let limit_expr = self.parse_expression()?;
                    self.consume_and_expect(TokenKind::RightParen)?;
                    Ok(VariableType::Counter((type_, Box::new(limit_expr))))
                }
                "reg" => {
                    self.consume_and_expect(TokenKind::LeftParen)?;
                    let n = self.consume_number()?;
                    self.consume_and_expect(TokenKind::RightParen)?;
                    Ok(VariableType::Register(n))
                }
                w => Err(CompilationError::new_localized(
                    format!("Unknown type `{}`.", w),
                    token.span.clone(),
                )),
            },
            _ => Err(CompilationError::new_localized(
                format!("Expected variable type to be word, not `{}`", token.kind),
                token.span.clone(),
            )),
        }
    }

    /// Parse arguments for `block` definition.
    fn parse_block_arguments(&mut self) -> Result<Vec<Rc<Variable>>, CompilationError> {
        let mut arguments: Vec<Rc<Variable>> = vec![];
        while self.current().kind != TokenKind::RightParen {
            let var_span = self.current().span.clone();
            let var = match self.parse_variable()? {
                ParsedVariable::Def(v) => Ok(v),

                // TODO: this will never happen as parse_variable_type til return an error on undefined variables.
                ParsedVariable::Ref(_) => Err(CompilationError::new_localized(
                    "Please give variable a type.".to_string(),
                    var_span,
                )),

                ParsedVariable::Const(_) => Err(CompilationError::new_localized(
                    "Constants cannot be block inputs.".to_string(),
                    var_span,
                )),
            }?;
            let var_ref = Rc::new(var);
            self.add_var_to_scope(var_ref.clone());
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
        self.consume_and_expect(TokenKind::LeftParen)?;
        loop {
            if self.current().kind == TokenKind::Comma {
                self.consume();
            }
            if self.current().kind == TokenKind::RightParen {
                break;
            }
            inputs.push(self.parse_expression()?);
        }
        self.consume_and_expect(TokenKind::RightParen)?;
        Ok(inputs)
    }

    fn parse_statement_list_expression(&mut self) -> Result<StatementList, CompilationError> {
        self.consume_and_expect(TokenKind::LeftCurly)?;

        let mut statements: Vec<Statement> = vec![];
        let mut out = None;

        while let Some(statement) = self.parse_statement() {
            statements.push(statement?)
        }

        if let Some(StatementKind::Out(out_expr)) = statements.last().map(|s| &s.kind) {
            out = Some(Box::new(out_expr.clone()));
            statements.pop();
        }

        // Make sure no output statements are left
        for statement in &statements {
            if let StatementKind::Out(_) = statement.kind {
                return Err(CompilationError::new_localized(
                    "Output statements have to be last.".to_string(),
                    statement.span.clone(),
                ));
            }
        }

        self.consume_and_expect(TokenKind::RightCurly)?;

        Ok(StatementList { statements, out })
    }

    // TODO: Constants should be able to be imported
    /// Parse chef `import`
    fn parse_import(&mut self) -> CompilationResult<Vec<Block>> {
        self.consume(); // Consume "import" word

        let file_token = self.consume().clone();

        if let TokenKind::Literal(file) = &file_token.kind {
            let text = if file.ends_with(".py") {
                python_macro::run_python_import(
                    self.options.clone(),
                    Some(file_token.span),
                    file.as_str(),
                )?
            } else {
                match SourceText::from_file(file.as_str()) {
                    Ok(text) => Ok(text),
                    Err(e) => Err(CompilationError::new_localized(
                        format!("Could not read imported file '{}': {}", file, e),
                        file_token.span,
                    )),
                }?
            };

            let text = Rc::new(text);

            let diagnostics_bag = DiagnosticsBag::new_ref(self.options.clone(), text.clone());
            let tokens = Lexer::from_source(diagnostics_bag.clone(), text).collect();
            let mut import_parser = Parser::new(tokens, diagnostics_bag, self.options.clone());
            let mut blocks = vec![];
            for cs in &mut import_parser {
                blocks.push(cs);
            }
            self.blocks.extend(import_parser.blocks);
            Ok(blocks)
        } else {
            Err(CompilationError::new_unexpected_token(
                file_token.clone(),
                TokenKind::Literal("path".to_string()),
            ))
        }
    }

    fn parse_constant(&mut self) -> CompilationResult<Directive> {
        // Consume 'const' keyword
        let start_token = self.consume();
        debug_assert_eq!(start_token.kind, TokenKind::Word("const".to_string()));

        let name = self.consume_word()?.to_string();

        self.consume_and_expect(TokenKind::Equals)?;

        // let value_token = self.consume();

        let const_expr = self.parse_expression()?;
        let value = crate::ast::constant_evaluator::evaluate_constant_expression(const_expr)?;

        let constant = match value {
            crate::ast::constant_evaluator::ConstantValue::Int(i) => Constant::Int(i),
            crate::ast::constant_evaluator::ConstantValue::Bool(b) => Constant::Bool(b),
        };

        self.add_to_scope(name, ScopedItem::Const(constant));

        Ok(Directive::Constant)
    }

    /// Parse chef `block`.
    fn parse_block(&mut self) -> CompilationResult<Block> {
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

        self.consume_and_expect(TokenKind::LeftParen)?;

        let inputs = self.parse_block_arguments()?;

        self.consume_and_expect(TokenKind::RightParen)?;
        self.consume_and_expect(TokenKind::RightArrow)?;

        let output_type = self.parse_block_outputs()?;

        let statement_list = self.parse_statement_list_expression()?;

        let out_expr = statement_list.out.ok_or({
            CompilationError::new_localized(
                "A `block` must contain an output statement.".to_string(),
                self.get_span_from(&start_token.span),
            )
        })?;

        self.exit_scope();

        Ok(Block::new(
            name,
            inputs,
            output_type,
            statement_list.statements,
            *out_expr,
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
                _ => self.parse_binary_expression(None, 0),
            },
            _ => self.parse_binary_expression(None, 0),
        }
    }

    fn parse_when_expression(&mut self) -> Result<Expression, CompilationError> {
        let start_token = self.consume().clone(); // Consume "when" token
        let condition = self.parse_expression()?;

        self.enter_scope();

        let statements_list = self.parse_statement_list_expression()?;

        self.exit_scope();

        Ok(Expression::new(
            ExpressionKind::When(WhenExpression {
                condition: Box::new(condition),
                statements: statements_list.statements,
                out: statements_list.out,
            }),
            self.get_span_from(&start_token.span),
        ))
    }

    /// Returns the operator if the current token is an operator
    fn get_binary_operator(&mut self) -> Option<BinaryOperator> {
        let token = self.current();
        match token.kind {
            TokenKind::Plus => Some(BinaryOperator::Add),
            TokenKind::Minus => Some(BinaryOperator::Subtract),
            TokenKind::Asterisk => Some(BinaryOperator::Multiply),
            TokenKind::Slash => Some(BinaryOperator::Divide),
            TokenKind::LargerThan => Some(BinaryOperator::LargerThan),
            TokenKind::LargerThanEquals => Some(BinaryOperator::LargerThanOrEqual),
            TokenKind::LessThan => Some(BinaryOperator::LessThan),
            TokenKind::LessThanEquals => Some(BinaryOperator::LessThanOrEqual),
            TokenKind::DoubleEquals => Some(BinaryOperator::Equals),
            TokenKind::BangEquals => Some(BinaryOperator::NotEquals),
            TokenKind::At => Some(BinaryOperator::Combine),
            _ => None,
        }
    }

    /// Parse a binary expression.
    /// https://en.wikipedia.org/wiki/Operator-precedence_parser
    fn parse_binary_expression(
        &mut self,
        left: Option<Expression>,
        min_precedence: u8,
    ) -> Result<Expression, CompilationError> {
        let mut left = match left {
            Some(e) => e,
            None => self.parse_primary_expression()?,
        };

        while let Some(op) = self.get_binary_operator() {
            if op.precedence() < min_precedence {
                break;
            }

            self.consume(); // advance to next token
            let mut right = self.parse_primary_expression()?;

            while let Some(right_op) = self.get_binary_operator() {
                if right_op.precedence() <= op.precedence() {
                    break;
                }
                right = self.parse_binary_expression(Some(right), right_op.precedence())?;
            }

            left = Expression::new(
                ExpressionKind::Binary(BinaryExpression {
                    left: Box::new(left.clone()),
                    right: Box::new(right.clone()),
                    operator: op.clone(),
                    return_type: op.return_type(),
                }),
                TextSpan {
                    start: left.span.clone().start,
                    end: right.span.end,
                    text: left.span.clone().text,
                },
            );
        }

        Ok(left)
    }

    /// Parse a primary expression.
    fn parse_primary_expression(&mut self) -> Result<Expression, CompilationError> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Minus => {
                self.consume();
                let inner = self.parse_primary_expression()?;
                let inner_span = inner.span.clone();
                Ok(Expression::new(
                    ExpressionKind::Negative(Box::new(inner)),
                    TextSpan::from_spans(&start_token.span, &inner_span),
                ))
            }
            TokenKind::Number(number) => {
                self.consume();
                Ok(Expression::new(
                    ExpressionKind::Int((*number).into()),
                    self.get_span_from(&start_token.span),
                ))
            }
            TokenKind::Word(word) if word == "true" => {
                self.consume();
                Ok(Expression::bool(true, self.peak(-1).span.clone()))
            }
            TokenKind::Word(word) if word == "false" => {
                self.consume();
                Ok(Expression::bool(false, self.peak(-1).span.clone()))
            }
            TokenKind::Word(word) => {
                let item_span = self.consume().span.clone();

                // If is defined variable
                if let Some(item) = self.search_scope(word) {
                    match item {
                        ScopedItem::Var(var) => {
                            // If pick
                            match self.current().kind {
                                TokenKind::LeftSquare => self.parse_variable_index(var),
                                _ => Ok({
                                    let span = self.get_span_from(&start_token.span);
                                    let var_ref = VariableRef::new(var, span.clone());
                                    let kind = ExpressionKind::VariableRef(var_ref);
                                    Expression { kind, span }
                                }),
                            }
                        }
                        ScopedItem::Const(Constant::Int(n)) => {
                            Ok(Expression::new(ExpressionKind::Int(n), item_span))
                        }
                        ScopedItem::Const(Constant::Bool(b)) => {
                            Ok(Expression::new(ExpressionKind::Bool(b), item_span))
                        }
                    }
                } else if let Some(block) = self.search_blocks(word) {
                    let block_link_expr = self.parse_block_link(block)?;
                    Ok({
                        let kind = ExpressionKind::BlockLink(block_link_expr);
                        let span = self.get_span_from(&start_token.span);
                        Expression { kind, span }
                    })
                } else {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &start_token.span,
                        &format!("Variable `{}` not defined.", word),
                    );
                    Ok({
                        let kind = ExpressionKind::Error;
                        let span = self.get_span_from(&start_token.span);
                        Expression { kind, span }
                    })
                }
            }
            TokenKind::LeftParen => {
                self.consume();
                let inner = self.parse_expression()?;
                let expr = {
                    let kind = ExpressionKind::Parenthesized(ParenthesizedExpression::new(
                        Box::new(inner),
                    ));
                    let span = self.get_span_from(&start_token.span);
                    Expression { kind, span }
                };
                self.consume_and_expect(TokenKind::RightParen)?;
                Ok(expr)
            }
            _ => Err(CompilationError::new_localized(
                format!(
                    "Primary expressions can not start with `{}`.",
                    start_token.kind
                ),
                start_token.span,
            )),
        }
    }

    fn parse_variable_index(&mut self, var: Rc<Variable>) -> CompilationResult<Expression> {
        let start_span = self.peak(-1).span.clone();
        self.consume_and_expect(TokenKind::LeftSquare)?;
        let index_token = self.consume();

        let expr_kind = match &index_token.kind {
            TokenKind::Word(signal) => ExpressionKind::Pick(PickExpression::new(
                signal.to_string(),
                VariableRef::new(var, self.get_span_from(&start_span)),
            )),
            TokenKind::Number(n) => ExpressionKind::Index(IndexExpression {
                var_ref: VariableRef::new(var, start_span.clone()),
                index: *n,
            }),
            _ => {
                return Err(CompilationError::new_localized(
                    "Variables can only be picked/indexed with types and numbers.",
                    self.get_span_from(&start_span),
                ))
            }
        };
        self.consume_and_expect(TokenKind::RightSquare)?;

        let span = self.get_span_from(&start_span);
        Ok(Expression {
            kind: expr_kind,
            span,
        })
    }

    /// Parse `var!` syntax
    fn parse_variable_operation_statement(
        &mut self,
        var: Rc<Variable>,
    ) -> CompilationResult<StatementKind> {
        let start_span = self.peak(-1).span.clone();

        // Consume `var!`
        self.consume_word()?;
        self.consume_and_expect(TokenKind::Bang)?;
        self.consume_and_expect(TokenKind::Semicolon)?;

        match &var.type_ {
            VariableType::Register(_) => Ok(StatementKind::Operation(super::VarOperation {
                var_ref: VariableRef::new(var.clone(), start_span),
            })),
            t => Err(CompilationError::new_localized(
                format!("No standard operation exists for type: `{}`.", t),
                self.get_span_from(&start_span),
            )),
        }
    }

    /// Parse a chef block link.
    fn parse_block_link(
        &mut self,
        block: Rc<Block>,
    ) -> Result<BlockLinkExpression, CompilationError> {
        let start = self.peak(-1).span.start;
        let inputs = self.parse_block_link_arguments()?;
        let end = self.current().span.clone();

        if inputs.len() != block.inputs.len() {
            self.diagnostics_bag.borrow_mut().report_error(
                &TextSpan::new(start, end.end, end.text),
                &format!(
                    // TODO: Maybe report function signiture instead of just name
                    "Expected {} arguments for function '{}'. Found {}.",
                    block.inputs.len(),
                    block.name,
                    inputs.len(),
                ),
            )
        }

        Ok(BlockLinkExpression::new(block, inputs))
    }

    fn get_span_from(&self, start_span: &TextSpan) -> TextSpan {
        TextSpan::from_spans(start_span, &self.peak(-1).span)
    }
}

impl Iterator for Parser {
    type Item = Block;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_directive() {
            Some(Directive::Block(block)) => Some(block),
            Some(_) => self.next(),
            None => None,
        }
    }
}

enum ParsedVariable {
    Def(Variable),
    Ref(VariableRef),
    Const(Constant),
}
