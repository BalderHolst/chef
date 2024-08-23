//! Module for parsing a token stream into an abstract syntax tree.

use crate::cli::Opts;
use crate::compiler::graph::WireKind;
use crate::diagnostics::{CompilationError, CompilationResult, DiagnosticsBagRef};
use crate::text::TextSpan;
use crate::{
    ast::{
        lexer::{Token, TokenKind},
        BinaryExpression, BinaryOperator, Block, DynBlock, Expression, ExpressionKind,
        GateExpression, OutputAssignment, ParenthesizedExpression, Statement, StatementKind,
        TupleDeclarationDefinition, VariableType,
    },
    diagnostics::DiagnosticsBag,
    text::SourceText,
};
use std::cell::RefCell;
use std::cmp::min;
use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;

use super::{
    lexer::Lexer, AssignmentType, BlockLinkArg, BlockLinkExpression, Declaration,
    DeclarationDefinition, Definition, DefinitionKind, Directive, DynBlockArg, Import,
    IndexExpression, MutVar, NegativeExpression, OperatorKind, PickExpression, SizeOfExpression,
    VarData, Variable, VariableRef, VariableSignalType, WhenStatement, AST,
};

enum Module {
    /// Module embedded in compiler
    Static(&'static static_files::File),

    /// Module included from file
    Dynamic(String),
}

const STD_LIB: static_files::FileStash = static_files::static_files! {
    "stdlib/std.rcp",
};

// TODO: Add example
/// A list of statements with an optional return expression at its end.
#[derive(Debug, Clone, PartialEq)]
pub struct StatementList<V>
where
    V: Variable,
{
    pub statements: Vec<Statement<V>>,
    pub out: Option<Box<Expression<V>>>,
}

#[derive(Debug, Clone, PartialEq)]
enum ScopedItem<V> {
    Var(Rc<V>),
    Const(Constant),
}

#[derive(Debug, Clone, PartialEq)]
enum Constant {
    Int(i32),
    Bool(bool),
}

/// The parser. The parser can be used as an iterator to get statements one at a time.
pub struct Parser {
    ast: AST<MutVar>,
    tokens: Vec<Token>,
    cursor: usize,
    scopes: Vec<HashMap<String, ScopedItem<MutVar>>>,
    modules: HashMap<String, Module>,
    diagnostics_bag: DiagnosticsBagRef,
    options: Rc<Opts>,
    next_blocks: VecDeque<Block<MutVar>>,
    next_var_id: usize,
    next_block_id: usize,
}

type BlockArgs = Vec<Rc<MutVar>>;

impl Parser {
    /// Parse a token stream into an [AST].
    pub fn parse(
        tokens: Vec<Token>,
        diagnostics_bag: DiagnosticsBagRef,
        options: Rc<Opts>,
    ) -> AST<MutVar> {
        // Filter out comments
        let tokens = tokens
            .into_iter()
            .filter(|token| !token.kind.is_comment())
            .collect();

        let mut parser = Self::new(tokens, diagnostics_bag, options);

        while let Some(directive) = parser.next_directive() {
            parser.ast.directives.push(directive);
        }

        parser.ast
    }

    /// Create a new [Parser].
    fn new(tokens: Vec<Token>, diagnostics_bag: DiagnosticsBagRef, options: Rc<Opts>) -> Self {
        let mut modules = HashMap::new();

        modules.insert(
            "std".to_string(),
            Module::Static(
                STD_LIB
                    .get_file("stdlib/std.rcp")
                    .expect("stdlib should be embedded"),
            ),
        );

        if let Some(includes) = &options.include {
            for (name, path) in includes {
                modules.insert(name.clone(), Module::Dynamic(path.clone()));
            }
        }

        Self {
            ast: AST::new(diagnostics_bag.clone()),
            tokens: tokens
                .iter()
                .filter(|token| token.kind != TokenKind::Whitespace)
                .cloned()
                .collect(),
            cursor: 0,
            scopes: vec![HashMap::new()],
            diagnostics_bag,
            options,
            next_blocks: VecDeque::new(),
            next_var_id: 0,
            next_block_id: 0,
            modules,
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

        if self.peak(-1).kind.is_comment() {
            self.consume()
        } else {
            self.peak(-1)
        }
    }

    /// Consume until and with provided token kind
    fn consume_until(&mut self, token_kind: &TokenKind) -> &Token {
        loop {
            let current = self.current().kind.clone();

            if current == *token_kind || current == TokenKind::End {
                break;
            }

            self.consume();
        }

        self.consume() // Consume and return ending token
    }

    /// Consume a token and expect it to be a word. Return the word as a slice.
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

    /// Consume a token and expect it to be a string literal. Return the word as a slice.
    fn _consume_string_lit(&mut self) -> CompilationResult<&str> {
        let token = self.consume();
        if let TokenKind::StringLiteral(lit) = &token.kind {
            Ok(lit.as_str())
        } else {
            Err(CompilationError::new_localized(
                format!("Expected string literal but found '{}'.", token.kind),
                token.span.clone(),
            ))
        }
    }

    fn _consume_number(&mut self) -> CompilationResult<u16> {
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
    fn consume_if(&mut self, token_kind: TokenKind) -> bool {
        if self.current().kind == token_kind {
            self.consume();
            true
        } else {
            false
        }
    }

    /// Consume and error if the token is not what was expected.
    fn consume_and_expect(&mut self, expected: TokenKind) -> CompilationResult<Token> {
        let token = self.consume().clone();
        let is_correct = token.kind == expected;
        if !is_correct {
            Err(CompilationError::new_unexpected_token(token, expected))
        } else {
            Ok(token)
        }
    }

    /// Consume and error if the word is not what was expected.
    fn consume_and_expect_word<'a>(&mut self, word: &'a str) -> CompilationResult<&'a str> {
        match self.consume_word()? {
            s if s == word => Ok(word),
            other => Err(CompilationError::new_localized(
                format!("Expected '{word}' but got '{other}'."),
                self.peak(-1).span.clone(),
            )),
        }
    }

    /// Search the current scope for a variable.
    fn search_scope(&self, name: &str) -> Option<ScopedItem<MutVar>> {
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

    /// Get new unique variable id.
    fn get_next_var_id(&mut self) -> usize {
        let id = self.next_var_id;
        self.next_var_id += 1;
        id
    }

    /// Get new unique block id.
    fn get_next_block_id(&mut self) -> usize {
        let id = self.next_block_id;
        self.next_block_id += 1;
        id
    }

    /// Add a scope item to the current scope.
    fn add_to_scope(&mut self, name: String, item: ScopedItem<MutVar>) {
        self.scopes.last_mut().unwrap().insert(name, item);
    }

    /// Add a variable to the current scope.
    fn add_var_to_scope(&mut self, var: Rc<MutVar>) {
        self.add_to_scope(var.name().to_string(), ScopedItem::Var(var.clone()))
    }

    /// Iterate to next [Directive]. Return `None` if there are no more directives.
    fn next_directive(&mut self) -> Option<Directive<MutVar>> {
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

    /// Parse a [Directive].
    fn parse_directive(&mut self) -> CompilationResult<Directive<MutVar>> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Word(word) => match word.as_str() {
                "block" => {
                    let block = self.parse_block()?;
                    Ok(Directive::Block(block))
                }
                "dyn" => {
                    self.consume(); // Consume "dyn" word
                    if self.consume_word()? != "block" {
                        return Err(CompilationError::new_localized(
                            "`dyn` keyword can only be followed by `block`.",
                            start_token.span.clone(),
                        ));
                    }

                    let dyn_block = self.parse_dyn_block()?;

                    Ok(Directive::DynBlock(dyn_block))
                }
                "import" => {
                    let import = self.parse_import()?;
                    Ok(Directive::Import(import))
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
    fn parse_statement(&mut self) -> Option<CompilationResult<Statement<MutVar>>> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Word(word) => {
                let kind = match word.as_str() {
                    "when" => self.parse_when_statement(),
                    "let" => self.parse_declaration_statement(),
                    _ if self.is_at_definition_statment() => self.parse_definition_statement(),
                    _ => {
                        self.consume_bad_statement();
                        Err(CompilationError::new_localized(
                            "Invalid statement.",
                            TextSpan::from_spans(&start_token.span, &self.current().span),
                        ))
                    }
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
            TokenKind::LeftParen => match self.parse_unpack_statement() {
                Ok(s) => Some(Ok(s)),
                Err(e) => Some(Err(e)),
            },
            TokenKind::End => None,
            TokenKind::RightCurly => None,
            _ => {
                self.consume_bad_statement();
                Some(Err(CompilationError::new_localized(
                    "Statements have to begin with a `word`.",
                    start_token.span,
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

    /// Returns true if the cursor is at the begining of an assignment statement.
    fn is_at_definition_statment(&self) -> bool {
        matches!(
            &self.peak(1).kind,
            TokenKind::LeftArrow
                | TokenKind::LeftCurlyArrow
                | TokenKind::LeftDoubleArrow
                | TokenKind::LeftCurlyDoubleArrow
                | TokenKind::Equals
        )
    }

    fn parse_declaration_statement(&mut self) -> CompilationResult<StatementKind<MutVar>> {
        let variable = self.parse_variable_declaration()?;

        match &variable.type_ {
            VariableType::Var(_) => {
                self.consume_and_expect(TokenKind::Semicolon)?;

                // `var` type variable is always zero initialized, because memory cells in factorio
                // cannot be assigned values.
                let var = Rc::new(RefCell::new(variable));
                self.add_var_to_scope(var.clone());
                return Ok(StatementKind::Declaration(Declaration::new(var)));
            }
            VariableType::Bool(_) => {}
            VariableType::Int(_) => {}
            VariableType::Many => {}
            VariableType::_Tuple(_) => {}
            VariableType::Inferred => {}
        };

        let variable = Rc::new(RefCell::new(variable));
        self.add_var_to_scope(variable.clone());

        let current = self.current();

        // Return early if it is just a declaration
        if current.kind == TokenKind::Semicolon {
            self.consume();
            return Ok(StatementKind::Declaration(Declaration { variable }));
        }

        let kind = self.parse_assignment_operator()?;
        let expr = self.parse_expression()?;

        self.consume_and_expect(TokenKind::Semicolon)?;
        Ok(StatementKind::DeclarationDefinition(
            DeclarationDefinition::new(variable, expr, kind),
        ))
    }

    /// Parse assignment operator and determine the kind of definition.
    fn parse_assignment_operator(&mut self) -> CompilationResult<DefinitionKind> {
        let start_span = self.current().span.clone();

        let kind = match &self.consume().kind {
            TokenKind::LeftArrow => Ok(DefinitionKind::Wire(WireKind::Red)),
            TokenKind::LeftCurlyArrow => Ok(DefinitionKind::Wire(WireKind::Green)),
            TokenKind::LeftDoubleArrow => Ok(DefinitionKind::Convert(WireKind::Red)),
            TokenKind::LeftCurlyDoubleArrow => Ok(DefinitionKind::Convert(WireKind::Green)),
            TokenKind::Equals => Ok(DefinitionKind::Equal),

            other => Err(CompilationError::new_localized(
                format!("'{}' is not a valid assignment operator.", other),
                start_span.clone(),
            )),
        }?;

        Ok(kind)
    }

    /// Parse variable assignment statement.
    fn parse_definition_statement(&mut self) -> CompilationResult<StatementKind<MutVar>> {
        let start_span = self.current().span.clone();

        let name = self.consume_word()?.to_string();

        let var = match self.search_scope(&name) {
            Some(ScopedItem::Var(v)) => Ok(v),
            Some(_) => Err(CompilationError::new_localized(
                "Only variables can be assigned to.".to_string(),
                start_span.clone(),
            )),
            None => Err(CompilationError::new_localized(
                format!("Variable `{}` not defined.", name),
                start_span.clone(),
            )),
        }?;

        let kind = self.parse_assignment_operator()?;
        let expr = self.parse_expression()?;

        self.consume_and_expect(TokenKind::Semicolon)?;

        Ok(StatementKind::Definition(Definition::new(var, expr, kind)))
    }

    /// Parse signal part of variable type
    ///
    /// Example:
    /// ```text
    /// i: int(signal) = ...
    ///       ^^^^^^^^ - this part
    /// ```
    fn parse_variable_type_signal(&mut self) -> CompilationResult<VariableSignalType> {
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

    /// Parse variable declaration.
    fn parse_variable_declaration(&mut self) -> CompilationResult<VarData> {
        let start_token = self.current().clone();

        self.consume_and_expect_word("let")?;

        let name = self.consume_word()?.to_string();

        let type_ = match &self.current().kind {
            TokenKind::Colon => {
                self.consume();
                self.parse_variable_type()?
            }
            _ => VariableType::Inferred,
        };

        Ok(VarData::new(
            name.to_owned(),
            type_,
            self.get_span_from(&start_token.span),
            self.get_next_var_id(),
        ))
    }

    /// Parse variable.
    fn parse_variable(&mut self) -> CompilationResult<ParsedVariable<MutVar>> {
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

            Ok(ParsedVariable::Dec(VarData::new(
                name.to_owned(),
                type_,
                self.get_span_from(&start_token.span),
                self.get_next_var_id(),
            )))
        }
    }

    /// Parse variable type.
    fn parse_variable_type(&mut self) -> CompilationResult<VariableType> {
        let token = self.consume();
        match token.kind.clone() {
            TokenKind::Word(start_word) => match start_word.as_str() {
                "bool" => Ok(VariableType::Bool(self.parse_variable_type_signal()?)),
                "int" => Ok(VariableType::Int(self.parse_variable_type_signal()?)),
                "var" => Ok(VariableType::Var(self.parse_variable_type_signal()?)),
                "many" => Ok(VariableType::Many),
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

    /// Consume literal block link argument to be passed directly into python later.
    fn parse_literal_block_link_argument(&mut self) -> CompilationResult<TextSpan> {
        let start_token = self.current().clone();
        loop {
            let current = self.current();

            match &current.kind {
                TokenKind::LeftCurly => {
                    self.consume_until(&TokenKind::RightCurly);
                }
                TokenKind::LeftSquare => {
                    self.consume_until(&TokenKind::RightSquare);
                }
                TokenKind::Comma | TokenKind::RightParen => {
                    break;
                }
                TokenKind::End => {
                    return Err(CompilationError::new_localized(
                        "Runaway literal.",
                        TextSpan::from_spans(&start_token.span, &self.peak(-1).span),
                    ))
                }
                _ => {
                    self.consume();
                }
            }
        }

        let span = TextSpan::from_spans(&start_token.span, &self.peak(-1).span);

        Ok(span)
    }

    /// Parse arguments for `block` definition.
    fn parse_block_arguments(&mut self) -> CompilationResult<Vec<Rc<MutVar>>> {
        let mut arguments: Vec<Rc<MutVar>> = vec![];

        while !matches!(self.current().kind, TokenKind::RightParen | TokenKind::End) {
            let var_span = self.current().span.clone();

            let arg = match self.parse_variable()? {
                ParsedVariable::Dec(v) => Ok(Rc::new(RefCell::new(v))),

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

            self.consume_if(TokenKind::Comma);

            self.add_var_to_scope(arg.clone());

            arguments.push(arg);
        }
        Ok(arguments)
    }

    /// Parse arguments for `dyn block` definition.
    fn parse_dyn_block_arguments(&mut self) -> CompilationResult<Vec<DynBlockArg<MutVar>>> {
        let mut arguments: Vec<DynBlockArg<MutVar>> = vec![];
        while !matches!(self.current().kind, TokenKind::RightParen | TokenKind::End) {
            let var_span = self.current().span.clone();

            let checkpoint = self.cursor;

            let block_arg = match self.parse_variable() {
                Ok(ParsedVariable::Dec(v)) => Ok(DynBlockArg::Var(Rc::new(RefCell::new(v)))),

                // TODO: this will never happen as parse_variable_type til return an error on undefined variables.
                Ok(ParsedVariable::Ref(_)) => Err(CompilationError::new_localized(
                    "Please give variable a type.".to_string(),
                    var_span,
                )),

                Ok(ParsedVariable::Const(_)) => Err(CompilationError::new_localized(
                    "Constants cannot be block inputs.".to_string(),
                    var_span,
                )),

                // Parse literal
                Err(_) => {
                    self.cursor = checkpoint;
                    let name = self.consume_word()?.to_string();
                    self.consume_and_expect(TokenKind::Colon)?;
                    let type_ = self.consume_word()?.to_string();

                    if &type_ != "lit" {
                        return Err(CompilationError::new_localized(
                            format!("Invalid type '{}'.", type_),
                            TextSpan::from_spans(&var_span, &self.peak(-1).span),
                        ));
                    }
                    Ok(DynBlockArg::Literal(name))
                }
            }?;

            self.consume_if(TokenKind::Comma);

            if let DynBlockArg::Var(v) = &block_arg {
                self.add_var_to_scope(v.clone());
            }

            arguments.push(block_arg);
        }
        Ok(arguments)
    }

    /// Parse arguments for a chef block link definition.
    fn parse_block_link_arguments(&mut self) -> CompilationResult<Vec<Expression<MutVar>>> {
        let args = self.parse_dyn_block_link_arguments()?;
        let mut inputs = vec![];
        for arg in args {
            match arg {
                BlockLinkArg::Expr(e) => inputs.push(e),
                BlockLinkArg::Literal(span) => {
                    return Err(CompilationError::new_localized(
                        "Literal arguments are only allowed in dynamic block links.".to_string(),
                        span,
                    ))
                }
            }
        }
        Ok(inputs)
    }

    /// Parse arguments for a dynamic block link definition.
    fn parse_dyn_block_link_arguments(&mut self) -> CompilationResult<Vec<BlockLinkArg<MutVar>>> {
        let mut inputs: Vec<BlockLinkArg<MutVar>> = vec![];
        self.consume_and_expect(TokenKind::LeftParen)?;

        if self.current().kind == TokenKind::RightParen {
            self.consume();
            return Ok(vec![]);
        }

        loop {
            match self.parse_expression() {
                Ok(expr) => inputs.push(BlockLinkArg::Expr(expr)),
                Err(e) => match self.parse_literal_block_link_argument() {
                    Ok(lit) => inputs.push(BlockLinkArg::Literal(lit)),
                    Err(_) => return Err(e),
                },
            }

            self.consume_if(TokenKind::Comma);

            if matches!(self.current().kind, TokenKind::RightParen | TokenKind::End) {
                break;
            }
        }
        self.consume_and_expect(TokenKind::RightParen)?;
        Ok(inputs)
    }

    fn parse_statement_list(&mut self) -> CompilationResult<Vec<Statement<MutVar>>> {
        self.consume_and_expect(TokenKind::LeftCurly)?;

        let mut statements: Vec<Statement<MutVar>> = vec![];

        while let Some(statement) = self.parse_statement() {
            statements.push(statement?)
        }

        self.consume_and_expect(TokenKind::RightCurly)?;

        Ok(statements)
    }

    fn get_module(&self, module: &String) -> CompilationResult<&Module> {
        self.modules
            .get(module)
            .ok_or(CompilationError::new_localized(
                format!("Module `{}` not found.", module),
                self.peak(-1).span.clone(),
            ))
    }

    /// Parse chef `import`. This will parse the imported file and add its items to the scope.
    fn parse_import(&mut self) -> CompilationResult<Import<MutVar>> {
        self.consume_word()?; // Consume "import" word

        let options = self.options.clone();
        let mut namespace = None;

        // Get path directly or from module alias
        let text = match self.consume().kind.clone() {
            TokenKind::StringLiteral(s) => {
                let text = SourceText::from_file(s.as_str(), options.clone())?;
                Ok(text)
            }
            TokenKind::Word(module) => {
                namespace = Some(module.clone());
                match self.get_module(&module)? {
                    Module::Static(f) => {
                        let path = f.path.to_string();
                        let contents = f.contents.to_string();
                        let text = SourceText::new(path, contents);
                        Ok(text)
                    }
                    Module::Dynamic(path) => {
                        let text = SourceText::from_file(path.as_str(), options.clone())?;
                        Ok(text)
                    }
                }
            }
            other => Err(CompilationError::new_localized(
                format!("Expected path or module. Found: {}.", other),
                self.peak(-1).span.clone(),
            )),
        }?;

        // TODO: Make file mandetory in SourceText
        let path = text.file().unwrap().to_string();

        let text = Rc::new(text);
        let file_path = PathBuf::from(path);

        let diagnostics_bag = DiagnosticsBag::new_ref(options.clone());
        let tokens = Lexer::from_source(text).collect();
        let imported_ast = Parser::parse(tokens, diagnostics_bag, options.clone());
        let ast = imported_ast;

        if self.consume_if(TokenKind::word("as")) {
            namespace = Some(self.consume_word()?.to_string());
        }
        Ok(Import {
            namespace,
            file_path,
            ast,
        })
    }

    /// Parse constant directive.
    fn parse_constant(&mut self) -> CompilationResult<Directive<MutVar>> {
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
    fn parse_block(&mut self) -> CompilationResult<Block<MutVar>> {
        let start_token = self.consume().clone(); // Consume "block" word
        debug_assert_eq!(start_token.kind, TokenKind::Word("block".to_string()));

        self.enter_scope();

        let (block_name, inputs, outputs) = self.parse_block_signature()?;

        let statements = self.parse_statement_list()?;

        Ok(Block::new(
            self.get_next_block_id(),
            None,
            block_name,
            inputs,
            outputs,
            statements,
            TextSpan {
                start: start_token.span.start,
                end: self.peak(-1).span.end,
                text: start_token.span.text,
            },
        ))
    }

    /// Parse chef `dyn block`.
    fn parse_dyn_block(&mut self) -> CompilationResult<DynBlock<MutVar>> {
        let start_token = self.current().clone();

        self.enter_scope();

        let (block_name, inputs) = self.parse_dyn_block_signature()?;

        let dyn_block = match self.current().kind.clone() {
            // Use external script
            TokenKind::LessThan => {
                // Parse `<path/to/script.py>`
                let start = self.consume().span.end; // Consume "<" token
                let end_span = self.consume_until(&TokenKind::LargerThan).span.clone();
                let span = TextSpan::new(start, end_span.start, end_span.text);

                let path = PathBuf::from(span.text());

                DynBlock::new(
                    block_name,
                    inputs,
                    path,
                    TextSpan::from_spans(&start_token.span, &self.peak(-1).span),
                    self.options.clone(),
                )
            }

            TokenKind::LeftCurly => {
                // We use the end of '{' token as start of the source, to include the beginning white space
                let code_start = self.consume_and_expect(TokenKind::LeftCurly)?.span.end;

                // Consume code tokens
                while !matches!(self.current().kind, TokenKind::RightCurly | TokenKind::End) {
                    self.consume();
                }

                let code_end = self.peak(-1).span.end;

                // consume '}'
                self.consume();

                let tmp_dir = std::env::temp_dir().join("chef");
                let _ = fs::create_dir(&tmp_dir);

                let python_file_path = tmp_dir.join(format!("{}.py", block_name));

                let source = start_token.span.text.text();
                let code_text = &source[code_start..code_end];

                // get amount of leading white space in the first (non empty) line
                let leading_whitespace = code_text
                    .lines()
                    .find(|line| !line.is_empty())
                    .map(|line| line.chars().take_while(|c| c.is_whitespace()).count())
                    .unwrap_or(0);

                let mut code = String::new();
                for (n, line) in code_text.lines().enumerate() {
                    if line.chars().all(|c| c.is_whitespace()) {
                        code.push('\n');
                    } else if line
                        .chars()
                        .take(leading_whitespace)
                        .all(|c| c.is_whitespace())
                    {
                        code = code + &line[leading_whitespace..] + "\n";
                    } else {
                        return Err(CompilationError::new_localized(
                            format!("Not enough whitespace in front of line {}.", n),
                            TextSpan {
                                start: code_start,
                                end: code_end,
                                text: self.current().span.text.clone(),
                            },
                        ));
                    }
                }

                let block_span = TextSpan::from_spans(&start_token.span, &self.peak(-1).span);

                fs::write(&python_file_path, code).map_err(|e| {
                    CompilationError::new_localized(
                        format!("Could not write to file: {}", e),
                        block_span.clone(),
                    )
                })?;

                DynBlock::new(
                    block_name,
                    inputs,
                    python_file_path,
                    TextSpan::from_spans(&start_token.span, &self.peak(-1).span),
                    self.options.clone(),
                )
            }
            TokenKind::RightFatArrow => {
                return Err(CompilationError::new_localized(
                    "Outputs of dynamic blocks should be declared by the genated code.",
                    self.current().span.clone(),
                ))
            }

            other => {
                return Err(CompilationError::new_localized(
                    format!(
                        "Expected '{{' or '<' after dynamic block name, found: {}",
                        other
                    ),
                    self.current().span.clone(),
                ))
            }
        };

        self.exit_scope();

        Ok(dyn_block)
    }

    fn parse_block_signature(&mut self) -> CompilationResult<(String, BlockArgs, BlockArgs)> {
        let block_name: String = if let TokenKind::Word(s) = &self.consume().kind {
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

        // Blocks can be defined without outputs
        let outputs = match self.current().kind {
            TokenKind::RightFatArrow => {
                self.consume();

                self.consume_and_expect(TokenKind::LeftParen)?;
                let outputs = self.parse_block_arguments()?;
                self.consume_and_expect(TokenKind::RightParen)?;

                outputs
            }
            TokenKind::LeftCurly => vec![],
            _ => {
                return Err(CompilationError::new_localized(
                    "Expected '=>' or '{' after block name.".to_string(),
                    self.current().span.clone(),
                ))
            }
        };

        Ok((block_name, inputs, outputs))
    }

    /// Parse `dyn block` signature (Types of arguments).
    fn parse_dyn_block_signature(
        &mut self,
    ) -> CompilationResult<(String, Vec<DynBlockArg<MutVar>>)> {
        let block_name: String = if let TokenKind::Word(s) = &self.consume().kind {
            s.clone()
        } else {
            self.diagnostics_bag
                .borrow_mut()
                .report_error(&self.peak(-1).span, "No name for `dyn block` was given.");
            "".to_string()
        };

        self.consume_and_expect(TokenKind::LeftParen)?;
        let inputs = self.parse_dyn_block_arguments()?;
        self.consume_and_expect(TokenKind::RightParen)?;

        Ok((block_name, inputs))
    }

    /// Parse chef expression.
    fn parse_expression(&mut self) -> CompilationResult<Expression<MutVar>> {
        self.parse_binary_expression(None, 0)
    }

    /// Parse chef `when` statement.
    fn parse_when_statement(&mut self) -> CompilationResult<StatementKind<MutVar>> {
        let _start_token = self.consume().clone(); // Consume "when" token
        let condition = self.parse_expression()?;

        self.enter_scope();

        let statements = self.parse_statement_list()?;

        let kind = StatementKind::When(WhenStatement {
            condition,
            statements,
        });

        Ok(kind)
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
            TokenKind::EveryDoubleEquals => Some(BinaryOperator::EveryEquals),
            TokenKind::EveryLargerThan => Some(BinaryOperator::EveryLargerThan),
            TokenKind::EveryLargerThanEquals => Some(BinaryOperator::EveryLargerThanEquals),
            TokenKind::EveryLessThan => Some(BinaryOperator::EveryLessThan),
            TokenKind::EveryLessThanEquals => Some(BinaryOperator::EveryLessThanEquals),
            TokenKind::EveryBangEquals => Some(BinaryOperator::EveryNotEquals),
            TokenKind::AnyDoubleEquals => Some(BinaryOperator::AnyEquals),
            TokenKind::AnyLargerThan => Some(BinaryOperator::AnyLargerThan),
            TokenKind::AnyLargerThanEquals => Some(BinaryOperator::AnyLargerThanEquals),
            TokenKind::AnyLessThan => Some(BinaryOperator::AnyLessThan),
            TokenKind::AnyLessThanEquals => Some(BinaryOperator::AnyLessThanEquals),
            TokenKind::AnyBangEquals => Some(BinaryOperator::AnyNotEquals),
            _ => None,
        }
    }

    /// Parse a binary expression.
    /// https://en.wikipedia.org/wiki/Operator-precedence_parser
    fn parse_binary_expression(
        &mut self,
        left: Option<Expression<MutVar>>,
        min_precedence: u8,
    ) -> CompilationResult<Expression<MutVar>> {
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

            let return_type = op
                .return_type(left.return_type(), right.return_type())
                .map_err(|_| {
                    CompilationError::new_localized(
                        format!(
                            "Invalid operation between `{}` and `{}`.",
                            left.return_type(),
                            right.return_type()
                        ),
                        TextSpan::from_spans(&left.span, &right.span),
                    )
                })?;

            left = Expression::new(
                ExpressionKind::Binary(BinaryExpression {
                    left: Box::new(left.clone()),
                    right: Box::new(right.clone()),
                    operator: op.clone(),
                    return_type,
                    span: TextSpan::from_spans(&left.span, &right.span),
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
    fn parse_primary_expression(&mut self) -> CompilationResult<Expression<MutVar>> {
        let start_token = self.current().clone();
        match &start_token.kind {
            TokenKind::Minus => {
                self.consume();
                let inner = self.parse_primary_expression()?;
                let inner_span = inner.span.clone();
                Ok(Expression::new(
                    ExpressionKind::Negative(NegativeExpression::new(inner)),
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

            // If imported block
            TokenKind::Word(_word) if self.peak(1).kind == TokenKind::DoubleColon => {
                let import_name = self.consume_word()?.to_string();
                self.consume_and_expect(TokenKind::DoubleColon)?;

                let item_name = match &self.current().kind {
                    TokenKind::Word(w) => w.clone(),
                    _ => {
                        return Err(CompilationError::new_localized(
                            "Expected word after `::`.".to_string(),
                            self.current().span.clone(),
                        ))
                    }
                };

                let import = self.ast.get_import_by_name(&import_name).ok_or({
                    CompilationError::new_localized(
                        format!("Import `{}` not found.", import_name),
                        start_token.span.clone(),
                    )
                })?;
                let item = import.ast.get_directive_by_name(&item_name).ok_or(
                    CompilationError::new_localized(
                        format!(
                            "Item `{}` not found in import `{}`.",
                            item_name, import_name
                        ),
                        TextSpan::from_spans(&start_token.span, &self.peak(1).span),
                    ),
                )?;
                let mut link = match item {
                    Directive::Block(_) => self.parse_block_link()?,
                    Directive::DynBlock(_) => self.parse_dyn_block_link()?,
                    other => todo!("Link to imported {:?}", other),
                };
                link.namespace = Some(import_name);
                Ok(Expression {
                    kind: ExpressionKind::BlockLink(link),
                    span: self.get_span_from(&start_token.span),
                })
            }

            // If it is a block
            TokenKind::Word(word) if self.peak(1).kind == TokenKind::LeftParen => {
                // Normal block
                let link = if self.ast.get_block_by_name(word, None).is_some() {
                    self.parse_block_link()?
                }
                // Dynamic block
                else {
                    self.parse_dyn_block_link()?
                };
                Ok({
                    let kind = ExpressionKind::BlockLink(link);
                    let span = self.get_span_from(&start_token.span);
                    Expression { kind, span }
                })
            }
            TokenKind::Word(word) => {
                // If is defined variable
                if let Some(item) = self.search_scope(word) {
                    let item_span = self.consume().span.clone();
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
                } else {
                    Err(CompilationError::new_localized(
                        format!("`{}` not defined.", word),
                        start_token.span,
                    ))
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
            TokenKind::Period => {
                let mut delay = 0;
                while self.current().kind == TokenKind::Period {
                    self.consume();
                    delay += 1;
                }

                let expr = self.parse_expression()?;

                let end = expr.span.end;

                Ok(Expression::new(
                    ExpressionKind::Delay(super::DelayExpression::new(Box::new(expr), delay)),
                    TextSpan::new(start_token.span.start, end, start_token.span.text.clone()),
                ))
            }
            TokenKind::Bar => {
                self.consume(); // Consume "|"
                let expr = self.parse_expression()?;
                self.consume(); // Consume "|"

                Ok(Expression::new(
                    ExpressionKind::SizeOf(SizeOfExpression::new(Box::new(expr))),
                    TextSpan::from_spans(&start_token.span, &self.peak(-1).span),
                ))
            }
            TokenKind::QuestionMark => {
                self.consume(); // Consume "?"

                let left = self.parse_primary_expression()?;
                let op = match self.get_binary_operator() {
                    Some(bin_op) => {
                        if let OperatorKind::Decider(dc) = bin_op.operator() {
                            self.consume(); // Consume operator
                            Ok(dc)
                        } else {
                            Err(CompilationError::new_localized(
                                format!("Expected decider operator, found '{}'.", bin_op),
                                self.current().span.clone(),
                            ))
                        }
                    }
                    None => Err(CompilationError::new_localized(
                        format!(
                            "Expected binary operator for gate expression, found '{}'.",
                            self.current().kind
                        ),
                        self.current().span.clone(),
                    )),
                }?;
                let right = self.parse_primary_expression()?;

                self.consume_and_expect(TokenKind::LeftArrow)?;
                let gated = self.parse_expression()?;

                Ok(Expression::new(
                    ExpressionKind::Gate(GateExpression::new(left, right, op, gated)),
                    TextSpan::from_spans(&start_token.span, &self.peak(-1).span),
                ))
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

    /// Parse a block link expression.
    fn parse_variable_index(&mut self, var: Rc<MutVar>) -> CompilationResult<Expression<MutVar>> {
        let start_span = self.peak(-1).span.clone();
        let var_span = self.current().span.clone();
        self.consume_and_expect(TokenKind::LeftSquare)?;
        let index_token = self.consume();

        let expr_kind = match &index_token.kind {
            TokenKind::Word(signal) => ExpressionKind::Pick(PickExpression::new(
                signal.to_string(),
                VariableRef::new(var, var_span),
                self.get_span_from(&start_span),
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

    /// Parse a dynamic block link expression.
    fn parse_dyn_block_link(&mut self) -> CompilationResult<BlockLinkExpression<MutVar>> {
        let start_span = self.peak(-1).span.clone();
        let name = self.consume_word()?.to_string();

        // Get arguments
        let inputs = self.parse_dyn_block_link_arguments()?;

        let link_span = TextSpan::from_spans(&start_span, &self.peak(-1).span);

        let dyn_block =
            self.ast
                .get_dyn_block_mut(&name)
                .ok_or(CompilationError::new_localized(
                    format!("Block `{}` not defined.", &name),
                    start_span.clone(),
                ))?;

        // Generate dynamic block and store get this version number of the block
        let version = dyn_block.generate_version(
            &inputs,
            link_span.clone(),
            self.options.clone(),
            self.diagnostics_bag.clone(),
        )?;

        let input_exprs = inputs
            .iter()
            .filter_map(|arg| match arg {
                BlockLinkArg::Expr(e) => Some(e.clone()),
                BlockLinkArg::Literal(_) => None,
            })
            .collect::<Vec<_>>();

        Ok(BlockLinkExpression::new(
            dyn_block.name.clone(),
            input_exprs,
            Some(version),
            link_span,
        ))
    }

    /// Parse a chef block link.
    fn parse_block_link(&mut self) -> CompilationResult<BlockLinkExpression<MutVar>> {
        let start = self.current().span.clone();
        let name = self.consume_word()?.to_string();
        let inputs = self.parse_block_link_arguments()?;

        Ok(BlockLinkExpression::new(
            name,
            inputs,
            None,
            TextSpan::from_spans(&start, &self.current().span),
        ))
    }

    /// Parse tuple unpacking statement.
    fn parse_unpack_statement(&mut self) -> CompilationResult<Statement<MutVar>> {
        let start_token = self.consume_and_expect(TokenKind::LeftParen)?;
        let mut vars = vec![];

        while self.current().kind != TokenKind::RightParen {
            match self.parse_variable()? {
                ParsedVariable::Dec(var) => {
                    let var = Rc::new(RefCell::new(var));
                    self.add_var_to_scope(var.clone());
                    vars.push((var, AssignmentType::Declaration));
                }
                ParsedVariable::Ref(var_ref) => {
                    vars.push((var_ref.var.clone(), AssignmentType::Definition))
                }
                ParsedVariable::Const(_c) => todo!(),
            }
            self.consume_if(TokenKind::Comma);
        }

        self.consume();

        let def_kind = self.parse_assignment_operator()?;

        let link = self.parse_block_link()?;

        self.consume_and_expect(TokenKind::Semicolon)?;

        let block_name = &link.name;
        let block = self
            .ast
            .get_block_by_name(block_name, link.dyn_block_version)
            .ok_or_else(|| {
                CompilationError::new_localized(
                    format!("Block '{}' not defined.", block_name),
                    self.peak(-1).span.clone(),
                )
            })?;

        let block_outputs = block.outputs.clone();

        if block_outputs.len() != vars.len() {
            return Err(CompilationError::new_localized(
                format!(
                    "Cannot assign tuple with {} elements to block '{}', which has {} outputs.",
                    vars.len(),
                    block_name,
                    block_outputs.len()
                ),
                TextSpan::from_spans(&start_token.span, &self.peak(-1).span),
            ));
        }

        let mut defs = vec![];

        for i in 0..vars.len() {
            let (var, assignment_type) = vars[i].clone();
            let block_output = &block_outputs[i];

            let def = OutputAssignment {
                variable: var,
                block_variable: block_output.clone(),
                assignment_type,
            };

            defs.push(def);
        }

        let kind = StatementKind::TupleDeclarationDefinition(TupleDeclarationDefinition {
            defs,
            block_link: link,
            def_kind,
        });

        Ok(Statement::new(
            kind,
            TextSpan::from_spans(&start_token.span, &self.peak(-1).span),
        ))
    }

    fn get_span_from(&self, start_span: &TextSpan) -> TextSpan {
        TextSpan::from_spans(start_span, &self.peak(-1).span)
    }
}

enum ParsedVariable<V> {
    /// Declaration of a variable.
    Dec(VarData),

    /// A reference to a variable in an expression.
    Ref(VariableRef<V>),

    /// A constant.
    Const(Constant),
}
