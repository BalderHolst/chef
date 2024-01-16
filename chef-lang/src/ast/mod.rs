//! Module for lexing and parsing chef source code into an abstract syntax tree and checking for errors.

use std::fmt::Display;
use std::rc::Rc;

use crate::ast::visitors::Visitor;
use crate::diagnostics::DiagnosticsBagRef;
use crate::text::{SourceText, TextSpan};
use crate::Opts;

use self::lexer::{Lexer, Token};
use self::parser::Parser;

mod constant_evaluator;
pub mod lexer;
pub mod parser;
mod type_checker;
mod visitors;

/// [AST] representation of chef `block`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub name: String,
    pub inputs: Vec<Rc<Variable>>,
    pub output_type: VariableType,
    pub statements: Vec<Statement>,
    pub output: Expression,
    pub span: TextSpan,
}

impl Block {
    /// Instantiate a new [Block].
    fn new(
        name: String,
        inputs: Vec<Rc<Variable>>,
        output_type: VariableType,
        statements: Vec<Statement>,
        output: Expression,
        span: TextSpan,
    ) -> Self {
        Self {
            name,
            inputs,
            output_type,
            statements,
            output,
            span,
        }
    }
}

/// The abstract syntax tree.
#[allow(clippy::upper_case_acronyms)]
pub struct AST {
    pub blocks: Vec<Block>,
    diagnostics_bag: DiagnosticsBagRef,
}

impl AST {
    /// Instantiate a new [AST].
    pub fn new(diagnostics_bag: DiagnosticsBagRef) -> Self {
        Self {
            blocks: vec![],
            diagnostics_bag,
        }
    }

    /// Build an [AST] from a [SourceText] instance. This also evaluates constants and does type
    /// checking.
    pub fn from_source(
        text: Rc<SourceText>,
        diagnostics_bag: DiagnosticsBagRef,
        opts: Rc<Opts>,
    ) -> Self {
        let lexer = Lexer::from_source(diagnostics_bag.clone(), text);
        let tokens: Vec<Token> = lexer.collect();
        let parser = Parser::new(tokens, diagnostics_bag.clone(), opts);
        let mut ast = AST::new(diagnostics_bag);
        for block in parser {
            ast.add_block(block);
        }

        // Check types first to make sure that bool constants are evaluated as int constants and
        // vise versa.
        ast.check_types();
        ast.evaluate_constants();
        ast
    }

    /// Add a statement to the [AST].
    pub fn add_block(&mut self, block: Block) {
        self.blocks.push(block);
    }

    /// Print the [AST] to stout.
    pub fn print(&self) {
        let mut printer = Printer::new();
        for block in &self.blocks {
            printer.visit_block(block);
        }
    }

    /// Evaluate constant expressions in the [AST] to simplify it.
    pub fn evaluate_constants(&mut self) {
        constant_evaluator::evaluate_constants(self)
    }

    /// Check that types are valid.
    pub fn check_types(&self) {
        type_checker::check(self, self.diagnostics_bag.clone());
    }
}

// TODO: The code example is not run correctly
/// A parsed statement.
///
/// Statements in chef are separated by semicolon:
/// ```
/// # let code = "
/// block main(input: all) -> (int(inserter)) {
///     a: int = input[pipe] * 5;          // <-- Statement 0
///     b: int = input[inserter] / 3 + 8;  // <-- Statement 1
///     out a + b;                         // <-- Statement 2
/// }
/// # "
/// # let diagnostics_bag: DiagnosticsBagRef = Rc::new(RefCell::new(DiagnosticsBag::new(opts.clone(), text.clone())));
/// # let opts = Rc::new(Opts::parse_args_default_or_exit());
/// # let ast = AST::from_source(text, diagnostics_bag.clone(), opts.clone());
/// # assert_eq!(diagnostics_bag.borrow().diagnostics.len(), 0);
/// # compiler::compile(ast, diagnostics_bag.clone(), opts.clone())
/// # assert_eq!(diagnostics_bag.borrow().diagnostics.len(), 0);
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: TextSpan,
}

impl Statement {
    /// Instantiate a new [Statement].
    fn new(kind: StatementKind, span: TextSpan) -> Self {
        Statement { kind, span }
    }
}

/// Kinds of statement.
#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Expression),
    Assignment(Assignment),
    Mutation(Mutation),
    Out(Expression),
    Error,
}

/// Chef variable types.
#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    Bool(VariableSignalType),
    Int(VariableSignalType),
    Var(VariableSignalType),
    Counter((VariableSignalType, Box<Expression>)),
    ConstInt(i32),
    ConstBool(bool),
    All,
}

impl VariableType {
    pub fn return_type(&self) -> ExpressionReturnType {
        match self {
            VariableType::Bool(_) => ExpressionReturnType::Bool,
            VariableType::Int(_) => ExpressionReturnType::Int,
            VariableType::Var(_) => ExpressionReturnType::Int,
            VariableType::Counter(_) => ExpressionReturnType::Int,
            VariableType::ConstInt(_) => ExpressionReturnType::Int,
            VariableType::ConstBool(_) => ExpressionReturnType::Bool,
            VariableType::All => ExpressionReturnType::Group,
        }
    }

    pub fn signal_type(&self) -> Option<&VariableSignalType> {
        match self {
            VariableType::Bool(s) => Some(s),
            VariableType::Int(s) => Some(s),
            VariableType::Var(s) => Some(s),
            VariableType::Counter((s, _lim)) => Some(s),
            VariableType::ConstInt(_) => None,
            VariableType::ConstBool(_) => None,
            VariableType::All => None,
        }
    }

    pub fn signal(&self) -> Option<String> {
        match self.signal_type()? {
            VariableSignalType::Signal(s) => Some(s.to_owned()),
            VariableSignalType::Any => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableSignalType {
    Signal(String),
    Any,
}

/// A chef variable.
#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub type_: VariableType,
    pub span: TextSpan,
}

impl Variable {
    /// Instantiate a new [Variable].
    pub fn new(name: String, variable_type: VariableType, span: TextSpan) -> Self {
        Self {
            name,
            type_: variable_type,
            span,
        }
    }

    /// Type returned
    pub fn return_type(&self) -> ExpressionReturnType {
        self.type_.return_type()
    }
}

/// A reference to a defined chef variable
#[derive(Debug, Clone, PartialEq)]
pub struct VariableRef {
    pub var: Rc<Variable>,
    pub span: TextSpan,
}

impl VariableRef {
    fn new(var: Rc<Variable>, span: TextSpan) -> Self {
        Self { var, span }
    }

    pub fn return_type(&self) -> ExpressionReturnType {
        self.var.return_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentKind {
    Sig,
    Var,
    Counter,
}

/// [AST] representation of chef `int` variable assignment.
#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub variable: Rc<Variable>,
    pub expression: Expression,
    pub kind: AssignmentKind,
}

impl Assignment {
    /// Instantiate a new [Assignment].
    pub fn new(variable: Rc<Variable>, expression: Expression, kind: AssignmentKind) -> Self {
        Self {
            variable,
            expression,
            kind,
        }
    }
}

/// [AST] representation of mutaton of a chef `var` type variable.
#[derive(Debug, Clone, PartialEq)]
pub struct Mutation {
    pub var_ref: VariableRef,
    pub operator: MutationOperator,
    pub expression: Expression,
}

impl Mutation {
    /// Instantiate a new [Mutation].
    pub fn new(variable: VariableRef, operator: MutationOperator, expression: Expression) -> Self {
        Self {
            var_ref: variable,
            expression,
            operator,
        }
    }
}

/// A chef expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: TextSpan,
}

impl Expression {
    fn new(kind: ExpressionKind, span: TextSpan) -> Self {
        Self { kind, span }
    }

    fn return_type(&self) -> ExpressionReturnType {
        match &self.kind {
            ExpressionKind::Bool(_) => ExpressionReturnType::Bool,
            ExpressionKind::Int(_) => ExpressionReturnType::Int,
            ExpressionKind::Binary(e) => e.return_type().clone(),
            ExpressionKind::Parenthesized(e) => e.return_type(),
            ExpressionKind::Pick(_) => ExpressionReturnType::Int,
            ExpressionKind::VariableRef(var_ref) => var_ref.return_type(),
            ExpressionKind::BlockLink(e) => e.return_type(),
            ExpressionKind::When(e) => e.return_type(),
            ExpressionKind::Error => ExpressionReturnType::Int,
        }
    }

    fn number(n: i32, span: TextSpan) -> Self {
        Self {
            kind: ExpressionKind::Int(n),
            span,
        }
    }

    fn _binary(
        left: Expression,
        right: Expression,
        operator: BinaryOperator,
        span: TextSpan,
        return_type: ExpressionReturnType,
    ) -> Self {
        Self {
            kind: ExpressionKind::Binary({
                BinaryExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                    return_type,
                }
            }),
            span,
        }
    }

    fn bool(value: bool, span: TextSpan) -> Self {
        Self {
            kind: ExpressionKind::Bool(value),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionReturnType {
    Bool,
    Int,
    Group,
    None,
}

impl Display for ExpressionReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ExpressionReturnType::Bool => "bool",
            ExpressionReturnType::Int => "int",
            ExpressionReturnType::Group => "all",
            ExpressionReturnType::None => "none",
        };
        write!(f, "{s}")
    }
}

/// Kinds of expression.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Bool(bool),
    Int(i32),
    Binary(BinaryExpression),
    Parenthesized(ParenthesizedExpression),
    Pick(PickExpression),
    VariableRef(VariableRef),
    BlockLink(BlockLinkExpression),
    When(WhenExpression),
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenExpression {
    pub condition: Box<Expression>,
    pub statements: Vec<Statement>,
    pub out: Option<Box<Expression>>,
}

impl WhenExpression {
    pub fn return_type(&self) -> ExpressionReturnType {
        match &self.out {
            Some(o) => o.return_type(),
            None => ExpressionReturnType::None,
        }
    }
}

/// An expression within parenthesis.
#[derive(Debug, Clone, PartialEq)]
pub struct ParenthesizedExpression {
    pub expression: Box<Expression>,
}

impl ParenthesizedExpression {
    fn new(expression: Box<Expression>) -> Self {
        Self { expression }
    }

    fn return_type(&self) -> ExpressionReturnType {
        self.expression.return_type()
    }
}

/// [AST] representation of a chef pick expression.
/// ### Example of a chef pick expression:
/// ```chef
/// picked_signal: int = all_signals[some_signal];
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct PickExpression {
    pub pick_signal: String,
    pub from: VariableRef,
}

impl PickExpression {
    fn new(pick_signal: String, from: VariableRef) -> Self {
        Self { pick_signal, from }
    }
}

/// [AST] representation of chef block link. This is like a function call in other languages.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockLinkExpression {
    pub block: Rc<Block>,
    pub inputs: Vec<Expression>,
}

impl BlockLinkExpression {
    pub fn new(block: Rc<Block>, inputs: Vec<Expression>) -> Self {
        Self { block, inputs }
    }

    fn return_type(&self) -> ExpressionReturnType {
        self.block.output_type.return_type()
    }
}

/// [AST] representation of an expression containing two operands and one operator.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: BinaryOperator,
    return_type: ExpressionReturnType,
}

impl BinaryExpression {
    fn _new(
        left: Box<Expression>,
        right: Box<Expression>,
        operator: BinaryOperator,
        return_type: ExpressionReturnType,
    ) -> Self {
        Self {
            left,
            right,
            operator,
            return_type,
        }
    }

    fn return_type(&self) -> &ExpressionReturnType {
        &self.return_type
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MutationOperator {
    Add,
    Subtract,
}

impl Display for MutationOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            MutationOperator::Add => "+=",
            MutationOperator::Subtract => "-=",
        };
        write!(f, "{s}")
    }
}

/// Operator used by a [BinaryExpression].
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
}

impl BinaryOperator {
    fn new(kind: BinaryOperatorKind) -> Self {
        Self { kind }
    }

    /// Get the operator's precedence. Operations with highter precedence will be evaluated first.
    fn precedence(&self) -> u8 {
        match self.kind {
            BinaryOperatorKind::LargerThan => 0,
            BinaryOperatorKind::LargerThanOrEqual => 0,
            BinaryOperatorKind::LessThan => 0,
            BinaryOperatorKind::LessThanOrEqual => 0,
            BinaryOperatorKind::Equals => 0,
            BinaryOperatorKind::NotEquals => 0,
            BinaryOperatorKind::Add => 2,
            BinaryOperatorKind::Subtract => 2,
            BinaryOperatorKind::Multiply => 3,
            BinaryOperatorKind::Divide => 3,
        }
    }

    /// Get the type that the operator returns
    fn return_type(&self) -> ExpressionReturnType {
        match self.kind {
            BinaryOperatorKind::Add => ExpressionReturnType::Int,
            BinaryOperatorKind::Subtract => ExpressionReturnType::Int,
            BinaryOperatorKind::Multiply => ExpressionReturnType::Int,
            BinaryOperatorKind::Divide => ExpressionReturnType::Int,
            BinaryOperatorKind::LargerThan => ExpressionReturnType::Bool,
            BinaryOperatorKind::LargerThanOrEqual => ExpressionReturnType::Bool,
            BinaryOperatorKind::LessThan => ExpressionReturnType::Bool,
            BinaryOperatorKind::LessThanOrEqual => ExpressionReturnType::Bool,
            BinaryOperatorKind::Equals => ExpressionReturnType::Bool,
            BinaryOperatorKind::NotEquals => ExpressionReturnType::Bool,
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            BinaryOperatorKind::Add => write!(f, "+"),
            BinaryOperatorKind::Subtract => write!(f, "-"),
            BinaryOperatorKind::Multiply => write!(f, "*"),
            BinaryOperatorKind::Divide => write!(f, "/"),
            BinaryOperatorKind::LargerThan => write!(f, ">"),
            BinaryOperatorKind::LargerThanOrEqual => write!(f, ">="),
            BinaryOperatorKind::LessThan => write!(f, "<"),
            BinaryOperatorKind::LessThanOrEqual => write!(f, "<="),
            BinaryOperatorKind::Equals => write!(f, "=="),
            BinaryOperatorKind::NotEquals => write!(f, "!="),
        }
    }
}

/// Kinds of [BinaryOperator].
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    LargerThan,
    LargerThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equals,
    NotEquals,
}

/// The indentation depth when printing the [AST].
const INDENTATON: usize = 4;

/// A struct for printing [AST]s.
struct Printer {
    current_intent: usize,
}

impl Printer {
    /// Instantiate a new printer.
    fn new() -> Self {
        Self { current_intent: 0 }
    }

    /// Print a string at the current indentation.
    fn print(&self, text: &str) {
        println!("{}{}", " ".repeat(self.current_intent), text);
    }

    /// Add one indentation level.
    fn indent(&mut self) {
        self.current_intent += INDENTATON;
    }

    /// Remove one indentation level.
    fn unindent(&mut self) {
        self.current_intent -= INDENTATON;
    }
}

impl Display for VariableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            VariableType::Bool(bool_type) => match bool_type {
                VariableSignalType::Signal(n) => format!("Bool({n})"),
                VariableSignalType::Any => "Bool(Any)".to_string(),
            },
            VariableType::Int(int_type) => match int_type {
                VariableSignalType::Signal(n) => format!("Int({n})"),
                VariableSignalType::Any => "Int(Any)".to_string(),
            },
            VariableType::Var(var_type) => match var_type {
                VariableSignalType::Signal(n) => format!("Var({n})"),
                VariableSignalType::Any => "Var(Any)".to_string(),
            },
            VariableType::Counter((var_type, _lim)) => match var_type {
                VariableSignalType::Signal(n) => format!("Counter({n})"),
                VariableSignalType::Any => "Counter(Any)".to_string(),
            },
            VariableType::ConstInt(_) => "ConstInt".to_string(),
            VariableType::ConstBool(_) => "ConstBool".to_string(),
            VariableType::All => "All".to_string(),
        };
        write!(f, "{s}")
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.name, self.type_)
    }
}

impl Visitor for Printer {
    fn visit_statement(&mut self, statement: &Statement) {
        self.print("Statement:");
        self.indent();
        self.do_visit_statement(statement);
        self.unindent();
    }

    fn visit_expression_statement(&mut self, expr: &Expression) {
        self.print("ExpressionStatement:");
        self.indent();
        self.visit_expression(expr);
        self.unindent();
    }

    fn visit_block(&mut self, block: &Block) {
        self.print(&format!(
            "Block: \"{}\" {:?} -> {:?}",
            block.name,
            block
                .inputs
                .iter()
                .map(|i| (i.name.clone(), i.type_.to_string()))
                .collect::<Vec<(String, String)>>(),
            block.output_type
        ));
        self.indent();

        self.print("BlockStatements:");
        self.indent();
        for statement in &block.statements {
            self.visit_statement(statement);
        }
        self.unindent();

        self.print("BlockOutput:");
        self.indent();
        self.visit_expression(&block.output);
        self.unindent();

        self.unindent();
    }

    fn visit_assignment(&mut self, assignment: &Assignment) {
        self.print(&format!(
            "Assignment: \"{} ({})\"",
            assignment.variable.name, assignment.variable.type_
        ));
        self.indent();
        self.do_visit_assignment(assignment);
        self.unindent();
    }

    fn visit_mutation(&mut self, mutation: &Mutation) {
        self.print(&format!("Mutation: \"{}\"", mutation.var_ref.var.name));
        self.indent();
        self.print(&format!("Operation: {}", mutation.operator));
        self.visit_expression(&mutation.expression);
        self.unindent();
    }

    fn visit_expression(&mut self, expression: &Expression) {
        self.print("Expression:");
        self.indent();
        self.do_visit_expression(expression);
        self.unindent();
    }

    fn visit_number(&mut self, number: &i32) {
        self.print(&format!("Number: {}", number));
    }

    fn visit_bool(&mut self, bool: &bool) {
        self.print(&format!("Boolean: {}", bool));
    }

    fn visit_variable_ref(&mut self, var_ref: &VariableRef) {
        let var = var_ref.var.clone();
        self.print(&format!("VariableRef: {}", var.name))
    }

    fn visit_binary_expression(&mut self, binary_expression: &BinaryExpression) {
        self.print("BinaryExpression:");
        self.indent();
        self.print(&format!("Operator: {}", binary_expression.operator));
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
        self.unindent();
    }

    fn visit_parenthesized_expression(&mut self, expr: &ParenthesizedExpression) {
        self.print("Parenthesized:");
        self.indent();
        self.visit_expression(&expr.expression);
        self.unindent();
    }

    fn visit_error_statement(&mut self) {
        self.print("ErrorStatement:")
    }

    fn visit_error_expression(&mut self) {
        self.print("ErrorExpression:")
    }

    fn visit_pick_expression(&mut self, expr: &PickExpression) {
        self.print("PickExpression:");
        self.indent();
        self.print(&format!("Pick Signal: {}", expr.pick_signal));
        self.print(&format!("From Variable: {}", expr.from.var.name));
        self.unindent();
    }

    fn visit_block_link(&mut self, block: &BlockLinkExpression) {
        self.print(&format!("BlockLink: \"{}\"", block.block.name));
        self.indent();
        self.print(&format!("Args: ({})", block.inputs.len()));
        self.indent();
        for input in &block.inputs {
            self.visit_expression(input);
        }
        self.unindent();
        self.unindent();
    }

    fn visit_out(&mut self, expr: &Expression) {
        self.print("Out:");
        self.indent();
        self.visit_expression(expr);
        self.unindent();
    }

    fn visit_when_expression(&mut self, when: &WhenExpression) {
        self.print("WhenExpression:");
        self.indent();
        self.print("Condition:");
        self.indent();
        self.do_visit_expression(&when.condition);
        self.unindent();
        for statement in &when.statements {
            self.do_visit_statement(statement);
        }
        self.print("WhenOutput:");
        self.indent();
        match &when.out {
            Some(out) => self.visit_expression(out),
            None => self.print("No Output."),
        }
        self.unindent();
        self.unindent();
    }
}

#[cfg(test)]
impl AST {
    /// This function is only for testing
    fn from_str(code: &str) -> (Self, crate::diagnostics::DiagnosticsBagRef) {
        let source = Rc::new(SourceText::from_str(code));
        let opts = Rc::new(Opts::new_test());
        let bag = crate::diagnostics::DiagnosticsBag::new_ref(opts.clone(), source.clone());
        (AST::from_source(source, bag.clone(), opts), bag)
    }
}
