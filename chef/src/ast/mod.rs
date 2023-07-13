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

/// The abstract syntax tree.
#[allow(clippy::upper_case_acronyms)]
pub struct AST {
    pub statements: Vec<Statement>,
    diagnostics_bag: DiagnosticsBagRef,
}

impl AST {
    /// Instantiate a new [AST].
    pub fn new(diagnostics_bag: DiagnosticsBagRef) -> Self {
        Self {
            statements: vec![],
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
        for statement in parser {
            ast.add_statement(statement);
        }
        ast.evaluate_constants();
        ast.check_types();
        ast
    }

    /// Add a statement to the [AST].
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    /// Print the [AST] to stout.
    pub fn print(&self) {
        let mut printer = Printer::new();
        for statement in &self.statements {
            printer.visit_statement(statement);
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
    Assignment(Assignment),
    Block(Block),
    Out(Expression),
    Error,
}

/// Chef variable types.
#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    Int(VariableSignalType),
    Bool(VariableSignalType),
    All,
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
        match self.type_ {
            VariableType::Int(_) => ExpressionReturnType::Int,
            VariableType::Bool(_) => ExpressionReturnType::Bool,
            VariableType::All => ExpressionReturnType::Group,
        }
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

/// [AST] representation of chef variable assignment.
#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub variable: Rc<Variable>,
    pub expression: Expression,
}

impl Assignment {
    /// Instantiate a new [Assignment].
    pub fn new(variable: Rc<Variable>, expression: Expression) -> Self {
        Self {
            variable,
            expression,
        }
    }
}

/// [AST] representation of chef `block`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub name: String,
    pub inputs: Vec<Rc<Variable>>,
    pub output: VariableType,
    pub statements: Vec<Statement>,
    pub span: TextSpan,
}

impl Block {
    /// Instantiate a new [Block].
    fn new(
        name: String,
        inputs: Vec<Rc<Variable>>,
        output: VariableType,
        statements: Vec<Statement>,
        span: TextSpan,
    ) -> Self {
        Self {
            name,
            inputs,
            output,
            statements,
            span,
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
            ExpressionKind::VariableDef(var) => var.return_type(),
            ExpressionKind::VariableRef(var_ref) => var_ref.return_type(),
            ExpressionKind::BlockLink(e) => e.return_type(),
            ExpressionKind::When(e) => e.return_type(),
            ExpressionKind::Error => ExpressionReturnType::Int,
        }
    }

    fn _number(n: i32, span: TextSpan) -> Self {
        Self {
            kind: ExpressionKind::Int(IntExpression::new(n)),
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
}

impl Display for ExpressionReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ExpressionReturnType::Bool => "bool",
            ExpressionReturnType::Int => "int",
            ExpressionReturnType::Group => "all",
        };
        write!(f, "{s}")
    }
}

/// Kinds of expression.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Bool(bool),
    Int(IntExpression),
    Binary(BinaryExpression),
    Parenthesized(ParenthesizedExpression),
    Pick(PickExpression),
    VariableDef(Rc<Variable>),
    VariableRef(VariableRef),
    BlockLink(BlockLinkExpression),
    When(WhenExpression),
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenExpression {
    condition: Box<Expression>,
    statements: Vec<Statement>,
    out: Box<Expression>,
}

impl WhenExpression {
    pub fn return_type(&self) -> ExpressionReturnType {
        self.out.return_type()
    }
}

/// An expression containing only a single number.
#[derive(Debug, Clone, PartialEq)]
pub struct IntExpression {
    pub number: i32,
}

impl IntExpression {
    fn new(number: i32) -> Self {
        Self { number }
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
    pub from: Rc<Variable>,
}

impl PickExpression {
    fn new(pick_signal: String, from: Rc<Variable>) -> Self {
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
        match self.block.output {
            VariableType::Int(_) => ExpressionReturnType::Int,
            VariableType::Bool(_) => ExpressionReturnType::Bool,
            VariableType::All => ExpressionReturnType::Group,
        }
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

/// Operator used by a [BinaryExpression].
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
}

impl BinaryOperator {
    fn new(kind: BinaryOperatorKind) -> Self {
        Self { kind }
    }

    /// Get the operator's precedence.
    fn precedence(&self) -> u8 {
        match self.kind {
            BinaryOperatorKind::Plus => 2,
            BinaryOperatorKind::Minus => 2,
            BinaryOperatorKind::Multiply => 3,
            BinaryOperatorKind::Divide => 3,
            BinaryOperatorKind::LargerThan => 1,
            BinaryOperatorKind::LargerThanOrEqual => 1,
            BinaryOperatorKind::LessThan => 1,
            BinaryOperatorKind::LessThanOrEqual => 1,
            BinaryOperatorKind::Equals => 1,
            BinaryOperatorKind::NotEquals => 1,
        }
    }

    /// Get the type that the operator returns
    fn return_type(&self) -> ExpressionReturnType {
        match self.kind {
            BinaryOperatorKind::Plus => ExpressionReturnType::Int,
            BinaryOperatorKind::Minus => ExpressionReturnType::Int,
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
            BinaryOperatorKind::Plus => write!(f, "+"),
            BinaryOperatorKind::Minus => write!(f, "-"),
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
    Plus,
    Minus,
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
            VariableType::Int(int_type) => match int_type {
                VariableSignalType::Signal(n) => format!("Int({n})"),
                VariableSignalType::Any => "Int(Any)".to_string(),
            },
            VariableType::Bool(bool_type) => match bool_type {
                VariableSignalType::Signal(n) => format!("Bool({n})"),
                VariableSignalType::Any => "Bool(Any)".to_string(),
            },
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

    fn visit_block(&mut self, block: &Block) {
        self.print(&format!(
            "Block: \"{}\" {:?} -> {:?}",
            block.name,
            block
                .inputs
                .iter()
                .map(|i| (i.name.clone(), i.type_.to_string()))
                .collect::<Vec<(String, String)>>(),
            block.output
        ));
        self.indent();
        self.do_visit_block(block);
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

    fn visit_expression(&mut self, expression: &Expression) {
        self.print("Expression:");
        self.indent();
        self.do_visit_expression(expression);
        self.unindent();
    }

    fn visit_number(&mut self, number: &IntExpression) {
        self.print(&format!("Number: {}", number.number));
    }

    fn visit_bool(&mut self, bool: &bool) {
        self.print(&format!("Boolean: {}", bool));
    }

    fn visit_variable_def(&mut self, var: &Variable) {
        self.print(&format!(
            "VariableDef: {} (type: {:?})",
            var.name, var.type_
        ))
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
        self.print(&format!("From Variable: {}", expr.from.name));
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
        self.visit_expression(&when.out);
        self.unindent();
        self.unindent();
    }
}

#[cfg(test)]
impl AST {
    /// This function is only for testing
    fn from_str(code: &str) -> (Self, crate::diagnostics::DiagnosticsBagRef) {
        let source = Rc::new(SourceText::from_str(code));
        let opts = Rc::new(Opts::default());
        let bag = crate::diagnostics::DiagnosticsBag::new_ref(opts.clone(), source.clone());
        (AST::from_source(source, bag.clone(), opts), bag)
    }
}
