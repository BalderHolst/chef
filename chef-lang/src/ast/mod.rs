//! Module for lexing and parsing chef source code into an abstract syntax tree and checking for errors.

use std::cell::RefCell;
use std::fmt::Display;
use std::path::PathBuf;
use std::rc::Rc;

use crate::ast::visitors::Visitor;
use crate::compiler::graph::WireKind;
use crate::diagnostics::DiagnosticsBagRef;
use crate::text::{SourceText, TextSpan};
use crate::Opts;

use self::lexer::{Lexer, Token};
use self::parser::Parser;

mod constant_evaluator;
pub mod lexer;
pub mod parser;
pub mod python_macro;
mod type_checker;
mod type_inference;
mod visitors;

pub type MutVar = RefCell<VarData>;

/// [AST] representation of chef `block`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block<V>
where
    V: Variable,
{
    pub id: usize,
    pub name: String,
    pub inputs: Vec<Rc<V>>,
    pub outputs: Vec<Rc<V>>,
    pub statements: Vec<Statement<V>>,
    pub span: TextSpan,
    pub dyn_block_id: Option<usize>,
}

impl<V> Block<V>
where
    V: Variable,
{
    /// Instantiate a new [Block].
    fn new(
        id: usize,
        name: String,
        inputs: Vec<Rc<V>>,
        outputs: Vec<Rc<V>>,
        statements: Vec<Statement<V>>,
        span: TextSpan,
    ) -> Self {
        Self {
            id,
            name,
            inputs,
            outputs,
            statements,
            span,
            dyn_block_id: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DynBlock<V> {
    pub name: String,
    pub inputs: Vec<DynBlockArg<V>>,
    pub script_path: PathBuf,
    pub span: TextSpan,
    pub opts: Rc<Opts>,
}

impl<V> DynBlock<V> {
    fn new(
        name: String,
        inputs: Vec<DynBlockArg<V>>,
        script_path: PathBuf,
        span: TextSpan,
        opts: Rc<Opts>,
    ) -> Self {
        Self {
            name,
            inputs,
            script_path,
            span,
            opts,
        }
    }
}

/// The abstract syntax tree.
#[allow(clippy::upper_case_acronyms)]
pub struct AST<V>
where
    V: Variable,
{
    pub blocks: Vec<Block<V>>,
    diagnostics_bag: DiagnosticsBagRef,
}

impl AST<MutVar> {
    /// Build an [AST] from a [SourceText] instance. This also evaluates constants and does type
    /// checking.
    pub fn from_source(
        text: Rc<SourceText>,
        diagnostics_bag: DiagnosticsBagRef,
        opts: Rc<Opts>,
    ) -> Self {
        let lexer = Lexer::from_source(text);
        let tokens: Vec<Token> = lexer.collect();
        let parser = Parser::new(tokens, diagnostics_bag.clone(), opts);
        let mut ast = AST::new(diagnostics_bag);
        for block in parser {
            ast.add_block(block);
        }

        ast.infer_types();

        // Check types before constant evaluation to make sure that bool constants
        // are not evaluated as int constants and vise versa.
        ast.check_types();
        ast.evaluate_constants();
        ast
    }
}

impl<V> AST<V>
where
    V: Variable,
{
    /// Instantiate a new [AST].
    pub fn new(diagnostics_bag: DiagnosticsBagRef) -> Self {
        Self {
            blocks: vec![],
            diagnostics_bag,
        }
    }

    /// Add a statement to the [AST].
    pub fn add_block(&mut self, block: Block<V>) {
        self.blocks.push(block);
    }

    pub fn get_block(&self, name: &str, id: Option<usize>) -> Option<&Block<V>> {
        self.blocks
            .iter()
            .find(|b| b.name == name && b.dyn_block_id == id)
    }
}

impl AST<MutVar> {
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

    pub fn infer_types(&mut self) {
        type_inference::infer(self);
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
pub struct Statement<V>
where
    V: Variable,
{
    pub kind: StatementKind<V>,
    pub span: TextSpan,
}

impl<V> Statement<V>
where
    V: Variable,
{
    /// Instantiate a new [Statement].
    fn new(kind: StatementKind<V>, span: TextSpan) -> Self {
        Statement { kind, span }
    }
}

/// Kinds of statement.
#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind<V>
where
    V: Variable,
{
    When(WhenStatement<V>),
    Declaration(Declaration<V>),
    DeclarationDefinition(DeclarationDefinition<V>),
    Definition(Definition<V>),
    TupleDeclarationDefinition(TupleDeclarationDefinition<V>),
}

/// Chef variable types.
#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    Bool(VariableSignalType),
    Int(VariableSignalType),
    Var(VariableSignalType),
    Many,
    Inferred,
}

impl VariableType {
    /// Get the variable type as expressed in the chef language
    pub fn signature(&self) -> String {
        match self {
            VariableType::Bool(_) => "bool".to_string(),
            VariableType::Int(_) => "int".to_string(),
            VariableType::Var(_) => "var".to_string(),
            VariableType::Many => "many".to_string(),
            VariableType::Inferred => todo!(),
        }
    }

    pub fn return_type(&self) -> ExpressionReturnType {
        match self {
            VariableType::Bool(_) => ExpressionReturnType::Bool,
            VariableType::Int(_) => ExpressionReturnType::Int,
            VariableType::Var(_) => ExpressionReturnType::Int,
            VariableType::Many => ExpressionReturnType::Many,
            VariableType::Inferred => ExpressionReturnType::Infered,
        }
    }

    pub fn signal_type(&self) -> Option<&VariableSignalType> {
        match self {
            VariableType::Bool(s) => Some(s),
            VariableType::Int(s) => Some(s),
            VariableType::Var(s) => Some(s),
            VariableType::Many => None,
            VariableType::Inferred => None,
        }
    }

    pub fn signal(&self) -> Option<String> {
        match self.signal_type()? {
            VariableSignalType::Signal(s) => Some(s.to_owned()),
            VariableSignalType::Any => None,
        }
    }
}

impl From<ExpressionReturnType> for VariableType {
    fn from(val: ExpressionReturnType) -> Self {
        match val {
            ExpressionReturnType::Bool => VariableType::Bool(VariableSignalType::Any),
            ExpressionReturnType::Int => VariableType::Int(VariableSignalType::Any),
            ExpressionReturnType::Many => VariableType::Many,
            ExpressionReturnType::None => todo!(),
            ExpressionReturnType::Infered => VariableType::Inferred,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableSignalType {
    Signal(String),
    Any,
}

pub type VariableId = usize;

/// An argument to a block definition.
#[derive(Debug, Clone)]
pub enum DynBlockArg<V> {
    Var(Rc<V>),
    Literal(String),
}

/// An argument to a block link.
#[derive(Debug, Clone)]
enum BlockLinkArg<V>
where
    V: Variable,
{
    Expr(Expression<V>),
    Literal(TextSpan),
}

impl<V> BlockLinkArg<V>
where
    V: Variable,
{
    fn span(&self) -> &TextSpan {
        match self {
            Self::Expr(expr) => &expr.span,
            Self::Literal(span) => span,
        }
    }
}

pub trait Variable {
    fn name(&self) -> String;
    fn type_(&self) -> VariableType;
    fn span(&self) -> TextSpan;
    fn id(&self) -> VariableId;
    fn return_type(&self) -> ExpressionReturnType;
}

/// A chef variable.
#[derive(Debug, Clone, PartialEq)]
pub struct VarData {
    pub name: String,
    pub type_: VariableType,
    pub span: TextSpan,
    pub id: VariableId,
}

impl VarData {
    /// Instantiate a new [Variable].
    pub fn new(name: String, variable_type: VariableType, span: TextSpan, id: usize) -> Self {
        Self {
            name,
            type_: variable_type,
            span,
            id,
        }
    }
}

impl Variable for VarData {
    fn name(&self) -> String {
        self.name.clone()
    }

    fn type_(&self) -> VariableType {
        self.type_.clone()
    }

    fn span(&self) -> TextSpan {
        self.span.clone()
    }

    fn id(&self) -> VariableId {
        self.id
    }

    fn return_type(&self) -> ExpressionReturnType {
        self.type_.return_type()
    }
}

impl Variable for MutVar {
    fn name(&self) -> String {
        self.borrow().name.clone()
    }

    fn type_(&self) -> VariableType {
        self.borrow().type_.clone()
    }

    fn span(&self) -> TextSpan {
        self.borrow().span.clone()
    }

    fn id(&self) -> VariableId {
        self.borrow().id
    }

    fn return_type(&self) -> ExpressionReturnType {
        self.borrow().type_.return_type()
    }
}

/// A reference to a defined chef variable
#[derive(Debug, Clone, PartialEq)]
pub struct VariableRef<V> {
    pub var: Rc<V>,
    pub span: TextSpan,
}

impl<V> VariableRef<V> {
    fn new(var: Rc<V>, span: TextSpan) -> Self {
        Self { var, span }
    }
}

impl VariableRef<MutVar> {
    pub fn return_type(&self) -> ExpressionReturnType {
        self.var.borrow().return_type()
    }
}

/// [AST] representation of chef `int` variable assignment.
#[derive(Debug, Clone, PartialEq)]
pub struct Declaration<V> {
    pub variable: Rc<V>,
}

impl<V> Declaration<V> {
    /// Instantiate a new [Declaration].
    pub fn new(variable: Rc<V>) -> Self {
        Self { variable }
    }
}

/// [AST] representation of chef `int` variable assignment and declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationDefinition<V>
where
    V: Variable,
{
    // TODO: Make this contain a `Declaration` and a `Definition`
    pub variable: Rc<V>,
    pub expression: Expression<V>,
    pub kind: DefinitionKind,
}

impl<V> DeclarationDefinition<V>
where
    V: Variable,
{
    /// Instantiate a new [Assignment].
    pub fn new(variable: Rc<V>, expression: Expression<V>, kind: DefinitionKind) -> Self {
        Self {
            variable,
            expression,
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DefinitionKind {
    Wire(WireKind),
    Equal,
}

/// [AST] representation of chef `int` variable assignment.
#[derive(Debug, Clone, PartialEq)]
pub struct Definition<V>
where
    V: Variable,
{
    pub variable: Rc<V>,
    pub expression: Expression<V>,
    pub kind: DefinitionKind,
}

impl<V> Definition<V>
where
    V: Variable,
{
    /// Instantiate a new [Assignment].
    pub fn new(variable: Rc<V>, expression: Expression<V>, kind: DefinitionKind) -> Self {
        Self {
            variable,
            expression,
            kind,
        }
    }
}

impl<V> From<DeclarationDefinition<V>> for Definition<V>
where
    V: Variable,
{
    fn from(value: DeclarationDefinition<V>) -> Self {
        Self {
            variable: value.variable,
            expression: value.expression,
            kind: value.kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentType {
    Definition,
    Declaration,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OutputAssignment<V> {
    pub variable: Rc<V>,
    pub block_variable: Rc<V>,
    pub assignment_type: AssignmentType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleDeclarationDefinition<V>
where
    V: Variable,
{
    pub defs: Vec<OutputAssignment<V>>,
    pub block_link: BlockLinkExpression<V>,
    pub def_kind: DefinitionKind,
}

/// A chef expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Expression<V>
where
    V: Variable,
{
    pub kind: ExpressionKind<V>,
    pub span: TextSpan,
}

impl<V> Expression<V>
where
    V: Variable,
{
    fn new(kind: ExpressionKind<V>, span: TextSpan) -> Self {
        Self { kind, span }
    }

    fn _number(n: i32, span: TextSpan) -> Self {
        Self {
            kind: ExpressionKind::Int(n),
            span,
        }
    }

    fn _binary(
        left: Expression<V>,
        right: Expression<V>,
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
                    span: span.clone(),
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

impl Expression<MutVar> {
    fn return_type(&self) -> ExpressionReturnType {
        match &self.kind {
            ExpressionKind::Bool(_) => ExpressionReturnType::Bool,
            ExpressionKind::Int(_) => ExpressionReturnType::Int,
            ExpressionKind::Binary(e) => e.return_type().clone(),
            ExpressionKind::Parenthesized(e) => e.return_type(),
            ExpressionKind::Negative(e) => e.return_type(),
            ExpressionKind::Pick(_) => ExpressionReturnType::Int,
            ExpressionKind::Index(_) => ExpressionReturnType::Int,
            ExpressionKind::VariableRef(var_ref) => var_ref.return_type(),
            ExpressionKind::BlockLink(e) => e.return_type(true),
            ExpressionKind::Delay(e) => e.return_type(),
            ExpressionKind::SizeOf(e) => e.return_type(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionReturnType {
    Bool,
    Int,
    Many,
    None,
    Infered,
}

impl Display for ExpressionReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ExpressionReturnType::Bool => "bool",
            ExpressionReturnType::Int => "int",
            ExpressionReturnType::Many => "many",
            ExpressionReturnType::None => "none",
            ExpressionReturnType::Infered => "infered",
        };
        write!(f, "{s}")
    }
}

/// Kinds of expression.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind<V>
where
    V: Variable,
{
    Bool(bool),
    Int(i32),
    Binary(BinaryExpression<V>),
    Parenthesized(ParenthesizedExpression<V>),
    Negative(NegativeExpression<V>),
    Pick(PickExpression<V>),
    Index(IndexExpression<V>),
    VariableRef(VariableRef<V>),
    BlockLink(BlockLinkExpression<V>),
    Delay(DelayExpression<V>),
    SizeOf(SizeOfExpression<V>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenStatement<V>
where
    V: Variable,
{
    pub condition: Expression<V>,
    pub statements: Vec<Statement<V>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NegativeExpression<V>
where
    V: Variable,
{
    pub expression: Box<Expression<V>>,
}

impl<V> NegativeExpression<V>
where
    V: Variable,
{
    fn new(expression: Expression<V>) -> Self {
        Self {
            expression: Box::new(expression),
        }
    }
}

impl NegativeExpression<MutVar> {
    fn return_type(&self) -> ExpressionReturnType {
        self.expression.return_type()
    }
}

/// An expression within parenthesis.
#[derive(Debug, Clone, PartialEq)]
pub struct ParenthesizedExpression<V>
where
    V: Variable,
{
    pub expression: Box<Expression<V>>,
}

impl<V> ParenthesizedExpression<V>
where
    V: Variable,
{
    fn new(expression: Box<Expression<V>>) -> Self {
        Self { expression }
    }
}

impl ParenthesizedExpression<MutVar> {
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
pub struct PickExpression<V> {
    pub pick_signal: String,
    pub from: VariableRef<V>,
    pub span: TextSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression<V> {
    pub var_ref: VariableRef<V>,
    pub index: u16,
}

impl<V> PickExpression<V> {
    fn new(pick_signal: String, from: VariableRef<V>, span: TextSpan) -> Self {
        Self {
            pick_signal,
            from,
            span,
        }
    }
}

/// [AST] representation of chef block link. This is like a function call in other languages.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockLinkExpression<V>
where
    V: Variable,
{
    pub block: Rc<Block<V>>,
    pub inputs: Vec<Expression<V>>,
}

impl<V> BlockLinkExpression<V>
where
    V: Variable,
{
    pub fn new(block: Rc<Block<V>>, inputs: Vec<Expression<V>>) -> Self {
        Self { block, inputs }
    }
}

impl BlockLinkExpression<MutVar> {
    fn return_type(&self, return_int: bool) -> ExpressionReturnType {
        match self.block.outputs.len() {
            0 => ExpressionReturnType::None,
            // In expressions, we want the block link to act as a single value
            1 if return_int => self.block.outputs[0].borrow().return_type(),
            _ => ExpressionReturnType::Many,
        }
    }
}

/// An expression that is delayed by a number of cycles.
#[derive(Debug, Clone, PartialEq)]
pub struct DelayExpression<V>
where
    V: Variable,
{
    pub expression: Box<Expression<V>>,
    pub delay: usize,
}

impl<V> DelayExpression<V>
where
    V: Variable,
{
    fn new(expression: Box<Expression<V>>, delay: usize) -> Self {
        Self { expression, delay }
    }
}

impl DelayExpression<MutVar> {
    fn return_type(&self) -> ExpressionReturnType {
        self.expression.return_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SizeOfExpression<V>
where
    V: Variable,
{
    pub expression: Box<Expression<V>>,
}

impl<V> SizeOfExpression<V>
where
    V: Variable,
{
    fn new(expression: Box<Expression<V>>) -> Self {
        Self { expression }
    }

    fn return_type(&self) -> ExpressionReturnType {
        ExpressionReturnType::Int
    }
}

/// [AST] representation of an expression containing two operands and one operator.
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression<V>
where
    V: Variable,
{
    pub left: Box<Expression<V>>,
    pub right: Box<Expression<V>>,
    pub operator: BinaryOperator,
    return_type: ExpressionReturnType,
    span: TextSpan,
}

impl<V> BinaryExpression<V>
where
    V: Variable,
{
    fn _new(
        left: Box<Expression<V>>,
        right: Box<Expression<V>>,
        operator: BinaryOperator,
        return_type: ExpressionReturnType,
        span: TextSpan,
    ) -> Self {
        Self {
            left,
            right,
            operator,
            return_type,
            span,
        }
    }

    fn return_type(&self) -> &ExpressionReturnType {
        &self.return_type
    }
}

/// Kinds of [BinaryOperator].
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
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
    Combine,
    EveryEquals,
    EveryLargerThan,
    EveryLargerThanEquals,
    EveryLessThan,
    EveryLessThanEquals,
    EveryNotEquals,
    AnyEquals,
    AnyLargerThan,
    AnyLargerThanEquals,
    AnyLessThan,
    AnyLessThanEquals,
    AnyNotEquals,
}

impl BinaryOperator {
    /// Get the operator's precedence. Operations with highter precedence will be evaluated first.
    fn precedence(&self) -> u8 {
        match self {
            Self::Combine => 0,
            Self::LargerThan => 1,
            Self::LargerThanOrEqual => 1,
            Self::LessThan => 1,
            Self::LessThanOrEqual => 1,
            Self::Equals => 1,
            Self::NotEquals => 1,
            Self::EveryEquals => 1,
            Self::EveryLargerThan => 1,
            Self::EveryLargerThanEquals => 1,
            Self::EveryLessThan => 1,
            Self::EveryLessThanEquals => 1,
            Self::EveryNotEquals => 1,
            Self::AnyEquals => 1,
            Self::AnyLargerThan => 1,
            Self::AnyLargerThanEquals => 1,
            Self::AnyLessThan => 1,
            Self::AnyLessThanEquals => 1,
            Self::AnyNotEquals => 1,
            Self::Add => 2,
            Self::Subtract => 2,
            Self::Multiply => 3,
            Self::Divide => 3,
        }
    }

    /// Get the type that the operator returns
    fn return_type(
        &self,
        a: ExpressionReturnType,
        b: ExpressionReturnType,
    ) -> Result<ExpressionReturnType, ()> {
        match self {
            Self::Add | Self::Subtract | Self::Multiply | Self::Divide => match (a, b) {
                (ExpressionReturnType::Int, ExpressionReturnType::Int) => {
                    Ok(ExpressionReturnType::Int)
                }
                (ExpressionReturnType::Many, ExpressionReturnType::Int) => {
                    Ok(ExpressionReturnType::Many)
                }
                _ => Err(()),
            },

            Self::LargerThan
            | Self::LargerThanOrEqual
            | Self::LessThan
            | Self::LessThanOrEqual
            | Self::Equals
            | Self::NotEquals => match (a, b) {
                (ExpressionReturnType::Int, ExpressionReturnType::Int) => {
                    Ok(ExpressionReturnType::Bool)
                }
                (ExpressionReturnType::Many, ExpressionReturnType::Int) => {
                    Ok(ExpressionReturnType::Many)
                }
                _ => Err(()),
            },

            Self::EveryEquals => Ok(ExpressionReturnType::Bool),
            Self::EveryLargerThan => Ok(ExpressionReturnType::Bool),
            Self::EveryLargerThanEquals => Ok(ExpressionReturnType::Bool),
            Self::EveryLessThan => Ok(ExpressionReturnType::Bool),
            Self::EveryLessThanEquals => Ok(ExpressionReturnType::Bool),
            Self::EveryNotEquals => Ok(ExpressionReturnType::Bool),
            Self::AnyEquals => Ok(ExpressionReturnType::Bool),
            Self::AnyLargerThan => Ok(ExpressionReturnType::Bool),
            Self::AnyLargerThanEquals => Ok(ExpressionReturnType::Bool),
            Self::AnyLessThan => Ok(ExpressionReturnType::Bool),
            Self::AnyLessThanEquals => Ok(ExpressionReturnType::Bool),
            Self::AnyNotEquals => Ok(ExpressionReturnType::Bool),

            Self::Combine => Ok(ExpressionReturnType::Many),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Subtract => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::LargerThan => write!(f, ">"),
            Self::LargerThanOrEqual => write!(f, ">="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::Equals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
            Self::Combine => write!(f, "@"),
            Self::EveryEquals => write!(f, "@=="),
            Self::EveryLargerThan => write!(f, "@>"),
            Self::EveryLargerThanEquals => write!(f, "@>="),
            Self::EveryLessThan => write!(f, "@<"),
            Self::EveryLessThanEquals => write!(f, "@<="),
            Self::EveryNotEquals => write!(f, "@!="),
            Self::AnyEquals => write!(f, "?=="),
            Self::AnyLargerThan => write!(f, "?>"),
            Self::AnyLargerThanEquals => write!(f, "?>="),
            Self::AnyLessThan => write!(f, "?<"),
            Self::AnyLessThanEquals => write!(f, "?<="),
            Self::AnyNotEquals => write!(f, "?!="),
        }
    }
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
            VariableType::Many => "Many".to_string(),
            VariableType::Inferred => "Inferred".to_string(),
        };
        write!(f, "{s}")
    }
}

impl Display for VarData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.name, self.type_)
    }
}

impl Visitor<MutVar> for Printer {
    fn visit_statement(&mut self, statement: &Statement<MutVar>) {
        self.print("Statement:");
        self.indent();
        self.do_visit_statement(statement);
        self.unindent();
    }

    fn visit_block(&mut self, block: &Block<MutVar>) {
        self.print(&format!(
            "Block: \"{}\" {:?} -> {:?}",
            block.name,
            block
                .inputs
                .iter()
                .map(|i| {
                    let i = i.borrow();
                    format!("{}: {} ({})", i.name, i.type_, i.id)
                })
                .collect::<Vec<String>>(),
            block
                .outputs
                .iter()
                .map(|i| {
                    let i = i.borrow();
                    format!("{}: {} ({})", i.name, i.type_, i.id)
                })
                .collect::<Vec<String>>(),
        ));
        self.indent();

        self.print("BlockStatements:");
        self.indent();
        for statement in &block.statements {
            self.visit_statement(statement);
        }
        self.unindent();
        self.unindent();
    }

    fn visit_declaration(&mut self, dec: &Declaration<MutVar>) {
        let var = dec.variable.borrow();
        self.print(&format!(
            "Declaration: \"{}: {}\" ({})",
            var.name, var.type_, var.id
        ));
    }

    fn visit_definition(&mut self, def: &Definition<MutVar>) {
        let var = def.variable.borrow();
        self.print(&format!(
            "Definition <{:?}>: \"{}: {}\" ({})",
            def.kind, var.name, var.type_, var.id
        ));
        self.indent();
        self.visit_expression(&def.expression);
        self.unindent();
    }

    fn visit_declaration_definition(&mut self, dec_def: &DeclarationDefinition<MutVar>) {
        let var = dec_def.variable.borrow();
        self.print(&format!(
            "DeclarationDefinition <{:?}>: \"{}: {}\" ({})",
            dec_def.kind, var.name, var.type_, var.id
        ));
        self.indent();
        self.do_visit_declaration_definition(dec_def);
        self.unindent();
    }

    fn visit_expression(&mut self, expression: &Expression<MutVar>) {
        self.print("Expression:");
        self.indent();
        self.do_visit_expression(expression);
        self.unindent();
    }

    fn visit_delay_expression(&mut self, delay: &DelayExpression<MutVar>) {
        self.print(&format!("Delay: {}", delay.delay));
        self.indent();
        self.visit_expression(&delay.expression);
        self.unindent();
    }

    fn visit_number(&mut self, number: &i32) {
        self.print(&format!("Number: {}", number));
    }

    fn visit_bool(&mut self, bool: &bool) {
        self.print(&format!("Boolean: {}", bool));
    }

    fn visit_variable_ref(&mut self, var_ref: &VariableRef<MutVar>) {
        let var = var_ref.var.borrow();
        self.print(&format!("VariableRef: {}", var.name))
    }

    fn visit_binary_expression(&mut self, binary_expression: &BinaryExpression<MutVar>) {
        self.print("BinaryExpression:");
        self.indent();
        self.print(&format!("Operator: {}", binary_expression.operator));
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
        self.unindent();
    }

    fn visit_parenthesized_expression(&mut self, expr: &ParenthesizedExpression<MutVar>) {
        self.print("Parenthesized:");
        self.indent();
        self.visit_expression(&expr.expression);
        self.unindent();
    }

    fn visit_pick_expression(&mut self, expr: &PickExpression<MutVar>) {
        self.print("PickExpression:");
        self.indent();
        self.print(&format!("Pick Signal: {}", expr.pick_signal));

        let from_var = expr.from.var.borrow();
        self.print(&format!(
            "From Variable: {} ({})",
            from_var.name, from_var.id
        ));
        self.unindent();
    }

    fn visit_index_expression(&mut self, expr: &IndexExpression<MutVar>) {
        self.print("IndexExpression:");
        self.indent();
        self.print(&format!("Variable: {}", expr.var_ref.var.borrow()));
        self.print(&format!("Size: {}", expr.index));
        self.unindent();
    }

    fn visit_block_link_expression(&mut self, block: &BlockLinkExpression<MutVar>) {
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

    fn visit_when_statement(&mut self, when: &WhenStatement<MutVar>) {
        self.print("WhenExpression:");
        self.indent();
        self.print("Condition:");
        self.indent();
        self.do_visit_expression(&when.condition);
        self.unindent();
        for statement in &when.statements {
            self.do_visit_statement(statement);
        }
        self.unindent();
    }
}

#[cfg(test)]
impl AST<MutVar> {
    /// This function is only for testing
    fn from_str(code: &str) -> (Self, crate::diagnostics::DiagnosticsBagRef) {
        let source = Rc::new(SourceText::from_str(code));
        let opts = Rc::new(Opts::new_test());
        let bag = crate::diagnostics::DiagnosticsBag::new_ref(opts.clone());
        (AST::from_source(source, bag.clone(), opts), bag)
    }
}
