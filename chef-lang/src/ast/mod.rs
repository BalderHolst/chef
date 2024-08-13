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
mod determiner;
pub mod lexer;
pub mod parser;
pub mod python_macro;
mod type_checker;
mod type_inference;
mod visitors;

/// [AST] representation of chef `block`.
#[derive(Debug, Clone, PartialEq)]
pub struct Block<V>
where
    V: Variable,
{
    pub id: BlockId,
    pub dyn_block_id: Option<DynBlockId>,
    pub name: String,
    pub inputs: Vec<Rc<V>>,
    pub outputs: Vec<Rc<V>>,
    pub statements: Vec<Statement<V>>,
    pub span: TextSpan,
}

impl<V> Block<V>
where
    V: Variable,
{
    /// Instantiate a new [Block].
    fn new(
        id: BlockId,
        dyn_block_id: Option<DynBlockId>,
        name: String,
        inputs: Vec<Rc<V>>,
        outputs: Vec<Rc<V>>,
        statements: Vec<Statement<V>>,
        span: TextSpan,
    ) -> Self {
        Self {
            id,
            dyn_block_id,
            name,
            inputs,
            outputs,
            statements,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DynBlock<V> {
    pub name: String,
    pub inputs: Vec<DynBlockArg<V>>,
    pub script_path: PathBuf,
    pub _span: TextSpan,
    pub _opts: Rc<Opts>,
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
            _span: span,
            _opts: opts,
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
    pub fn mut_from_source(
        text: Rc<SourceText>,
        diagnostics_bag: DiagnosticsBagRef,
        opts: Rc<Opts>,
    ) -> Self {
        let lexer = Lexer::from_source(text);
        let tokens: Vec<Token> = lexer.collect();

        let mut ast = Parser::parse(tokens, diagnostics_bag.clone(), opts);

        ast.infer_types();
        ast.evaluate_constants();
        ast
    }
}

impl AST<DetVar> {
    /// Build an [AST] from a [SourceText] instance. This also evaluates constants and does type
    /// checking.
    pub fn from_source(
        text: Rc<SourceText>,
        diagnostics_bag: DiagnosticsBagRef,
        opts: Rc<Opts>,
    ) -> Self {
        let ast = AST::mut_from_source(text, diagnostics_bag, opts);
        let ast = determiner::determine(ast);
        ast.check_types();
        ast
    }

    /// Check that types are valid.
    pub fn check_types(&self) {
        type_checker::check(self, self.diagnostics_bag.clone());
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

    pub fn get_block_by_id(&self, id: BlockId, dyn_block_id: Option<usize>) -> Option<&Block<V>> {
        self.blocks
            .iter()
            .find(|b| b.id == id && b.dyn_block_id == dyn_block_id)
    }

    pub fn get_block_by_name(&self, name: &str, dyn_block_id: Option<usize>) -> Option<&Block<V>> {
        self.blocks
            .iter()
            .find(|b| b.name == name && b.dyn_block_id == dyn_block_id)
    }

    /// Print the [AST] to stout.
    pub fn print(&self) {
        let mut printer = Printer::new(self);
        for block in &self.blocks {
            printer.visit_block(block);
        }
    }
}

impl AST<MutVar> {
    /// Evaluate constant expressions in the [AST] to simplify it.
    pub fn evaluate_constants(&mut self) {
        constant_evaluator::evaluate_constants(self)
    }

    // Consume the AST, infer the types of variables, and return the determined AST.
    pub fn infer_types(&mut self) {
        type_inference::infer(self)
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
    _Tuple(Vec<VariableType>),
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
            VariableType::_Tuple(t) => format!(
                "({})",
                t.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            VariableType::Inferred => todo!(),
        }
    }

    pub fn return_type(&self) -> ExpressionReturnType {
        match self {
            VariableType::Bool(VariableSignalType::Any) => ExpressionReturnType::AnyBool,
            VariableType::Bool(VariableSignalType::Signal(s)) => {
                ExpressionReturnType::Bool(s.clone())
            }
            VariableType::Int(VariableSignalType::Any) => ExpressionReturnType::AnyInt,
            VariableType::Int(VariableSignalType::Signal(s)) => {
                ExpressionReturnType::Int(s.clone())
            }
            VariableType::Var(VariableSignalType::Any) => ExpressionReturnType::AnyInt,
            VariableType::Var(VariableSignalType::Signal(s)) => {
                ExpressionReturnType::Int(s.clone())
            }
            VariableType::Many => ExpressionReturnType::Many,
            VariableType::_Tuple(t) => {
                ExpressionReturnType::Tuple(t.iter().map(|t| t.return_type()).collect())
            }
            VariableType::Inferred => ExpressionReturnType::Infered,
        }
    }

    pub fn signal_type(&self) -> Option<&VariableSignalType> {
        match self {
            VariableType::Bool(s) => Some(s),
            VariableType::Int(s) => Some(s),
            VariableType::Var(s) => Some(s),
            VariableType::Many => None,
            VariableType::_Tuple(_) => None,
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

impl TryFrom<ExpressionReturnType> for VariableType {
    type Error = ();
    fn try_from(value: ExpressionReturnType) -> Result<Self, Self::Error> {
        match value {
            ExpressionReturnType::AnyBool => Ok(VariableType::Bool(VariableSignalType::Any)),
            ExpressionReturnType::AnyInt => Ok(VariableType::Int(VariableSignalType::Any)),
            ExpressionReturnType::Bool(s) => {
                Ok(VariableType::Bool(VariableSignalType::Signal(s.clone())))
            }
            ExpressionReturnType::Int(s) => {
                Ok(VariableType::Int(VariableSignalType::Signal(s.clone())))
            }
            ExpressionReturnType::Many => Ok(VariableType::Many),
            ExpressionReturnType::Tuple(_) => Err(()),
            ExpressionReturnType::Infered => Ok(VariableType::Inferred),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableSignalType {
    Signal(String),
    Any,
}

impl Display for VariableSignalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableSignalType::Signal(s) => write!(f, "{}", s),
            VariableSignalType::Any => write!(f, "any"),
        }
    }
}

pub type VariableId = usize;
pub type BlockId = usize;
pub type DynBlockId = usize;

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
    fn _span(&self) -> TextSpan;
    fn id(&self) -> VariableId;
    fn return_type(&self) -> ExpressionReturnType;
    fn display(&self) -> String {
        format!("{}: {}", self.name(), self.type_())
    }
}

/// A chef variable with a possibly undetermined type.
pub type MutVar = RefCell<VarData>;

/// A fully determined chef variable.
pub type DetVar = VarData;

/// The metadata of a chef variable.
#[derive(Debug, Clone, PartialEq)]
pub struct VarData {
    pub name: String,
    pub type_: VariableType,
    pub span: TextSpan,
    pub id: VariableId,
}

impl VarData {
    pub fn new(name: String, variable_type: VariableType, span: TextSpan, id: usize) -> Self {
        Self {
            name,
            type_: variable_type,
            span,
            id,
        }
    }
}

impl Variable for DetVar {
    fn name(&self) -> String {
        self.name.clone()
    }

    fn type_(&self) -> VariableType {
        self.type_.clone()
    }

    fn _span(&self) -> TextSpan {
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

    fn _span(&self) -> TextSpan {
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

impl<V> VariableRef<V>
where
    V: Variable,
{
    fn new(var: Rc<V>, span: TextSpan) -> Self {
        Self { var, span }
    }

    pub fn return_type(&self) -> ExpressionReturnType {
        self.var.return_type()
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
    Convert(WireKind),
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

    fn return_type(&self) -> ExpressionReturnType {
        match &self.kind {
            ExpressionKind::Bool(_) => ExpressionReturnType::AnyBool,
            ExpressionKind::Int(_) => ExpressionReturnType::AnyInt,
            ExpressionKind::Binary(e) => e.return_type(),
            ExpressionKind::Parenthesized(e) => e.return_type(),
            ExpressionKind::Negative(e) => e.return_type(),
            ExpressionKind::Pick(_) => ExpressionReturnType::AnyInt,
            ExpressionKind::Index(_) => todo!(),
            ExpressionKind::VariableRef(var_ref) => var_ref.return_type(),
            ExpressionKind::BlockLink(e) => e.return_type(),
            ExpressionKind::Delay(e) => e.return_type(),
            ExpressionKind::SizeOf(_) => ExpressionReturnType::AnyInt,
            ExpressionKind::Gate(g) => g.gated_expr.return_type(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionReturnType {
    AnyBool,
    AnyInt,
    Bool(String),
    Int(String),
    Many,
    Tuple(Vec<ExpressionReturnType>),
    Infered,
}

impl ExpressionReturnType {
    pub fn is_int(&self) -> bool {
        matches!(self, Self::AnyInt | Self::Int(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::AnyBool | Self::Bool(_))
    }
}

impl Display for ExpressionReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionReturnType::AnyBool => write!(f, "bool"),
            ExpressionReturnType::AnyInt => write!(f, "int"),
            ExpressionReturnType::Bool(s) => write!(f, "bool({s})"),
            ExpressionReturnType::Int(s) => write!(f, "int({s})"),
            ExpressionReturnType::Many => write!(f, "many"),
            ExpressionReturnType::Tuple(ts) => {
                write!(
                    f,
                    "({})",
                    ts.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ExpressionReturnType::Infered => write!(f, "infered"),
        }
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
    Gate(GateExpression<V>),
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

impl<V> PickExpression<V> {
    fn new(pick_signal: String, from: VariableRef<V>, span: TextSpan) -> Self {
        Self {
            pick_signal,
            from,
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression<V> {
    pub var_ref: VariableRef<V>,
    pub index: u16,
}

/// [AST] representation of chef block link. This is like a function call in other languages.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockLinkExpression<V>
where
    V: Variable,
{
    pub inputs: Vec<Expression<V>>,
    pub block_id: BlockId,
    pub dyn_block_id: Option<DynBlockId>,
    pub return_type: ExpressionReturnType,
}

impl<V> BlockLinkExpression<V>
where
    V: Variable,
{
    pub fn new(
        inputs: Vec<Expression<V>>,
        block_id: BlockId,
        dyn_block_id: Option<DynBlockId>,
    ) -> Self {
        Self {
            block_id,
            dyn_block_id,
            inputs,
            return_type: ExpressionReturnType::Infered,
        }
    }

    fn return_type(&self) -> ExpressionReturnType {
        self.return_type.clone()
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateExpression<V>
where
    V: Variable,
{
    pub gate_expr: Box<Expression<V>>,
    pub gated_expr: Box<Expression<V>>,
}

impl<V> GateExpression<V>
where
    V: Variable,
{
    pub fn new(gate_expr: Expression<V>, gated_expr: Expression<V>) -> Self {
        Self {
            gate_expr: Box::new(gate_expr),
            gated_expr: Box::new(gated_expr),
        }
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

    fn return_type(&self) -> ExpressionReturnType {
        // TODO: this should really be calculated here instead of stored in a variable
        self.return_type.clone()
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
        mut a: ExpressionReturnType,
        mut b: ExpressionReturnType,
    ) -> Result<ExpressionReturnType, ()> {
        if a == ExpressionReturnType::Infered || b == ExpressionReturnType::Infered {
            return Ok(ExpressionReturnType::Infered);
        }

        // Unwrap tuples with one element
        if let ExpressionReturnType::Tuple(ref t) = a {
            if t.len() == 1 {
                a = t[0].clone();
            }
        }
        if let ExpressionReturnType::Tuple(ref t) = b {
            if t.len() == 1 {
                b = t[0].clone();
            }
        }

        match self {
            Self::Add | Self::Subtract | Self::Multiply | Self::Divide => match (a, b) {
                (a, b) if a.is_int() && b.is_int() => Ok(ExpressionReturnType::AnyInt),
                (ExpressionReturnType::Many, b) if b.is_int() => Ok(ExpressionReturnType::Many),
                _ => Err(()),
            },

            Self::LargerThan
            | Self::LargerThanOrEqual
            | Self::LessThan
            | Self::LessThanOrEqual
            | Self::Equals
            | Self::NotEquals => match (a, b) {
                (a, b) if a.is_int() && b.is_int() => Ok(ExpressionReturnType::AnyBool),
                (ExpressionReturnType::Many, b) if b.is_int() => Ok(ExpressionReturnType::Many),
                _ => Err(()),
            },

            Self::EveryEquals => Ok(ExpressionReturnType::AnyBool),
            Self::EveryLargerThan => Ok(ExpressionReturnType::AnyBool),
            Self::EveryLargerThanEquals => Ok(ExpressionReturnType::AnyBool),
            Self::EveryLessThan => Ok(ExpressionReturnType::AnyBool),
            Self::EveryLessThanEquals => Ok(ExpressionReturnType::AnyBool),
            Self::EveryNotEquals => Ok(ExpressionReturnType::AnyBool),
            Self::AnyEquals => Ok(ExpressionReturnType::AnyBool),
            Self::AnyLargerThan => Ok(ExpressionReturnType::AnyBool),
            Self::AnyLargerThanEquals => Ok(ExpressionReturnType::AnyBool),
            Self::AnyLessThan => Ok(ExpressionReturnType::AnyBool),
            Self::AnyLessThanEquals => Ok(ExpressionReturnType::AnyBool),
            Self::AnyNotEquals => Ok(ExpressionReturnType::AnyBool),

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
struct Printer<'a, V>
where
    V: Variable,
{
    current_intent: usize,
    ast: &'a AST<V>,
}

impl<'a, V> Printer<'a, V>
where
    V: Variable,
{
    /// Instantiate a new printer.
    fn new(ast: &'a AST<V>) -> Self {
        Self {
            current_intent: 0,
            ast,
        }
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
            VariableType::_Tuple(t) => format!(
                "Tuple({})",
                t.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            VariableType::Inferred => "Inferred".to_string(),
        };
        write!(f, "{s}")
    }
}

impl<'a, V> Visitor<V> for Printer<'a, V>
where
    V: Variable,
{
    fn visit_statement(&mut self, statement: &Statement<V>) {
        self.print("Statement:");
        self.indent();
        self.do_visit_statement(statement);
        self.unindent();
    }

    fn visit_block(&mut self, block: &Block<V>) {
        self.print(&format!(
            "Block: \"{}\" {:?} -> {:?}",
            block.name,
            block
                .inputs
                .iter()
                .map(|i| { format!("{}: {} ({})", i.name(), i.type_(), i.id()) })
                .collect::<Vec<String>>(),
            block
                .outputs
                .iter()
                .map(|i| { format!("{}: {} ({})", i.name(), i.type_(), i.id()) })
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

    fn visit_declaration(&mut self, dec: &Declaration<V>) {
        let var = &dec.variable;
        self.print(&format!(
            "Declaration: \"{}: {}\" ({})",
            var.name(),
            var.type_(),
            var.id()
        ));
    }

    fn visit_definition(&mut self, def: &Definition<V>) {
        let var = &def.variable;
        self.print(&format!(
            "Definition <{:?}>: \"{}: {}\" ({})",
            def.kind,
            var.name(),
            var.type_(),
            var.id()
        ));
        self.indent();
        self.visit_expression(&def.expression);
        self.unindent();
    }

    fn visit_declaration_definition(&mut self, dec_def: &DeclarationDefinition<V>) {
        let var = &dec_def.variable;
        self.print(&format!(
            "DeclarationDefinition <{:?}>: \"{}: {}\" ({})",
            dec_def.kind,
            var.name(),
            var.type_(),
            var.id()
        ));
        self.indent();
        self.do_visit_declaration_definition(dec_def);
        self.unindent();
    }

    fn visit_expression(&mut self, expression: &Expression<V>) {
        self.print("Expression:");
        self.indent();
        self.do_visit_expression(expression);
        self.unindent();
    }

    fn visit_delay_expression(&mut self, delay: &DelayExpression<V>) {
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

    fn visit_variable_ref(&mut self, var_ref: &VariableRef<V>) {
        self.print(&format!(
            "VariableRef: {} ({})",
            var_ref.var.name(),
            var_ref.var.id()
        ))
    }

    fn visit_binary_expression(&mut self, binary_expression: &BinaryExpression<V>) {
        self.print(&format!(
            "BinaryExpression: -> {}",
            binary_expression.return_type()
        ));
        self.indent();
        self.print(&format!("Operator: {}", binary_expression.operator));
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
        self.unindent();
    }

    fn visit_parenthesized_expression(&mut self, expr: &ParenthesizedExpression<V>) {
        self.print("Parenthesized:");
        self.indent();
        self.visit_expression(&expr.expression);
        self.unindent();
    }

    fn visit_pick_expression(&mut self, expr: &PickExpression<V>) {
        self.print("PickExpression:");
        self.indent();
        self.print(&format!("Pick Signal: {}", expr.pick_signal));

        let from_var = &expr.from.var;
        self.print(&format!(
            "From Variable: {} ({})",
            from_var.name(),
            from_var.id()
        ));
        self.unindent();
    }

    fn visit_index_expression(&mut self, expr: &IndexExpression<V>) {
        self.print("IndexExpression:");
        self.indent();
        self.print(&format!("Variable: {}", expr.var_ref.var.display()));
        self.print(&format!("Size: {}", expr.index));
        self.unindent();
    }

    fn visit_block_link_expression(&mut self, link: &BlockLinkExpression<V>) {
        let block = self
            .ast
            .get_block_by_id(link.block_id, link.dyn_block_id)
            .unwrap();
        self.print(&format!(
            "BlockLink: \"{}\" -> {}",
            block.name,
            link.return_type()
        ));
        self.indent();
        self.print(&format!("Args: ({})", block.inputs.len()));
        self.indent();
        for input in &link.inputs {
            self.visit_expression(input);
        }
        self.unindent();
        self.unindent();
    }

    fn visit_when_statement(&mut self, when: &WhenStatement<V>) {
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
impl AST<DetVar> {
    /// This function is only for testing
    fn from_str(code: &str) -> (Self, crate::diagnostics::DiagnosticsBagRef) {
        let source = Rc::new(SourceText::from_str(code));
        let opts = Rc::new(Opts::new_test());
        let bag = crate::diagnostics::DiagnosticsBag::new_ref(opts.clone());
        (AST::from_source(source, bag.clone(), opts), bag)
    }
}
