use std::fmt::Display;
use std::rc::Rc;

use crate::text::TextSpan;
use crate::ast::visitors::Visitor;

pub mod lexer;
pub mod parser;
mod constant_evaluator;
mod visitors;

pub struct AST {
    pub statements: Vec<Statement>,
}

impl AST {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement);
    }

    pub fn print(&self) -> () {
        let mut printer = Printer::new();
        for statement in &self.statements {
            printer.visit_statement(&statement);
        }
    }

    pub fn evaluate_constants(&mut self) {
        constant_evaluator::evaluate_constants(self)
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: TextSpan,
}

impl Statement {
    fn new(kind: StatementKind, span: TextSpan) -> Self {
        Statement { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
    Assignment(Assignment),
    Block(Block),
    Out(Expression),
    Error,
}


#[derive(Debug, Clone)]
pub enum VariableType {
    Int(String),
    Any,
    All,
    Error,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub variable_type: VariableType,
}

impl Variable {
    pub fn new(name: String, variable_type: VariableType) -> Self {
        Self { name, variable_type }
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub variable: Rc<Variable>,
    pub expression: Expression,
}

impl Assignment {
    pub fn new(variable: Rc<Variable>, expression: Expression) -> Self {
        Self { variable, expression }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub name: String,
    pub inputs: Vec<Rc<Variable>>,
    pub outputs: Vec<VariableType>,
    pub statements: Vec<Statement>
}

impl Block {
    fn new(name: String, inputs: Vec<Rc<Variable>>, outputs: Vec<VariableType>, statements: Vec<Statement>) -> Self {
        Self { name, inputs, outputs, statements }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
}

impl Expression {
    fn new(kind: ExpressionKind) -> Self {
        Self { kind }
    }

    fn number(n: i64) -> Self {
        Self { kind: ExpressionKind::Number(NumberExpression::new(n)) }
    }

    fn binary(left: Expression, right: Expression, operator: BinaryOperator) -> Self {
        Self { kind: ExpressionKind::Binary(BinaryExpression::new(Box::new(left), Box::new(right), operator)) }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Number(NumberExpression),
    Binary(BinaryExpression),
    Parenthesized(ParenthesizedExpression),
    Pick(PickExpression),
    Variable(Rc<Variable>),
    BlockLink(BlockLinkExpression),
    Error,
}

#[derive(Debug, Clone)]
pub struct NumberExpression {
    pub number: i64,
}

impl NumberExpression {
    fn new(number: i64) -> Self {
        Self { number }
    }
}

#[derive(Debug, Clone)]
pub struct ParenthesizedExpression {
    pub expression: Box<Expression>,
}

impl ParenthesizedExpression {
    fn new(expression: Box<Expression>) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct PickExpression {
    pub pick_signal: String,
    pub from: Rc<Variable>,
}


impl PickExpression {
    fn new(pick_signal: String, from: Rc<Variable>) -> Self { Self { pick_signal, from } }
}

#[derive(Debug, Clone)]
pub struct BlockLinkExpression {
    pub block: Rc<Block>,
    pub inputs: Vec<Expression>,
}

impl BlockLinkExpression {
    pub fn new(block: Rc<Block>, inputs: Vec<Expression>) -> Self { Self { block, inputs } }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub operator: BinaryOperator,
}

impl BinaryExpression {
    fn new(left: Box<Expression>, right: Box<Expression>, operator: BinaryOperator) -> Self {
        Self { left, right, operator }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
}

impl BinaryOperator {
    fn new(kind: BinaryOperatorKind) -> Self {
        Self { kind }
    }

    fn precedence(&self) -> u8 {
        match self.kind {
            BinaryOperatorKind::Plus => 2,
            BinaryOperatorKind::Minus => 2,
            BinaryOperatorKind::Multiply => 3,
            BinaryOperatorKind::Divide => 3,
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
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
}

const INDENTATON: usize = 4;

struct Printer {
    current_intent: usize
}

impl Printer {
    fn new() -> Self {
        Self { current_intent: 0 }
    }

    fn print(&self, text: &str) {
        println!("{}{}", " ".repeat(self.current_intent), text);
    }

    fn indent(&mut self) {
        self.current_intent += INDENTATON;
    }

    fn unindent(&mut self) {
        self.current_intent -= INDENTATON;
    }
}

impl Display for VariableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            VariableType::Int(n) => format!("Int({n})"),
            VariableType::Any => "Any".to_string(),
            VariableType::All => "All".to_string(),
            VariableType::Error => "Error".to_string(),
        };
        write!(f, "{s}")
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.name, self.variable_type)
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
        self.print(&format!("Block: \"{}\" {:?} -> {:?}", block.name, block.inputs, block.outputs));
        self.indent();
        self.do_visit_block(&block);
        self.unindent();
    }

    fn visit_assignment(&mut self, assignment: &Assignment) {
        self.print(&format!("Assignment: \"{:?}\"", assignment.variable));
        self.indent();
        self.do_visit_assignment(&assignment);
        self.unindent();
    }

    fn visit_expression(&mut self, expression: &Expression) {
        self.print("Expression:");
        self.indent();
        self.do_visit_expression(expression);
        self.unindent();
    }

    fn visit_number(&mut self, number: &NumberExpression) {
        self.print(&format!("Number: {}", number.number));
    }

    fn visit_variable(&mut self, var: &Variable) {
        self.print(&format!("Variable: {} (type: {:?})", var.name, var.variable_type))
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
        self.print(&format!("From Variable: {:?}", expr.from));
        self.unindent();
    }

    fn visit_block_link(&mut self, block: &BlockLinkExpression) {
        self.print(&format!("BlockLink: \"{}\"", block.block.name));
        self.indent();
        self.print(&format!("Args: ({})", block.inputs.len()));
        self.indent();
        for input in &block.inputs { self.visit_expression(input); }
        self.unindent();
        self.unindent();
    }

    fn visit_out(&mut self, expr: &Expression) {
        self.print("Out:");
        self.indent();
        self.visit_expression(expr);
        self.unindent();
    }
}
