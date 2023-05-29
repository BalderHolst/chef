use std::{fmt::Display, fs::write};
use std::rc::Rc;
use lexer::Token;

pub mod lexer;
pub mod parser;

pub struct AST {
    statements: Vec<Statement>,
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
}

pub trait Visitor {
    fn do_visit_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Expression(expr) => {
                self.visit_expression(expr);
            }
            StatementKind::Block(block) => {
                self.visit_block(block);
            }
        }
    }
    fn visit_statement(&mut self, statement: &Statement) {
        self.do_visit_statement(statement);
    }
    fn do_visit_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::Number(number) => {
                self.visit_number(number);
            }
            ExpressionKind::Variable(variable) => {
                self.visit_variable(variable);
            }
            ExpressionKind::Binary(expr) => {
                self.visit_binary_expression(expr);
            }
            ExpressionKind::Parenthesized(expr) => {
                self.visit_parenthesized_expression(expr);
            }
        }
    }
    fn visit_expression(&mut self, expression: &Expression) {
        self.do_visit_expression(expression);
    }

    fn do_visit_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.visit_statement(&statement);
        }
    }

    fn visit_block(&mut self, block: &Block) {
        self.do_visit_block(block);
    }

    fn visit_number(&mut self, number: &NumberExpression);
    fn visit_variable(&mut self, var: &Variable);

    fn visit_parenthesized_expression(&mut self, expr: &ParenthesizedExpression) {
        self.visit_expression(&expr.expression);
    }

    fn visit_binary_expression(&mut self, binary_expression: &BinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }
}

#[derive(Debug, Clone)]
pub struct Statement {
    kind: StatementKind,
}

impl Statement {
    fn new(kind: StatementKind) -> Self {
        Statement { kind }
    }
}

#[derive(Debug, Clone)]
enum StatementKind {
    Expression(Expression),
    Block(Block),
}

#[derive(Debug, Clone)]
enum SignalType {
    Any,
    RED,
    GREEN,
    BLUE,
    WHITE,
}


#[derive(Debug, Clone)]
enum VariableType {
    Int(SignalType)
}

#[derive(Debug, Clone)]
pub struct Variable {
    name: String,
    variable_type: VariableType,
}

impl Variable {
    fn new(name: String, variable_type: VariableType) -> Self {
        Self { name, variable_type }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    name: String,
    inputs: Vec<Rc<Variable>>,
    outputs: Vec<Rc<Variable>>,
    statements: Vec<Statement>
}

impl Block {
    fn new(name: String, inputs: Vec<Rc<Variable>>, outputs: Vec<Rc<Variable>>, statements: Vec<Statement>) -> Self {
        Self { name, inputs, outputs, statements }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
}

impl Expression {
    fn new(kind: ExpressionKind) -> Self {
        Self { kind }
    }

    fn number(n: u32) -> Self {
        Self { kind: ExpressionKind::Number(NumberExpression::new(n)) }
    }

    fn binary(left: Expression, right: Expression, operator: BinaryOperator) -> Self {
        Self { kind: ExpressionKind::Binary(BinaryExpression::new(Box::new(left), Box::new(right), operator)) }
    }
}

#[derive(Debug, Clone)]
enum ExpressionKind {
    Number(NumberExpression),
    Binary(BinaryExpression),
    Parenthesized(ParenthesizedExpression),
    Variable(Rc<Variable>),
}

#[derive(Debug, Clone)]
pub struct NumberExpression {
    number: u32,
}

impl NumberExpression {
    fn new(number: u32) -> Self {
        Self { number }
    }
}

#[derive(Debug, Clone)]
pub struct ParenthesizedExpression {
    expression: Box<Expression>,
}

impl ParenthesizedExpression {
    fn new(expression: Box<Expression>) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: BinaryOperator,
}

impl BinaryExpression {
    fn new(left: Box<Expression>, right: Box<Expression>, operator: BinaryOperator) -> Self {
        Self { left, right, operator }
    }
}

#[derive(Debug, Clone)]
struct BinaryOperator {
    kind: BinaryOperatorKind,
}

impl BinaryOperator {
    fn new(kind: BinaryOperatorKind) -> Self {
        Self { kind }
    }

    fn precedence(&self) -> u8 {
        match self.kind {
            BinaryOperatorKind::Plus => 1,
            BinaryOperatorKind::Minus => 1,
            BinaryOperatorKind::Multiply => 2,
            BinaryOperatorKind::Divide => 2,
            BinaryOperatorKind::Assign => 0,
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
            BinaryOperatorKind::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, Clone)]
enum BinaryOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    Assign,
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

impl Visitor for Printer {
    fn visit_statement(&mut self, statement: &Statement) {
        self.print("Statement:");
        self.indent();
        self.do_visit_statement(statement);
        self.unindent();
    }

    fn visit_block(&mut self, block: &Block) {
        self.print(&format!("Block: {} {:?} -> {:?}", block.name, block.inputs, block.outputs));
        self.indent();
        self.do_visit_block(&block);
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
}
