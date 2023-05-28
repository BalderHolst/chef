use std::{fmt::Display, fs::write};
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

    fn visit_number(&mut self, number: &NumberExpression);

    fn visit_parenthesized_expression(&mut self, expr: &ParenthesizedExpression) {
        self.visit_expression(&expr.expression);
    }

    fn visit_binary_expression(&mut self, binary_expression: &BinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }
}

#[derive(Debug)]
pub struct Statement {
    kind: StatementKind,
}

impl Statement {
    fn new(kind: StatementKind) -> Self {
        Statement { kind }
    }
}

#[derive(Debug)]
enum StatementKind {
    Expression(Expression),
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
enum BinaryOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
}

const INDENTATON: usize = 2;

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

    fn visit_expression(&mut self, expression: &Expression) {
        self.print("Expression:");
        self.indent();
        self.do_visit_expression(expression);
        self.unindent();
    }

    fn visit_number(&mut self, number: &NumberExpression) {
        self.print(&format!("Number: {}", number.number));
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
        self.print("Parenthesized");
        self.indent();
        self.visit_expression(&expr.expression);
        self.unindent();
    }
}
