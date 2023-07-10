//!  Traits for traversing the abstract syntax tree.

// This file constains the traits Visitor and MutVisitor, both of these traits allow a struct to
// traverse the AST. Both clases have almost identical code, the only difference being the `mut`
// keyword. This is less than optimal, but i have no ideas how to write macros, or if they could
// even solve this. Suggestions very welcome!

use super::{
    Assignment, BinaryExpression, Block, BlockLinkExpression, Expression, ExpressionKind,
    NumberExpression, ParenthesizedExpression, PickExpression, Statement, StatementKind, Variable,
};

/// Trait allowing for traversal of an immutable [AST].
pub trait Visitor {
    fn do_visit_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Block(block) => {
                self.visit_block(block);
            }
            StatementKind::Out(expr) => {
                self.visit_out(expr);
            }
            StatementKind::Assignment(assignment) => {
                self.visit_assignment(assignment);
            }
            StatementKind::Error => {
                self.visit_error_statement();
            }
        }
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
            ExpressionKind::Pick(expr) => {
                self.visit_pick_expression(expr);
            }
            ExpressionKind::BlockLink(block) => {
                self.visit_block_link(block);
            }
            ExpressionKind::Error => {
                self.visit_error_expression();
            }
        }
    }

    fn do_visit_block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.visit_statement(statement);
        }
    }

    fn do_visit_assignment(&mut self, assignment: &Assignment) {
        self.visit_expression(&assignment.expression)
    }

    fn visit_statement(&mut self, statement: &Statement) {
        self.do_visit_statement(statement);
    }

    fn visit_expression(&mut self, expression: &Expression) {
        self.do_visit_expression(expression);
    }

    fn visit_binary_expression(&mut self, binary_expression: &BinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }

    fn visit_parenthesized_expression(&mut self, expr: &ParenthesizedExpression) {
        self.visit_expression(&expr.expression);
    }

    fn visit_block(&mut self, block: &Block) {
        self.do_visit_block(block);
    }

    fn visit_out(&mut self, expr: &Expression) {
        self.visit_expression(expr);
    }

    fn visit_assignment(&mut self, assignment: &Assignment) {
        self.do_visit_assignment(assignment);
    }

    fn visit_block_link(&mut self, block: &BlockLinkExpression);
    fn visit_pick_expression(&mut self, expr: &PickExpression);
    fn visit_error_statement(&mut self);
    fn visit_number(&mut self, number: &NumberExpression);
    fn visit_variable(&mut self, var: &Variable);
    fn visit_error_expression(&mut self);
}

/// Trait allowing for traversal of a mutable [AST].
pub trait MutVisitor {
    fn do_visit_statement(&mut self, statement: &mut Statement) {
        match &mut statement.kind {
            StatementKind::Block(block) => {
                self.visit_block(block);
            }
            StatementKind::Out(expr) => {
                self.visit_out(expr);
            }
            StatementKind::Assignment(assignment) => {
                self.visit_assignment(assignment);
            }
            StatementKind::Error => {
                self.visit_error_statement();
            }
        }
    }

    fn do_visit_expression(&mut self, expression: &mut Expression) {
        match &mut expression.kind {
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
            ExpressionKind::Pick(expr) => {
                self.visit_pick_expression(expr);
            }
            ExpressionKind::BlockLink(block) => {
                self.visit_block_link(block);
            }
            ExpressionKind::Error => {
                self.visit_error_expression();
            }
        }
    }

    fn do_visit_block(&mut self, block: &mut Block) {
        for statement in &mut block.statements {
            self.visit_statement(statement);
        }
    }

    fn do_visit_assignment(&mut self, assignment: &mut Assignment) {
        self.visit_expression(&mut assignment.expression)
    }

    fn visit_statement(&mut self, statement: &mut Statement) {
        self.do_visit_statement(statement);
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        self.do_visit_expression(expression);
    }

    fn visit_binary_expression(&mut self, binary_expression: &mut BinaryExpression) {
        self.visit_expression(&mut binary_expression.left);
        self.visit_expression(&mut binary_expression.right);
    }

    fn visit_parenthesized_expression(&mut self, expr: &mut ParenthesizedExpression) {
        self.visit_expression(&mut expr.expression);
    }

    fn visit_block(&mut self, block: &mut Block) {
        self.do_visit_block(block);
    }

    fn visit_out(&mut self, expr: &mut Expression) {
        self.visit_expression(expr);
    }

    fn visit_assignment(&mut self, assignment: &mut Assignment) {
        self.do_visit_assignment(assignment);
    }

    fn visit_block_link(&mut self, block: &mut BlockLinkExpression);
    fn visit_pick_expression(&mut self, expr: &mut PickExpression);
    fn visit_error_statement(&mut self);
    fn visit_number(&mut self, number: &mut NumberExpression);
    fn visit_variable(&mut self, var: &Variable);
    fn visit_error_expression(&mut self);
}
