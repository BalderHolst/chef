//!  Traits for traversing the abstract syntax tree.

// This file constains the traits Visitor and MutVisitor, both of these traits allow a struct to
// traverse the AST. Both clases have almost identical code, the only difference being the `mut`
// keyword. This is less than optimal, but i have no ideas how to write macros, or if they could
// even solve this. Suggestions very welcome!

use super::{
    parser::StatementList, Assignment, BinaryExpression, Block, BlockLinkExpression, Expression,
    ExpressionKind, Mutation, ParenthesizedExpression, PickExpression, Statement, StatementKind,
    VariableRef, WhenExpression, IndexExpression,
};

// For documentation references
#[allow(unused_imports)]
use super::AST;

/// Trait allowing for traversal of an immutable [AST].
pub trait Visitor {
    fn do_visit_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Expression(expr) => {
                self.visit_expression_statement(expr);
            }
            StatementKind::Out(expr) => {
                self.visit_out(expr);
            }
            StatementKind::Assignment(assignment) => {
                self.visit_assignment(assignment);
            }
            StatementKind::Mutation(mutation) => {
                self.visit_mutation(mutation);
            }
            StatementKind::Error => {
                self.visit_error_statement();
            }
        }
    }

    fn do_visit_expression(&mut self, expression: &Expression) {
        match &expression.kind {
            ExpressionKind::Int(number) => {
                self.visit_number(number);
            }
            ExpressionKind::Bool(bool) => {
                self.visit_bool(bool);
            }
            ExpressionKind::VariableRef(variable) => {
                self.visit_variable_ref(variable);
            }
            ExpressionKind::Binary(expr) => {
                self.visit_binary_expression(expr);
            }
            ExpressionKind::Parenthesized(expr) => {
                self.visit_parenthesized_expression(expr);
            }
            ExpressionKind::Negative(expr) => {
                self.visit_negative_expression(expr);
            }
            ExpressionKind::Pick(expr) => {
                self.visit_pick_expression(expr);
            }
            ExpressionKind::Index(expr) => {
                self.visit_index_expression(expr);
            }
            ExpressionKind::BlockLink(block) => {
                self.visit_block_link(block);
            }
            ExpressionKind::When(when) => {
                self.visit_when_expression(when);
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
        self.visit_expression(&block.output);
    }

    fn do_visit_assignment(&mut self, assignment: &Assignment) {
        self.visit_expression(&assignment.expression)
    }

    fn do_visit_mutation(&mut self, mutation: &Mutation) {
        self.visit_variable_ref(&mutation.var_ref);
        self.visit_expression(&mutation.expression);
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

    fn visit_negative_expression(&mut self, expr: &Expression) {
        self.visit_expression(expr);
    }

    fn visit_expression_statement(&mut self, expr: &Expression) {
        self.visit_expression(expr);
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

    fn visit_mutation(&mut self, mutation: &Mutation) {
        self.do_visit_mutation(mutation);
    }

    fn visit_block_link(&mut self, block: &BlockLinkExpression) {
        for expr in &block.inputs {
            self.visit_expression(expr);
        }
    }

    fn visit_when_expression(&mut self, when: &WhenExpression) {
        self.visit_expression(&when.condition);
        for statement in &when.statements {
            self.visit_statement(statement);
        }
        if let Some(out) = &when.out {
            self.visit_expression(out);
        }
    }

    fn visit_statement_list(&mut self, sl: &StatementList) {
        for s in &sl.statements {
            self.visit_statement(s);
        }
        if let Some(out_expr) = &sl.out {
            self.visit_expression(out_expr);
        }
    }

    fn visit_pick_expression(&mut self, expr: &PickExpression);
    fn visit_index_expression(&mut self, expr: &IndexExpression);
    fn visit_error_statement(&mut self);
    fn visit_number(&mut self, number: &i32);
    fn visit_bool(&mut self, value: &bool);
    fn visit_variable_ref(&mut self, var: &VariableRef);
    fn visit_error_expression(&mut self);
}

/// Trait allowing for traversal of a mutable [AST].
pub trait MutVisitor {
    fn do_visit_statement(&mut self, statement: &mut Statement) {
        match &mut statement.kind {
            StatementKind::Expression(expr) => {
                self.visit_expression_statement(expr);
            }
            StatementKind::Out(expr) => {
                self.visit_out(expr);
            }
            StatementKind::Assignment(assignment) => {
                self.visit_assignment(assignment);
            }
            StatementKind::Mutation(mutation) => {
                self.visit_mutation(mutation);
            }
            StatementKind::Error => {
                self.visit_error_statement();
            }
        }
    }

    fn do_visit_expression(&mut self, expression: &mut Expression) {
        match &mut expression.kind {
            ExpressionKind::Int(number) => {
                self.visit_number(number);
            }
            ExpressionKind::Bool(bool) => {
                self.visit_bool(bool);
            }
            ExpressionKind::VariableRef(variable) => {
                self.visit_variable_ref(variable);
            }
            ExpressionKind::Binary(expr) => {
                self.visit_binary_expression(expr);
            }
            ExpressionKind::Parenthesized(expr) => {
                self.visit_parenthesized_expression(expr);
            }
            ExpressionKind::Negative(expr) => {
                self.visit_negative_expression(expr);
            }
            ExpressionKind::Pick(expr) => {
                self.visit_pick_expression(expr);
            }
            ExpressionKind::Index(expr) => {
                self.visit_index_expression(expr);
            }
            ExpressionKind::BlockLink(block) => {
                self.visit_block_link(block);
            }
            ExpressionKind::When(when) => {
                self.visit_when_expression(when);
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
        self.visit_expression(&mut block.output);
    }

    fn do_visit_assignment(&mut self, assignment: &mut Assignment) {
        self.visit_expression(&mut assignment.expression)
    }

    fn do_visit_mutation(&mut self, mutation: &mut Mutation) {
        self.visit_variable_ref(&mutation.var_ref);
        self.visit_expression(&mut mutation.expression);
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

    fn visit_negative_expression(&mut self, expr: &mut Box<Expression>) {
        self.visit_expression(expr);
    }

    fn visit_expression_statement(&mut self, expr: &mut Expression) {
        self.visit_expression(expr);
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

    fn visit_mutation(&mut self, mutation: &mut Mutation) {
        self.do_visit_mutation(mutation);
    }

    fn visit_block_link(&mut self, block: &mut BlockLinkExpression) {
        for expr in &mut block.inputs {
            self.visit_expression(expr);
        }
    }

    fn visit_when_expression(&mut self, when: &mut WhenExpression) {
        self.visit_expression(&mut when.condition);
        for statement in &mut when.statements {
            self.visit_statement(statement);
        }
        if let Some(out) = &mut when.out {
            self.visit_expression(out);
        }
    }

    fn visit_statement_list(&mut self, sl: &mut StatementList) {
        for s in &mut sl.statements {
            self.visit_statement(s);
        }
        if let Some(out_expr) = &mut sl.out {
            self.visit_expression(out_expr);
        }
    }

    fn visit_pick_expression(&mut self, expr: &mut PickExpression);
    fn visit_index_expression(&mut self, expr: &mut IndexExpression);
    fn visit_error_statement(&mut self);
    fn visit_bool(&mut self, bool: &mut bool);
    fn visit_number(&mut self, number: &mut i32);
    fn visit_variable_ref(&mut self, var: &VariableRef);
    fn visit_error_expression(&mut self);
}
