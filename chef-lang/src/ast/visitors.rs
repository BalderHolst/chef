//!  Traits for traversing the abstract syntax tree.

// This file constains the traits Visitor and MutVisitor, both of these traits allow a struct to
// traverse the AST. Both clases have almost identical code, the only difference being the `mut`
// keyword. This is less than optimal, but i have no ideas how to write macros, or if they could
// even solve this. Suggestions very welcome!

use super::{
    parser::StatementList, BinaryExpression, Block, BlockLinkExpression, Declaration,
    DeclarationDefinition, Definition, DelayExpression, Expression, ExpressionKind,
    IndexExpression, ParenthesizedExpression, PickExpression, SizeOfExpression, Statement,
    StatementKind, TupleDefinitionDeclaration, VariableRef, WhenStatement,
};

// For documentation references
#[allow(unused_imports)]
use super::AST;

/// Trait allowing for traversal of an immutable [AST].
pub trait Visitor {
    fn do_visit_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Declaration(declaration) => {
                self.visit_declaration(declaration);
            }
            StatementKind::DeclarationDefinition(dec_def) => {
                self.visit_declaration_definition(dec_def);
            }
            StatementKind::Definition(def) => {
                self.visit_definition(def);
            }
            StatementKind::When(when) => {
                self.visit_when_statement(when);
            }
            StatementKind::TupleDefinitionDeclaration(tuple_dec_def) => {
                self.visit_tuple_definition_declaration_statement(tuple_dec_def);
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
            ExpressionKind::Delay(delay) => {
                self.visit_delay(delay);
            }
            ExpressionKind::SizeOf(expr) => {
                self.visit_size_of(expr);
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

    fn do_visit_declaration_definition(&mut self, assignment: &DeclarationDefinition) {
        let expr = &assignment.expression;
        self.visit_expression(expr)
    }

    fn do_visit_definition(&mut self, assignment: &Definition) {
        self.visit_expression(&assignment.expression)
    }

    fn do_visit_size_of(&mut self, expr: &SizeOfExpression) {
        self.visit_expression(&expr.expression);
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

    fn visit_declaration(&mut self, _declaration: &Declaration) {}

    fn visit_declaration_definition(&mut self, dec_def: &DeclarationDefinition) {
        self.do_visit_declaration_definition(dec_def);
    }

    fn visit_definition(&mut self, def: &Definition) {
        self.do_visit_definition(def);
    }

    fn visit_block_link(&mut self, block: &BlockLinkExpression) {
        for expr in &block.inputs {
            self.visit_expression(expr);
        }
    }

    fn visit_delay(&mut self, delay: &DelayExpression) {
        self.visit_expression(&delay.expression);
    }

    fn visit_size_of(&mut self, expr: &SizeOfExpression) {
        self.do_visit_size_of(expr);
    }

    fn visit_when_statement(&mut self, when: &WhenStatement) {
        self.visit_expression(&when.condition);
        for statement in &when.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_tuple_definition_declaration_statement(
        &mut self,
        tuple_dec_def: &TupleDefinitionDeclaration,
    ) {
        self.visit_block_link(&tuple_dec_def.block_link)
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
            StatementKind::Declaration(dec) => {
                self.visit_declaration(dec);
            }
            StatementKind::DeclarationDefinition(dec_def) => {
                self.visit_declaration_definition(dec_def);
            }
            StatementKind::Definition(def) => {
                self.visit_definition(def);
            }
            StatementKind::When(when) => {
                self.visit_when_statement(when);
            }
            StatementKind::TupleDefinitionDeclaration(tuple_dec_def) => {
                self.visit_tuple_definition_declaration_statement(tuple_dec_def);
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
            ExpressionKind::Delay(delay) => {
                self.visit_delay(delay);
            }
            ExpressionKind::SizeOf(expr) => {
                self.visit_size_of(expr);
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

    fn do_visit_declaration_definition(&mut self, dec_def: &mut DeclarationDefinition) {
        let expr = &mut dec_def.expression;
        self.visit_expression(expr);
    }

    fn do_visit_definition(&mut self, def: &mut Definition) {
        self.visit_expression(&mut def.expression);
    }

    fn do_visit_size_of(&mut self, expr: &mut SizeOfExpression) {
        self.visit_expression(&mut expr.expression);
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

    fn visit_declaration(&mut self, _declaration: &mut Declaration) {}

    fn visit_declaration_definition(&mut self, dec_def: &mut DeclarationDefinition) {
        self.do_visit_declaration_definition(dec_def);
    }

    fn visit_definition(&mut self, def: &mut Definition) {
        self.do_visit_definition(def);
    }

    fn visit_block_link(&mut self, block: &mut BlockLinkExpression) {
        for expr in &mut block.inputs {
            self.visit_expression(expr);
        }
    }

    fn visit_delay(&mut self, delay: &mut DelayExpression) {
        self.visit_expression(&mut delay.expression);
    }

    fn visit_size_of(&mut self, expr: &mut SizeOfExpression) {
        self.do_visit_size_of(expr);
    }

    fn visit_when_statement(&mut self, when: &mut WhenStatement) {
        self.visit_expression(&mut when.condition);
        for statement in &mut when.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_tuple_definition_declaration_statement(
        &mut self,
        tuple_dec_def: &mut TupleDefinitionDeclaration,
    ) {
        self.visit_block_link(&mut tuple_dec_def.block_link)
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
