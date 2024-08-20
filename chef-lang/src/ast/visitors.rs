//! Traits for traversing the abstract syntax tree.
//!
//! This module contains the traits [Visitor] and [MutVisitor]. Both of these traits allow a struct to
//! traverse the [AST]. The [Visitor] trait is for read-only traversals, while the [MutVisitor] trait
//! is for traversals that may modify the [AST].
//!
//! These traits are generated by the [make_visitor] macro as they share implementation apart from
//! the type of to reference to [AST] nodes.

#![allow(unused)]

use super::{
    BinaryExpression, Block, BlockLinkExpression, Declaration, DeclarationDefinition, Definition,
    DelayExpression, Directive, DynBlock, Expression, ExpressionKind, GateExpression, Import,
    IndexExpression, NegativeExpression, ParenthesizedExpression, PickExpression, SizeOfExpression,
    Statement, StatementKind, TupleDeclarationDefinition, Variable, VariableRef, WhenStatement,
};

// For documentation references
#[allow(unused_imports)]
use super::AST;

/// Macro for generating the [Visitor] and [MutVisitor] traits.
macro_rules! make_visitor {
    {doc: $doc:literal, name: $name:ident, ref: $($ref:tt)+} => {
        #[doc = $doc]
        pub trait $name<V>
        where
            V: Variable
        {

            fn visit_directive(&mut self, directive: $($ref)+ Directive<V>) {
                self.do_visit_directive(directive);
            }
            fn do_visit_directive(&mut self, directive: $($ref)+ Directive<V>) {
                match directive {
                    Directive::Block(block) => {
                        self.visit_block(block);
                    }
                    Directive::Import(import) => {
                        self.visit_import(import);
                    }
                    Directive::DynBlock(dyn_block) => self.visit_dyn_block(dyn_block),
                    Directive::Constant => self.visit_constant(),
                    Directive::Unknown => self.visit_unknown(),
                }
            }

            fn visit_import(&mut self, import: $($ref)+ Import<V>) {
                self.do_visit_import(import);
            }
            fn do_visit_import(&mut self, import: $($ref)+ Import<V>) {}

            fn visit_dyn_block(&mut self, dyn_block: $($ref)+ DynBlock<V>) {
                self.do_visit_dyn_block(dyn_block);
            }
            fn do_visit_dyn_block(&mut self, dyn_block: $($ref)+ DynBlock<V>) {
                for version in $($ref)+ dyn_block.versions {
                    self.visit_block(version);
                }
            }

            fn visit_constant(&mut self) { }

            fn visit_unknown(&mut self) { }

            fn visit_block(&mut self, block: $($ref)+ Block<V>) {
                self.do_visit_block(block);
            }
            fn do_visit_block(&mut self, block: $($ref)+ Block<V>) {
                for input in $($ref)+ block.inputs {
                    self.visit_variable(input);
                }
                for output in $($ref)+ block.outputs {
                    self.visit_variable(output);
                }
                for statement in $($ref)+ block.statements {
                    self.visit_statement(statement);
                }
            }

            fn visit_statement(&mut self, statement: $($ref)+ Statement<V>) {
                self.do_visit_statement(statement);
            }
            fn do_visit_statement(&mut self, statement: $($ref)+ Statement<V>) {
                match $($ref)+ statement.kind {
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
                    StatementKind::TupleDeclarationDefinition(tuple_dec_def) => {
                        self.visit_tuple_definition_declaration_statement(tuple_dec_def);
                    }
                }
            }

            fn visit_expression(&mut self, expression: $($ref)+ Expression<V>) {
                self.do_visit_expression(expression);
            }
            fn do_visit_expression(&mut self, expression: $($ref)+ Expression<V>) {
                match $($ref)+ expression.kind {
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
                        self.visit_block_link_expression(block);
                    }
                    ExpressionKind::Delay(delay) => {
                        self.visit_delay_expression(delay);
                    }
                    ExpressionKind::SizeOf(expr) => {
                        self.visit_size_of_expression(expr);
                    }
                    ExpressionKind::Gate(expr) => {
                        self.visit_gate_expression(expr);
                    }
                }
            }

            fn visit_declaration_definition(&mut self, dec_def: $($ref)+ DeclarationDefinition<V>) {
                self.do_visit_declaration_definition(dec_def);
            }
            fn do_visit_declaration_definition(&mut self, dec_def: $($ref)+ DeclarationDefinition<V>) {
                self.visit_variable($($ref)+ dec_def.variable);
                self.visit_expression($($ref)+ dec_def.expression);
            }

            fn visit_declaration(&mut self, dec: $($ref)+ Declaration<V>) {
                self.do_visit_declaration(dec);
            }
            fn do_visit_declaration(&mut self, dec: $($ref)+ Declaration<V>) {
                self.visit_variable($($ref)+ dec.variable);
            }

            fn visit_definition(&mut self, def: $($ref)+ Definition<V>) {
                self.do_visit_definition(def);
            }
            fn do_visit_definition(&mut self, def: $($ref)+ Definition<V>) {
                self.visit_expression($($ref)+ def.expression);
            }

            fn visit_size_of_expression(&mut self, size: $($ref)+ SizeOfExpression<V>) {
                self.do_visit_size_of_expression(size);
            }
            fn do_visit_size_of_expression(&mut self, size: $($ref)+ SizeOfExpression<V>) {
                self.visit_expression($($ref)+ size.expression);
            }

            fn visit_gate_expression(&mut self, gate: $($ref)+ GateExpression<V>) {
                self.do_visit_gate_expression(gate);
            }
            fn do_visit_gate_expression(&mut self, gate: $($ref)+ GateExpression<V>) {
                self.visit_expression($($ref)+ gate.gated_expr);
                self.visit_expression($($ref)+ gate.left);
                self.visit_expression($($ref)+ gate.right);
            }

            fn visit_when_statement(&mut self, when_statement: $($ref)+ WhenStatement<V>) {
                self.do_visit_when_statement(when_statement);
            }
            fn do_visit_when_statement(&mut self, when: $($ref)+ WhenStatement<V>) {
                self.visit_expression($($ref)+ when.condition);
                for statement in $($ref)+ when.statements {
                    self.visit_statement(statement);
                }
            }

            fn visit_tuple_definition_declaration_statement(&mut self, tuple_dec_def: $($ref)+ TupleDeclarationDefinition<V>) {
                self.do_visit_tuple_declaration_definition_statement(tuple_dec_def);
            }
            fn do_visit_tuple_declaration_definition_statement(&mut self, tuple_dec_def: $($ref)+ TupleDeclarationDefinition<V>) {
                for def in $($ref)+ tuple_dec_def.defs {
                    self.visit_variable($($ref)+ def.variable);
                    self.visit_variable($($ref)+ def.block_variable);
                }
                self.visit_block_link_expression($($ref)+ tuple_dec_def.block_link);
            }

            fn visit_number(&mut self, n: $($ref)+ i32) {
                self.do_visit_number(n);
            }
            fn do_visit_number(&mut self, n: $($ref)+ i32) {}

            fn visit_bool(&mut self, b: $($ref)+ bool) {
                self.do_visit_bool(b);
            }
            fn do_visit_bool(&mut self, b: $($ref)+ bool) {}

            fn visit_variable(&mut self, var: $($ref)+ std::rc::Rc<V>) {
                self.do_visit_variable(var);
            }
            fn do_visit_variable(&mut self, var: $($ref)+ std::rc::Rc<V>) {}

            fn visit_variable_ref(&mut self, var: $($ref)+ VariableRef<V>) {
                self.do_visit_variable_ref(var);
            }
            fn do_visit_variable_ref(&mut self, var: $($ref)+ VariableRef<V>) {
                self.visit_variable($($ref)+ var.var);
            }

            fn visit_binary_expression(&mut self, bin_expr: $($ref)+ BinaryExpression<V>) {
                self.do_visit_binary_expression(bin_expr);
            }
            fn do_visit_binary_expression(&mut self, bin_expr: $($ref)+ BinaryExpression<V>) {
                self.visit_expression($($ref)+ bin_expr.left);
                self.visit_expression($($ref)+ bin_expr.right);
            }

            fn visit_parenthesized_expression(&mut self, paren_expr: $($ref)+ ParenthesizedExpression<V>) {
                self.do_visit_parenthesized_expression(paren_expr);
            }
            fn do_visit_parenthesized_expression(&mut self, paren_expr: $($ref)+ ParenthesizedExpression<V>) {
                self.visit_expression($($ref)+ paren_expr.expression);
            }

            fn visit_negative_expression(&mut self, neg_expr: $($ref)+ NegativeExpression<V>) {
                self.do_visit_negative_expression(neg_expr);
            }
            fn do_visit_negative_expression(&mut self, neg_expr: $($ref)+ NegativeExpression<V>) {
                self.visit_expression($($ref)+ neg_expr.expression);
            }

            fn visit_pick_expression(&mut self, expr: $($ref)+ PickExpression<V>) {
                self.do_visit_pick_expression(expr);
            }
            fn do_visit_pick_expression(&mut self, pick_expr: $($ref)+ PickExpression<V>) {
                self.visit_variable_ref($($ref)+ pick_expr.from);
            }

            fn visit_index_expression(&mut self, expr: $($ref)+ IndexExpression<V>) {
                self.do_visit_index_expression(expr);
            }
            fn do_visit_index_expression(&mut self, index_expr: $($ref)+ IndexExpression<V>) {
                self.visit_variable_ref($($ref)+ index_expr.var_ref);
            }

            fn visit_block_link_expression(&mut self, link: $($ref)+ BlockLinkExpression<V>) {
                self.do_visit_block_link_expression(link);
            }
            fn do_visit_block_link_expression(&mut self, link_expr: $($ref)+ BlockLinkExpression<V>) {
                for input in $($ref)+ link_expr.inputs {
                    self.visit_expression(input);
                }
            }

            fn visit_delay_expression(&mut self, delay: $($ref)+ DelayExpression<V>) {
                self.do_visit_delay_expression(delay);
            }
            fn do_visit_delay_expression(&mut self, delay: $($ref)+ DelayExpression<V>) {
                self.visit_expression($($ref)+ delay.expression);
            }
        }
    }
}

// Generate trait implementations for the `Visitor` and `MutVisitor` traits
make_visitor! {
    doc: "Allows a struct to *immutably* traverse the chef [AST].",
    name: Visitor,
    ref: &
}
make_visitor! {
    doc: "Allows a struct to *mutably* traverse the chef [AST].",
    name: MutVisitor,
    ref: &mut
}
