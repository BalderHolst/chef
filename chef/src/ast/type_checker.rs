//! Chef type checker.

use crate::{diagnostics::DiagnosticsBagRef, text::TextSpan};

use super::{visitors::Visitor, AST};

const BASE_SIGNALS: &str = include_str!("base.signals");

/// Type check an [AST].
pub fn check(ast: &AST, diagnostics_bag: DiagnosticsBagRef) {
    let mut checker = TypeChecker { diagnostics_bag };
    for statement in &ast.statements {
        checker.visit_statement(statement);
    }
}

/// The type checker.
struct TypeChecker {
    diagnostics_bag: DiagnosticsBagRef,
}

impl TypeChecker {
    /// Checks if a signal is a valid factorio signal.
    fn is_valid_signal(s: &str) -> bool {
        for signal_line in BASE_SIGNALS.lines() {
            let signal = signal_line.split(':').last().unwrap();
            if s == signal {
                return true;
            }
        }
        false
    }

    /// Report error if the signal is invalid.
    fn report_if_invalid(&self, signal: &str, span: &TextSpan) {
        if !Self::is_valid_signal(signal) {
            self.diagnostics_bag
                .borrow_mut()
                .report_error(span, &format!("Invalid factorio signal: `{}`", signal))
            // TODO
        }
    }
}

// TODO: Variable ref
impl Visitor for TypeChecker {
    fn visit_block_link(&mut self, _block: &super::BlockLinkExpression) {}
    fn visit_error_statement(&mut self) {}
    fn visit_number(&mut self, _number: &super::NumberExpression) {}
    fn visit_error_expression(&mut self) {}

    // TODO: report correctly
    fn visit_pick_expression(&mut self, _expr: &super::PickExpression) {
        // self.report_if_invalid(expr.pick_signal.as_str());
    }

    fn visit_block(&mut self, block: &super::Block) {
        for var in &block.inputs {
            if let super::VariableType::Int(signal) = &var.type_ {
                self.report_if_invalid(signal, &var.definition);
            }
        }
        for var_type in &block.outputs {
            if let super::VariableType::Int(signal) = var_type {
                self.report_if_invalid(signal, &block.span);
            }
        }
        self.do_visit_block(block);
    }

    fn visit_variable(&mut self, var: &super::Variable) {
        if let super::VariableType::Int(signal) = &var.type_ {
            self.report_if_invalid(signal, &var.definition);
        }
    }
}
