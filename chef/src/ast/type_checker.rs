//! Chef type checker.

use crate::{diagnostics::DiagnosticsBagRef, text::TextSpan};

use super::{
    visitors::Visitor, ExpressionKind, ExpressionReturnType, StatementKind, VariableSignalType, AST,
};

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
    fn report_if_invalid_signal(&self, signal: &str, span: &TextSpan) {
        if !Self::is_valid_signal(signal) {
            self.diagnostics_bag
                .borrow_mut()
                .report_error(span, &format!("Invalid factorio signal: `{}`", signal))
        }
    }
}

// TODO: Variable ref
impl Visitor for TypeChecker {
    fn visit_block_link(&mut self, _block: &super::BlockLinkExpression) {}
    fn visit_error_statement(&mut self) {}
    fn visit_number(&mut self, _number: &super::IntExpression) {}
    fn visit_bool(&mut self, _value: &bool) {}
    fn visit_error_expression(&mut self) {}
    fn visit_pick_expression(&mut self, _expr: &super::PickExpression) {}

    fn visit_expression(&mut self, expression: &super::Expression) {
        match &expression.kind {
            ExpressionKind::Bool(_) => {}
            ExpressionKind::Int(_) => {}
            ExpressionKind::Parenthesized(_) => {}
            ExpressionKind::Variable(_) => {}
            ExpressionKind::BlockLink(_) => {}
            ExpressionKind::Error => {}
            ExpressionKind::Pick(e) => {
                self.report_if_invalid_signal(e.pick_signal.as_str(), &expression.span)
            }
            ExpressionKind::Binary(b_expr) => {
                let left_r = b_expr.left.return_type();
                if left_r != ExpressionReturnType::Int {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &expression.span,
                        &format!("Left side of expression must be `int` not `{}`.", left_r),
                    )
                }
                let rigth_r = b_expr.right.return_type();
                if rigth_r != ExpressionReturnType::Int {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &expression.span,
                        &format!("Right side of expression must be `int` not `{}`.", rigth_r),
                    )
                }
            }
        };
        self.do_visit_expression(expression);
    }

    fn visit_statement(&mut self, statement: &super::Statement) {
        // Make sure variables are only assign expressions returning their type
        if let StatementKind::Assignment(assignment) = &statement.kind {
            let var_type = assignment.variable.return_type();
            let expr_type = assignment.expression.return_type();
            if var_type != expr_type {
                self.diagnostics_bag.borrow_mut().report_error(
                    &statement.span,
                    &format!("Can not assign variable `{}` of type `{}` to expression returning `{}` type.",
                             assignment.variable.name, var_type, expr_type
                             ),
                    )
            }
        }

        self.do_visit_statement(statement);
    }

    fn visit_block(&mut self, block: &super::Block) {
        // Make sure block input signals are valid factorio signals
        for var in &block.inputs {
            if let super::VariableType::Int(VariableSignalType::Signal(signal)) = &var.type_ {
                self.report_if_invalid_signal(signal, &var.definition);
            }
        }
        if let super::VariableType::Int(VariableSignalType::Signal(signal)) = &block.output {
            self.report_if_invalid_signal(signal, &block.span);
        }

        self.do_visit_block(block);
    }

    fn visit_variable(&mut self, var: &super::Variable) {
        if let super::VariableType::Int(VariableSignalType::Signal(signal)) = &var.type_ {
            self.report_if_invalid_signal(signal, &var.definition);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_assignment_types() {
        let (_, bag) = AST::from_str("a:int = false;");
        let m_bag = bag.borrow_mut();
        assert!(m_bag.error_count() == 1);
        let message = &format!("{:?}", m_bag.diagnostics()[0]);
        assert_eq!(message, "Diagnostic { message: \"Can not assign variable `a` of type `int` to expression returning `bool` type.\", span: TextSpan { start: 0, end: 14, text: SourceText { file: None, text: \"a:int = false;\", lines: [0] } } }");
    }

    #[test]
    fn check_expression_types() {
        let (_, bag) = AST::from_str("b:int = 5 + false * 10;");
        let m_bag = bag.borrow_mut();
        assert!(m_bag.error_count() == 1);
        let message = &format!("{:?}", m_bag.diagnostics()[0]);
        assert_eq!(message, "Diagnostic { message: \"Left side of expression must be `int` not `bool`.\", span: TextSpan { start: 12, end: 22, text: SourceText { file: None, text: \"b:int = 5 + false * 10;\", lines: [0] } } }");
    }
}
