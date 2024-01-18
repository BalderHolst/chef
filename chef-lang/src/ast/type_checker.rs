//! Chef type checker.

// TODO: Check that blocks are called with the correct arguments
// TODO: Check for reserved signals. See RESERVED_GATE_SIGNAL.

use crate::{diagnostics::DiagnosticsBagRef, text::TextSpan, utils::BASE_SIGNALS};

use super::{
    visitors::Visitor, ExpressionKind, ExpressionReturnType, StatementKind, VariableSignalType, AST,
};

/// Type check an [AST].
pub fn check(ast: &AST, diagnostics_bag: DiagnosticsBagRef) {
    let mut checker = TypeChecker { diagnostics_bag };
    for block in &ast.blocks {
        checker.visit_block(block);
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
    fn visit_number(&mut self, _number: &i32) {}
    fn visit_bool(&mut self, _value: &bool) {}
    fn visit_error_expression(&mut self) {}
    fn visit_pick_expression(&mut self, _expr: &super::PickExpression) {}
    fn visit_variable_ref(&mut self, _var: &super::VariableRef) {}
    fn visit_index_expression(&mut self, _expr: &super::IndexExpression) {}

    fn visit_assignment(&mut self, assignment: &super::Assignment) {
        if let Some(sig) = assignment.variable.type_.signal() {
            self.report_if_invalid_signal(sig.as_str(), &assignment.variable.span)
        }
        self.do_visit_assignment(assignment);
    }

    // TODO: Maybe break this up into the unimplemented functions above
    fn visit_expression(&mut self, expression: &super::Expression) {
        match &expression.kind {
            ExpressionKind::Bool(_) => {}
            ExpressionKind::Int(_) => {}
            ExpressionKind::Parenthesized(_) => {}
            ExpressionKind::Negative(_) => {}
            ExpressionKind::VariableRef(_) => {}
            ExpressionKind::BlockLink(_) => {}
            ExpressionKind::Error => {}
            ExpressionKind::Index(index_expr) => {
                match &index_expr.var.type_ {
                    super::VariableType::Register(reg_size) => {
                        if index_expr.size >= *reg_size {
                            self.diagnostics_bag.borrow_mut().report_error(&expression.span, &format!("Index {n} of is out of range of register with size {reg_size}.", n=index_expr.size))
                        }
                    },
                    _ => self.diagnostics_bag.borrow_mut().report_error(&expression.span, "Only variables of type 'register' can be indexed.")
                }
            }
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
            ExpressionKind::When(e) => {
                let cond_type = e.condition.return_type();
                if cond_type != ExpressionReturnType::Bool {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &expression.span,
                        &format!(
                            "`when` conditions should be `{}` not `{}`.",
                            ExpressionReturnType::Bool,
                            cond_type
                        ),
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
                self.report_if_invalid_signal(signal, &var.span);
            }
        }

        if let Some(output_signal) = block.output_type.signal() {
            // Make sure output signal is a valid factorio signal
            self.report_if_invalid_signal(output_signal.as_str(), &block.span);
        }

        let block_return_type = block.output_type.return_type();
        let block_out_expr_type = block.output.return_type();
        if block_out_expr_type != block_return_type {
            self.diagnostics_bag.borrow_mut()
                .report_error(
                    &block.output.span,
                    &format!(
                        "Block output expression type '{}' does not correspond to defined block output type '{}'.",
                        block_out_expr_type,
                        block_return_type
                        )
                    )
        }

        self.do_visit_block(block);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn check_return_types() {
        let (_, bag) = AST::from_str(
            "
            block main() -> bool {
                10
            }
            ",
        );
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert_eq!(m_bag.error_count(), 1);
        let message = format!("{:?}", m_bag.diagnostics()[0].message());
        dbg!(&message);
        assert_eq!(message,"\"Block output expression type 'int' does not correspond to defined block output type 'bool'.\"");
    }

    #[test]
    fn check_assignment_types() {
        let (_, bag) = AST::from_str(
            "
            block main() -> bool {
                a: int = false;
                a
            }
            ",
        );
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert_eq!(m_bag.error_count(), 2);
        let message = &format!("{:?}", m_bag.diagnostics()[1]);
        assert_eq!(message, "Localized { message: \"Expected `}` but found `word`.\", span: TextSpan { start: 84, end: 85, text: SourceText { file: None, text: \"\\n            block main() -> bool {\\n                a: int = false;\\n                a\\n            }\\n            \", lines: [0, 0, 1, 36, 68, 86, 100] } } }");
    }

    #[test]
    fn check_expression_types() {
        let (_, bag) = AST::from_str(
            "
        block main() -> int {
            b: int = 5 + false * 10;
            b
        }
        ",
        );
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert!(m_bag.error_count() == 1);
        let message = &format!("{:?}", m_bag.diagnostics()[0]);
        assert_eq!(message,"Localized { message: \"Left side of expression must be `int` not `bool`.\", span: TextSpan { start: 56, end: 66, text: SourceText { file: None, text: \"\\n        block main() -> int {\\n            b: int = 5 + false * 10;\\n            b\\n        }\\n        \", lines: [0, 0, 1, 31, 68, 82, 92] } } }");
    }
}
