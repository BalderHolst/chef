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

    fn visit_declaration_definition(&mut self, assignment: &super::DeclarationDefinition) {
        if let Some(sig) = assignment.variable.type_.signal() {
            self.report_if_invalid_signal(sig.as_str(), &assignment.variable.span)
        }

        let expr = &assignment.expression;
        let expr_type = expr.return_type();
        let var_type = assignment.variable.return_type();
        if expr_type != var_type {
            self.diagnostics_bag.borrow_mut().report_error(
                &expr.span,
                &format!(
                    "Can not assign expression returning `{}` type to variable `{}` of type `{}`.",
                    expr_type, assignment.variable.name, var_type
                ),
            );
        }

        self.do_visit_declaration_definition(assignment);
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
            ExpressionKind::Delay(_) => {}
            ExpressionKind::SizeOf(_) => {}

            ExpressionKind::Index(index_expr) => {
                match &index_expr.var_ref.type_() {
                    super::VariableType::Register(reg_size) => {
                        if index_expr.index >= *reg_size {
                            self.diagnostics_bag.borrow_mut().report_error(&expression.span, &format!("Index {n} of is out of range of register with size {reg_size}.", n=index_expr.index))
                        }
                    }
                    _ => self.diagnostics_bag.borrow_mut().report_error(
                        &expression.span,
                        "Only variables of type 'register' can be indexed.",
                    ),
                }
            }
            ExpressionKind::Pick(e) => {
                self.report_if_invalid_signal(e.pick_signal.as_str(), &expression.span)
            }
            ExpressionKind::Binary(b_expr) => {
                let left_r = b_expr.left.return_type();
                if left_r != ExpressionReturnType::Int && left_r != ExpressionReturnType::Many {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &expression.span,
                        &format!(
                            "Left side of expression must be `int` or `many` not `{}`.",
                            left_r
                        ),
                    )
                }
                let rigth_r = b_expr.right.return_type();
                if rigth_r != ExpressionReturnType::Int && rigth_r != ExpressionReturnType::Many {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &expression.span,
                        &format!(
                            "Right side of expression must be `int` or `many` not `{}`.",
                            rigth_r
                        ),
                    )
                }
            }
        };
        self.do_visit_expression(expression);
    }

    fn visit_statement(&mut self, statement: &super::Statement) {
        // Make sure variables are only assign expressions returning their type
        if let StatementKind::DeclarationDefinition(assignment) = &statement.kind {
            let var_type = assignment.variable.return_type();
            let expr_type = &assignment.expression.return_type();
            if var_type != *expr_type {
                self.diagnostics_bag.borrow_mut().report_error(
                    &assignment.expression.span,
                    &format!(
                        "Can not assign expression returning `{}` type to variable `{}` of type `{}`.",
                        expr_type, assignment.variable.name, var_type
                    ),
                );
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

        for input in &block.inputs {
            if let Some(sig) = input.type_.signal() {
                self.report_if_invalid_signal(&sig, &input.span);
            }
        }

        for output in &block.outputs {
            if let Some(sig) = output.type_.signal() {
                self.report_if_invalid_signal(&sig, &output.span);
            }
        }

        self.do_visit_block(block);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[ignore = "Not implemented"]
    #[test]
    fn check_return_types() {
        let (_, bag) = AST::from_str(
            "
            block main() => (out: bool) {
                out <- 10;
            }
            ",
        );
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert_eq!(m_bag.error_count(), 1);
    }

    #[test]
    fn check_assignment_types() {
        let (_, bag) = AST::from_str(
            "
            block main() => (out: bool) {
                a: int <- false;
                out <- a;
            }
            ",
        );
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert_eq!(m_bag.error_count(), 2);
    }

    #[ignore = "Not implemented"]
    #[test]
    fn check_expression_types() {
        let (_, bag) = AST::from_str(
            "
        block main() => (out: int) {
            b: int <- 5 + false * 10;
            out <- b;
        }
        ",
        );
        let m_bag = bag.borrow_mut();
        m_bag.print();
        assert!(m_bag.error_count() == 1);
    }
}
