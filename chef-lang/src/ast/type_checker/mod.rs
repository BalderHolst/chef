//! Chef type checker.

// TODO: Check that blocks are called with the correct arguments
// TODO: Check for reserved signals. See RESERVED_GATE_SIGNAL.

#[cfg(test)]
mod tests;

use crate::{diagnostics::DiagnosticsBagRef, text::TextSpan, utils::BASE_SIGNALS};

use super::{visitors::Visitor, ExpressionReturnType, Variable, AST};

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

    fn check_equal<F>(&mut self, t1: &ExpressionReturnType, t2: &ExpressionReturnType, err: F)
    where
        F: FnOnce() -> (TextSpan, String),
    {
        if t1 != t2 {
            let (span, msg) = err();
            self.diagnostics_bag.borrow_mut().report_error(&span, &msg);
        }
    }

    /// Make sure variables are only assign expressions returning their type
    fn check_assign(
        &mut self,
        var_type: &ExpressionReturnType,
        expr_type: &ExpressionReturnType,
        span: &TextSpan,
    ) {
        // Allow any assignment to a variable of type `many`
        if var_type == &ExpressionReturnType::Many {
            return;
        }

        self.check_equal(var_type, expr_type, || {
            (
                span.clone(),
                format!(
                    "Can not assign expression returning `{}` type to variable of type `{}`.",
                    &expr_type, &var_type
                ),
            )
        })
    }

    fn check_variable(&mut self, var: &Variable) {
        let var = var.borrow();
        if let Some(sig) = var.type_.signal() {
            self.report_if_invalid_signal(sig.as_str(), &var.span)
        }
    }
}

// TODO: Variable ref
impl Visitor for TypeChecker {
    fn visit_block_link_expression(&mut self, _block: &super::BlockLinkExpression) {}
    fn visit_number(&mut self, _number: &super::visitors::Number) {}
    fn visit_bool(&mut self, _value: &bool) {}

    fn visit_pick_expression(&mut self, pick: &super::PickExpression) {
        self.report_if_invalid_signal(&pick.pick_signal, &pick.span)
    }

    fn visit_variable_ref(&mut self, _var: &super::VariableRef) {}
    fn visit_index_expression(&mut self, _expr: &super::IndexExpression) {}

    fn visit_definition(&mut self, definition: &super::Definition) {
        self.check_variable(&definition.variable);
        let expr = &definition.expression;
        let expr_type = expr.return_type();
        let var_type = definition.variable.borrow().return_type();
        self.check_assign(&var_type, &expr_type, &expr.span);
        self.do_visit_definition(definition);
    }

    fn visit_declaration_definition(&mut self, assignment: &super::DeclarationDefinition) {
        self.check_variable(&assignment.variable);
        let expr = &assignment.expression;
        let expr_type = expr.return_type();
        let var_type = assignment.variable.borrow().return_type();
        self.check_assign(&var_type, &expr_type, &expr.span);
        self.do_visit_declaration_definition(assignment);
    }

    fn visit_statement(&mut self, statement: &super::Statement) {
        self.do_visit_statement(statement);
    }

    fn visit_block(&mut self, block: &super::Block) {
        for input in &block.inputs {
            self.check_variable(input)
        }

        for output in &block.outputs {
            self.check_variable(output)
        }

        self.do_visit_block(block);
    }
}
