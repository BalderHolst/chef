//! Chef type checker.

// TODO: Check that blocks are called with the correct arguments

#[cfg(test)]
mod tests;

use std::rc::Rc;

use crate::{diagnostics::DiagnosticsBagRef, text::TextSpan, utils::BASE_SIGNALS};

use super::{visitors::Visitor, DefinitionKind, DetVar, ExpressionReturnType, Variable, AST};

/// Type check an [AST].
pub fn check(ast: &AST<DetVar>, diagnostics_bag: DiagnosticsBagRef) {
    let mut checker = TypeChecker {
        ast,
        diagnostics_bag,
    };
    checker.check();
}

/// The type checker.
struct TypeChecker<'a> {
    diagnostics_bag: DiagnosticsBagRef,
    ast: &'a AST<DetVar>,
}

impl TypeChecker<'_> {
    fn check(&mut self) {
        for block in self.ast.iter_blocks() {
            match block {
                super::DefinedBlock::Block(b) => self.visit_block(b),
                super::DefinedBlock::DynBlock(b) => self.visit_dyn_block(b),
            };
        }
    }

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

    fn report_error(&self, span: &TextSpan, msg: &str) {
        self.diagnostics_bag.borrow_mut().report_error(span, msg)
    }

    /// Report error if the signal is invalid.
    fn report_if_invalid_signal(&self, signal: &str, span: &TextSpan) {
        if !Self::is_valid_signal(signal) {
            self.report_error(span, &format!("Invalid factorio signal: `{}`", signal))
        }
    }

    /// Check the return type of an expression.
    fn check_expr_type(&mut self, t: &ExpressionReturnType, span: &TextSpan) {
        if *t == ExpressionReturnType::Infered {
            self.report_error(span, "Expression return type could not be inferred.")
        }
    }

    /// Check if two types are equal.
    fn check_direct_assign<F>(
        &mut self,
        t1: &ExpressionReturnType,
        t2: &ExpressionReturnType,
        err: F,
    ) where
        F: FnOnce() -> (TextSpan, String),
    {
        if !t1.direct_assignable(t2) {
            let (span, msg) = err();
            self.diagnostics_bag.borrow_mut().report_error(&span, &msg);
        }
    }

    /// Make sure variables are only assign expressions returning their type
    fn check_assign(
        &mut self,
        var_type: &ExpressionReturnType,
        expr_type: &ExpressionReturnType,
        assign_type: &DefinitionKind,
        span: &TextSpan,
    ) {
        // Allow any assignment to a variable of type `many`
        if var_type == &ExpressionReturnType::Many {
            return;
        }

        // Assume that this error has been reported elsewhere
        if expr_type == &ExpressionReturnType::Infered {
            return;
        }

        match assign_type {
            DefinitionKind::Convert(_) => {
                if !((var_type.is_int() && expr_type.is_int())
                    || (var_type.is_bool() && expr_type.is_bool()))
                {
                    self.report_error(
                        span,
                        &format!(
                            "Can not convert expression returning `{}` type to variable of type `{}`.",
                            &expr_type, &var_type
                        ),
                    )
                }
            }
            DefinitionKind::Equal | DefinitionKind::Wire(_) => {
                self.check_direct_assign(var_type, expr_type, || {
                    (
                        span.clone(),
                        format!(
                        "Can not assign expression returning `{}` type to variable of type `{}`.",
                        &expr_type, &var_type
                    ),
                    )
                })
            }
        }
    }

    /// Check if a variable is a valid signal.
    fn check_variable(&mut self, var: &Rc<DetVar>) {
        if let Some(sig) = var.type_.signal() {
            self.report_if_invalid_signal(sig.as_str(), &var.span)
        }
    }
}

// TODO: Variable ref
impl Visitor<DetVar> for TypeChecker<'_> {
    fn visit_block_link_expression(&mut self, link: &super::BlockLinkExpression<DetVar>) {
        let block_inputs = self
            .ast
            .get_block_by_name(&link.name, link.dyn_block_version)
            .expect("Block must be there after parsing.")
            .inputs
            .clone();

        // Check if the amount of inputs match
        if link.inputs.len() != block_inputs.len() {
            return self.report_error(
                &link.span,
                &format!(
                    "Block `{}` expected {} inputs, found {}.",
                    &link.name,
                    block_inputs.len(),
                    link.inputs.len()
                ),
            );
        }

        // Check if the types of the inputs match
        link.inputs
            .iter()
            .zip(block_inputs.iter())
            .for_each(|(input, block_input)| {
                self.check_direct_assign(&input.return_type(), &block_input.return_type(), || {
                    (
                        link.span.clone(),
                        format!(
                            "Incorrect argument '{}' for block `{}`. Expected `{}`, found `{}`.",
                            &block_input.name,
                            &link.name,
                            &input.return_type(),
                            &block_input.return_type()
                        ),
                    )
                })
            });
    }

    fn visit_number(&mut self, _number: &i32) {}
    fn visit_bool(&mut self, _value: &bool) {}

    fn visit_pick_expression(&mut self, pick: &super::PickExpression<DetVar>) {
        self.report_if_invalid_signal(&pick.pick_signal, &pick.span)
    }

    fn visit_variable_ref(&mut self, _var: &super::VariableRef<DetVar>) {}
    fn visit_index_expression(&mut self, _expr: &super::IndexExpression<DetVar>) {}

    fn visit_definition(&mut self, definition: &super::Definition<DetVar>) {
        self.check_variable(&definition.variable);
        let expr = &definition.expression;
        let expr_type = expr.return_type();
        let var_type = definition.variable.return_type();
        self.check_assign(&var_type, &expr_type, &definition.kind, &expr.span);
        self.do_visit_definition(definition);
    }

    fn visit_declaration_definition(&mut self, assignment: &super::DeclarationDefinition<DetVar>) {
        self.check_variable(&assignment.variable);
        let expr = &assignment.expression;
        let expr_type = expr.return_type();
        let var_type = assignment.variable.return_type();
        self.check_assign(&var_type, &expr_type, &assignment.kind, &expr.span);
        self.do_visit_declaration_definition(assignment);
    }

    fn visit_statement(&mut self, statement: &super::Statement<DetVar>) {
        self.do_visit_statement(statement);
    }

    fn visit_block(&mut self, block: &super::Block<DetVar>) {
        for input in &block.inputs {
            self.check_variable(input)
        }

        for output in &block.outputs {
            self.check_variable(output)
        }

        self.do_visit_block(block);
    }

    fn visit_expression(&mut self, expression: &super::Expression<DetVar>) {
        let expr_type = expression.return_type();
        self.check_expr_type(&expr_type, &expression.span);

        self.do_visit_expression(expression);
    }

    fn visit_binary_expression(&mut self, bin_expr: &super::BinaryExpression<DetVar>) {
        let left_type = bin_expr.left.return_type();
        let right_type = bin_expr.right.return_type();

        if bin_expr
            .operator
            .return_type(left_type.clone(), right_type.clone())
            .is_err()
        {
            self.diagnostics_bag.borrow_mut().report_error(
                &bin_expr.span,
                &format!(
                    "Invalid binary operation between `{}` and `{}`.",
                    &left_type, &right_type
                ),
            );
        }
        self.do_visit_binary_expression(bin_expr);
    }
}
