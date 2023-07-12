//! Evaluates constant expressions in the [AST]. Use the public [evaluate_constants] function.

use super::{visitors::MutVisitor, Expression, ExpressionKind, IntExpression, AST};

/// Evaluate constant expressions in the [AST] and substitutes them for their results.
pub fn evaluate_constants(ast: &mut AST) {
    ConstantEvaluator::new().evaluate(ast)
}

/// The evaluator.
struct ConstantEvaluator {
    did_work: bool,
}

impl ConstantEvaluator {
    fn new() -> Self {
        Self { did_work: false }
    }

    /// Evaluate constant expressions.
    fn evaluate(&mut self, ast: &mut AST) {
        // Run the evaluator through the tree until is cannot evaluate any more.
        loop {
            self.did_work = false;
            for statement in &mut ast.statements.iter_mut() {
                self.visit_statement(statement);
            }
            if !self.did_work {
                break;
            }
        }
    }
}

enum EvaluatorResult {
    Int(i32),
    Bool(bool),
}

impl MutVisitor for ConstantEvaluator {
    fn visit_pick_expression(&mut self, _expr: &mut super::PickExpression) {}
    fn visit_error_statement(&mut self) {}
    fn visit_number(&mut self, _number: &mut super::IntExpression) {}
    fn visit_bool(&mut self, _bool: &mut bool) {}
    fn visit_variable(&mut self, _var: &super::Variable) {}
    fn visit_error_expression(&mut self) {}

    fn visit_expression(&mut self, expression: &mut Expression) {
        let result = match &mut expression.kind {
            ExpressionKind::Binary(binary_expression) => {
                let (left, right) =
                    match (&binary_expression.left.kind, &binary_expression.right.kind) {
                        (ExpressionKind::Int(l), ExpressionKind::Int(r)) => (l.number, r.number),
                        _ => {
                            self.visit_binary_expression(binary_expression);
                            return;
                        }
                    };
                self.did_work = true;
                match binary_expression.operator.kind {
                    super::BinaryOperatorKind::Plus => EvaluatorResult::Int(left + right),
                    super::BinaryOperatorKind::Minus => EvaluatorResult::Int(left - right),
                    super::BinaryOperatorKind::Multiply => EvaluatorResult::Int(left * right),
                    super::BinaryOperatorKind::Divide => EvaluatorResult::Int(left / right),
                    super::BinaryOperatorKind::LargerThan => EvaluatorResult::Bool(left > right),
                    super::BinaryOperatorKind::LargerThanOrEqual => {
                        EvaluatorResult::Bool(left >= right)
                    }
                    super::BinaryOperatorKind::LessThan => EvaluatorResult::Bool(left < right),
                    super::BinaryOperatorKind::LessThanOrEqual => {
                        EvaluatorResult::Bool(left <= right)
                    }
                    super::BinaryOperatorKind::Equals => EvaluatorResult::Bool(left == right),
                    super::BinaryOperatorKind::NotEquals => EvaluatorResult::Bool(left != right),
                }
            }
            _ => {
                self.do_visit_expression(expression);
                return;
            }
        };

        expression.kind = match result {
            EvaluatorResult::Int(val) => ExpressionKind::Int(IntExpression::new(val)),
            EvaluatorResult::Bool(val) => ExpressionKind::Bool(val),
        }
    }
}
