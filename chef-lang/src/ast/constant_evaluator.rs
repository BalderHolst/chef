//! Evaluates constant expressions in the [AST]. Use the public [evaluate_constants] function.

use super::{visitors::MutVisitor, Expression, ExpressionKind, AST};

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
            for block in &mut ast.blocks.iter_mut() {
                self.visit_block(block);
            }
            if !self.did_work {
                break;
            }
        }
    }
}

#[derive(Debug)]
enum EvaluatorResult {
    Int(i32),
    Bool(bool),
}

/// Return the constant integer value of an expression if it is constant
fn get_constant_int(expr: &Expression) -> Option<i32> {
    match &expr.kind {
        ExpressionKind::Int(n) => Some(*n),
        ExpressionKind::Parenthesized(p) => get_constant_int(&p.expression),
        ExpressionKind::Bool(_) => None,
        ExpressionKind::Binary(_) => None,
        ExpressionKind::Pick(_) => None,
        ExpressionKind::VariableRef(_) => None,
        ExpressionKind::BlockLink(_) => None,
        ExpressionKind::When(_) => None,
        ExpressionKind::Error => None,
    }
}

impl MutVisitor for ConstantEvaluator {
    fn visit_pick_expression(&mut self, _expr: &mut super::PickExpression) {}
    fn visit_error_statement(&mut self) {}
    fn visit_number(&mut self, _number: &mut i32) {}
    fn visit_bool(&mut self, _bool: &mut bool) {}
    fn visit_variable_ref(&mut self, _var: &super::VariableRef) {}
    fn visit_error_expression(&mut self) {}

    fn visit_expression(&mut self, expression: &mut Expression) {
        let result = match &mut expression.kind {
            ExpressionKind::Binary(binary_expression) => {
                let left = get_constant_int(&binary_expression.left);
                let right = get_constant_int(&binary_expression.right);

                if left.is_none() || right.is_none() {
                    self.visit_binary_expression(binary_expression);
                    return;
                }
                let left = left.unwrap();
                let right = right.unwrap();

                match binary_expression.operator.kind {
                    super::BinaryOperatorKind::Add => EvaluatorResult::Int(left + right),
                    super::BinaryOperatorKind::Subtract => EvaluatorResult::Int(left - right),
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
            ExpressionKind::VariableRef(var_ref) => match var_ref.var.type_ {
                super::VariableType::ConstInt(i) => EvaluatorResult::Int(i),
                super::VariableType::ConstBool(b) => EvaluatorResult::Bool(b),
                _ => return,
            },
            _ => {
                self.do_visit_expression(expression);
                return;
            }
        };

        expression.kind = match result {
            EvaluatorResult::Int(val) => ExpressionKind::Int(val),
            EvaluatorResult::Bool(val) => ExpressionKind::Bool(val),
        };
        self.did_work = true;
    }
}
