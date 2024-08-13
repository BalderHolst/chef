//! Evaluates constant expressions in the [AST].

use crate::diagnostics::{CompilationError, CompilationResult};

use super::{visitors::MutVisitor, BinaryOperator, Expression, ExpressionKind, MutVar, AST};

/// Evaluate constant expressions in the [AST] and substitutes them for their results.
pub fn evaluate_constants(ast: &mut AST<MutVar>) {
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
    fn evaluate(&mut self, ast: &mut AST<MutVar>) {
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

/// Evaluate constant parts of an expression, simplifying it.
pub(crate) fn evaluate_constant_expression(
    mut expr: Expression<MutVar>,
) -> CompilationResult<ConstantValue> {
    let mut evaluator = ConstantEvaluator::new();

    let expr_span = expr.span.clone();

    let expr = &mut expr;

    evaluator.did_work = true;
    while evaluator.did_work {
        evaluator.did_work = false;
        evaluator.visit_expression(expr);
    }

    match expr.kind {
        ExpressionKind::Bool(b) => Ok(ConstantValue::Bool(b)),
        ExpressionKind::Int(i) => Ok(ConstantValue::Int(i)),
        _ => Err(CompilationError::new_localized(
            "Could not evaluate constant expression.",
            expr_span,
        )),
    }
}

/// A constant value. This value has no type.
#[derive(Debug)]
pub(crate) enum ConstantValue {
    Int(i32),
    Bool(bool),
}

/// Return the constant integer value of an expression if it is constant
fn get_constant_int(expr: &Expression<MutVar>) -> Option<i32> {
    match &expr.kind {
        ExpressionKind::Int(n) => Some(*n),
        ExpressionKind::Parenthesized(p) => get_constant_int(&p.expression),
        ExpressionKind::Negative(ne) => get_constant_int(&ne.expression).map(|n| -n),
        ExpressionKind::Bool(_) => None,
        ExpressionKind::Binary(_) => None,
        ExpressionKind::Pick(_) => None,
        ExpressionKind::Index(_) => None,
        ExpressionKind::VariableRef(_) => None,
        ExpressionKind::BlockLink(_) => None,
        ExpressionKind::Delay(de) => get_constant_int(&de.expression),
        ExpressionKind::SizeOf(se) => get_constant_int(&se.expression),
        ExpressionKind::Gate(_) => None,
    }
}

// Implementation of the visitor for the evaluator.
impl MutVisitor<MutVar> for ConstantEvaluator {
    fn visit_pick_expression(&mut self, _expr: &mut super::PickExpression<MutVar>) {}
    fn visit_number(&mut self, _number: &mut i32) {}
    fn visit_bool(&mut self, _bool: &mut bool) {}
    fn visit_variable_ref(&mut self, _var: &mut super::VariableRef<MutVar>) {}
    fn visit_expression(&mut self, expression: &mut Expression<MutVar>) {
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

                match binary_expression.operator {
                    BinaryOperator::Add => ConstantValue::Int(left + right),
                    BinaryOperator::Subtract => ConstantValue::Int(left - right),
                    BinaryOperator::Multiply => ConstantValue::Int(left * right),
                    BinaryOperator::Divide => ConstantValue::Int(left / right),
                    BinaryOperator::LargerThan => ConstantValue::Bool(left > right),
                    BinaryOperator::LargerThanOrEqual => ConstantValue::Bool(left >= right),
                    BinaryOperator::LessThanOrEqual => ConstantValue::Bool(left <= right),
                    BinaryOperator::Equals => ConstantValue::Bool(left == right),
                    BinaryOperator::NotEquals => ConstantValue::Bool(left != right),
                    BinaryOperator::LessThan => ConstantValue::Bool(left < right),

                    // These all take a 'many' input, and thus cannot be evaluated af compile time.
                    BinaryOperator::Combine => return,
                    BinaryOperator::EveryEquals => return,
                    BinaryOperator::EveryLargerThan => return,
                    BinaryOperator::EveryLargerThanEquals => return,
                    BinaryOperator::EveryLessThan => return,
                    BinaryOperator::EveryLessThanEquals => return,
                    BinaryOperator::EveryNotEquals => return,
                    BinaryOperator::AnyEquals => return,
                    BinaryOperator::AnyLargerThan => return,
                    BinaryOperator::AnyLargerThanEquals => return,
                    BinaryOperator::AnyLessThan => return,
                    BinaryOperator::AnyLessThanEquals => return,
                    BinaryOperator::AnyNotEquals => return,
                }
            }
            ExpressionKind::VariableRef(_) => return,
            ExpressionKind::Negative(negative_expr) => match &mut negative_expr.expression.kind {
                ExpressionKind::Int(n) => ConstantValue::Int(*n * -1),
                _ => {
                    self.visit_negative_expression(negative_expr);
                    return;
                }
            },
            _ => {
                self.do_visit_expression(expression);
                return;
            }
        };

        expression.kind = match result {
            ConstantValue::Int(val) => ExpressionKind::Int(val),
            ConstantValue::Bool(val) => ExpressionKind::Bool(val),
        };
        self.did_work = true;
    }
}
