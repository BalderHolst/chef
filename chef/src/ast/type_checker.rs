use super::{visitors::Visitor, AST};

const BASE_SIGNALS: &str = include_str!("base.signals");

pub fn check(ast: &AST) {
    let mut checker = TypeChecker{};
    for statement in &ast.statements {
        checker.visit_statement(statement);
    }
}

struct TypeChecker { }

impl TypeChecker {
    fn is_valid_signal(s: &str) -> bool {
        for signal_line in BASE_SIGNALS.lines() {
            let signal = signal_line.split(":").last().unwrap();
            if s == signal { return true }
        }
        false
    }

    fn report_if_invalid(signal: &str) {
        if !Self::is_valid_signal(signal) {
            println!("Invalid signal: {}", signal); // TODO Real reporting
            std::process::exit(1);
        }
    }
}

impl Visitor for TypeChecker {
    fn visit_block_link(&mut self, _block: &super::BlockLinkExpression) { }
    fn visit_error_statement(&mut self) { }
    fn visit_number(&mut self, _number: &super::NumberExpression) { }
    fn visit_error_expression(&mut self) { }

    fn visit_pick_expression(&mut self, expr: &super::PickExpression) {
        Self::report_if_invalid(expr.pick_signal.as_str());
    }

    fn visit_block(&mut self, block: &super::Block) {
        for var in &block.inputs {
            if let super::VariableType::Int(signal) = &var.variable_type {
                Self::report_if_invalid(signal);
            }
        }
        for var_type in &block.outputs {
            if let super::VariableType::Int(signal) = var_type {
                Self::report_if_invalid(signal);
            }
        }
        self.do_visit_block(block);
    }

    fn visit_variable(&mut self, var: &super::Variable) {
        if let super::VariableType::Int(signal) = &var.variable_type {
            Self::report_if_invalid(signal);
        }
    }
}
