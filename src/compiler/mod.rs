use crate::compiler::graph::*;
use crate::ast::{AST, Expression, ExpressionKind, BinaryExpression, BinaryOperator, Assignment, BinaryOperatorKind};
use crate::ast::{Statement, StatementKind, VariableType};

pub mod graph;

pub struct Compiler {
    ast: AST,
    next_anysignal: u64,
    graph: Graph,
}

impl Compiler {
    pub fn new(ast: AST) -> Self {
        Self { ast, next_anysignal: 0, graph: Graph::new() }
    }

    pub fn compile(&mut self) {
        for statement in self.ast.statements.clone() {
            self.compile_statement(statement);
        }
    }

    fn get_new_anysignal(&mut self) -> IOType {
        let signal = IOType::AnySignal(self.next_anysignal);
        self.next_anysignal += 1;
        signal
    }

    fn compile_expression(&mut self, expr: &Expression, out_type: Option<IOType>) -> (VId, IOType) {
        match &expr.kind {
            ExpressionKind::Number(n) => {
                let t = IOType::Constant(n.number);
                (self.graph.push_input_node(vec![t.clone()]), t)
            },
            ExpressionKind::Variable(var) => {
                let signal = match &var.variable_type {
                    VariableType::All => todo!(),
                    VariableType::Any => self.get_new_anysignal(),
                    VariableType::Int(s) => IOType::Signal(s.clone()),
                };
                (self.graph.push_input_node(vec![signal.clone()]), signal)
            },
            ExpressionKind::Parenthesized(expr) => todo!(),
            ExpressionKind::Binary(bin_expr) => {
                let (left_vid, left_type) = self.compile_expression(&*bin_expr.left, None);
                let (right_vid, right_type) = self.compile_expression(&*bin_expr.right, None);

                let operation = match bin_expr.operator.kind {
                    BinaryOperatorKind::Plus => ArithmeticOperation::ADD,
                    BinaryOperatorKind::Minus => ArithmeticOperation::SUBTRACT,
                    BinaryOperatorKind::Multiply => ArithmeticOperation::MULTIPLY,
                    BinaryOperatorKind::Divide => ArithmeticOperation::DIVIDE,
                };

                let input = self.graph.push_inner_node();
                let output = self.graph.push_inner_node();

                // Pick
                self.graph.push_connection(left_vid, input, Connection::Arithmetic(
                        ArithmeticConnection::new(left_type.clone(), IOType::Constant(0), ArithmeticOperation::ADD, left_type.clone()
                )));

                // Pick
                self.graph.push_connection(right_vid, input, Connection::Arithmetic(
                        ArithmeticConnection::new(right_type.clone(), IOType::Constant(0), ArithmeticOperation::ADD, right_type.clone()
                )));


                let out_type = if let Some(t) = out_type { t }
                else { self.get_new_anysignal() };

                let arithmetic_connection = ArithmeticConnection::new(left_type, right_type, operation, out_type.clone());
                self.graph.push_connection(input, output, Connection::Arithmetic(arithmetic_connection));
                (output, out_type)
            },
        }
    }

    fn compile_statement(&mut self, statement: Statement) {
        match &statement.kind {
            StatementKind::Block(block) => {
                for statement in &block.statements {
                    match &statement.kind {
                        StatementKind::Block(_) => todo!("Blocks within block are not implemented."),
                        StatementKind::Expression(expr) => { self.compile_expression(expr, None); },
                        StatementKind::Assignment(assignment) => {
                            let out_type = match &assignment.variable.variable_type {
                                VariableType::Int(s) => IOType::Signal(s.clone()),
                                VariableType::Any => self.get_new_anysignal(),
                                VariableType::All => todo!()
                            };
                            let (output_vid, _) = self.compile_expression(&assignment.expression, Some(out_type));
                            let mut output_node = self.graph.get_mut_vertex(&output_vid).unwrap();
                        },
                    };
                }

            }
            _ => todo!("Only block statements inplemented"),
        }

        self.graph.print();
    }

}
