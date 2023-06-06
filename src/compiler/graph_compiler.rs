use std::borrow::Borrow;
use std::collections::HashMap;
use std::process::exit;

use crate::compiler::graph::*;
use crate::ast::{AST, Expression, ExpressionKind, BinaryOperatorKind};
use crate::ast::{Statement, StatementKind, VariableType};
use crate::diagnostics::DiagnosticsBagRef;

pub struct GraphCompiler {
    ast: AST,
    next_anysignal: u64,
    pub graph: Graph,
    scopes: Vec<HashMap<String, VId>>,
    diagnostics_bag: DiagnosticsBagRef,
}

impl GraphCompiler {
    pub fn new(ast: AST, diagnostics_bag: DiagnosticsBagRef) -> Self {
        Self { ast, diagnostics_bag, next_anysignal: 0, graph: Graph::new(), scopes: vec![HashMap::new()] }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn add_to_scope(&mut self, variable_name: String, output_vid: VId) {
        if self.scopes.last_mut().unwrap().insert(variable_name.clone(), output_vid).is_some() {
            panic!("tried to override a variable in scope.")
        }
    }

    fn search_scope(&self, variable_name: String) -> Option<VId> {
        let scopes_len = self.scopes.len();
        for i in 0..scopes_len {
            let p = scopes_len - i - 1;
            if let Some(vid) = self.scopes.get(p).unwrap().get(&variable_name) {
                return Some(vid.clone());
            }
        }
        None
    }

    pub fn compile(&mut self) -> &Graph {
        for statement in self.ast.statements.clone() {
            self.compile_statement(statement);
        }
        &self.graph
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
                (self.graph.push_input_node("".to_string(), t.clone()), t) // TODO: It is ugly with "" being the variable name.
            },
            ExpressionKind::Variable(var) => {
                if let Some(var_node_vid) = self.search_scope(var.name.clone()) {
                    let var_node = self.graph.get_vertex(&var_node_vid).unwrap();
                    if let Node::Output(var_output_node) = var_node {
                        let signal = var_output_node.output_type.clone(); // TODO add connection
                        let vid = self.graph.push_node(Node::Inner(InnerNode::new()));

                        self.graph.push_connection(var_node_vid, vid, Connection::Arithmetic(ArithmeticConnection::new_pick(
                                    signal.clone()
                                    )));

                        (vid, signal)
                    }
                    else { panic!("Var nodes should be output nodes") }
                }
                else {
                    match &var.variable_type {
                        VariableType::All => todo!(),
                        VariableType::Any => {
                            let signal = self.get_new_anysignal();
                            (self.graph.push_input_node(var.name.clone(), signal.clone()), signal)
                        },
                        VariableType::Int(s) => {
                            let signal = IOType::Signal(s.clone());
                            (self.graph.push_input_node(var.name.clone(), signal.clone()), signal)
                        }
                        VariableType::Error => panic!("there should not be any error variables if the compiling step is reached."),
                    }
                }
            },
            ExpressionKind::Parenthesized(pexpr) => self.compile_expression(&*pexpr.expression, out_type),
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
                        ArithmeticConnection::new_pick(left_type.clone())
                ));

                // Pick
                self.graph.push_connection(right_vid, input, Connection::Arithmetic(
                        ArithmeticConnection::new_pick(right_type.clone())
                ));


                let out_type = if let Some(t) = out_type { t }
                else { self.get_new_anysignal() };

                let arithmetic_connection = ArithmeticConnection::new(left_type, right_type, operation, out_type.clone());
                self.graph.push_connection(input, output, Connection::Arithmetic(arithmetic_connection));
                (output, out_type)
            },
            ExpressionKind::Error => panic!("No errors shoud exist when compiling, as they should have stopped the after building the AST."),
        }
    }

    fn variable_type_to_iotype(&mut self, variable_type: &VariableType) -> IOType {
        match variable_type {
            VariableType::Int(s) => IOType::Signal(s.clone()),
            VariableType::Any => self.get_new_anysignal(),
            VariableType::All => todo!(),
            VariableType::Error => panic!("there should not be any error variables if the compiling step is reached.")
        }
    }

    fn compile_statement(&mut self, statement: Statement) {
        match &statement.kind {
            StatementKind::Block(block) => {
                self.enter_scope();
                for statement in &block.statements {
                    match &statement.kind {
                        StatementKind::Block(_) => {
                            self.diagnostics_bag.borrow_mut().report_error(&statement.span, "A `block` cannot be defined within another block.");
                        },
                        StatementKind::Expression(expr) => { self.compile_expression(expr, None); },
                        StatementKind::Assignment(assignment) => {
                            let out_type = self.variable_type_to_iotype(&assignment.variable.variable_type);
                            let (output_vid, _) = self.compile_expression(&assignment.expression, Some(out_type.clone()));
                            let output_node = self.graph.get_vertex(&output_vid).unwrap().clone();
                            match output_node {
                                Node::Inner(_n) => { // Convert inner node to output node
                                    let var_out_node = OutputNode::new(assignment.variable.name.clone(), out_type);
                                    let new_node = Node::Output(var_out_node);
                                    self.graph.override_node(output_vid, new_node);
                                    self.add_to_scope(assignment.variable.name.clone(), output_vid);
                                },
                                Node::Input(n) => {
                                    let var_node = Node::Output(OutputNode::new(assignment.variable.name.clone(), out_type.clone()));
                                    let var_node_vid = self.graph.push_node(var_node);
                                    self.graph.push_connection(output_vid, var_node_vid, 
                                                               Connection::Arithmetic(
                                                                   ArithmeticConnection::new_convert(
                                                                       n.input,
                                                                       out_type
                                                                   )
                                                               ));
                                    self.add_to_scope(assignment.variable.name.clone(), output_vid);
                                },
                                Node::Output(_) => panic!("dont think there should be an output node here at this stage, as they are created here."),
                            }
                        },
                        StatementKind::Error => panic!("There should not be error statements when compilation has started."),
                    };
                }
            self.exit_scope();
            }
            _ => todo!("Only block statements inplemented for now."),
        }
    }

}
