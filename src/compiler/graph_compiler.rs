use std::collections::HashMap;

use crate::compiler::graph::*;
use crate::ast::{AST, Expression, ExpressionKind, BinaryOperatorKind, Block};
use crate::ast::{Statement, StatementKind, VariableType};
use crate::diagnostics::DiagnosticsBagRef;

pub struct GraphCompiler {
    ast: AST,
    next_anysignal: u64,
    block_graphs: HashMap<String, Graph>,
    scopes: Vec<HashMap<String, VId>>,
    diagnostics_bag: DiagnosticsBagRef,
}

impl GraphCompiler {
    pub fn new(ast: AST, diagnostics_bag: DiagnosticsBagRef) -> Self {
        Self { ast, diagnostics_bag, next_anysignal: 0, block_graphs: HashMap::new(), scopes: vec![HashMap::new()] }
    }

    pub fn get_graph(&self) -> Graph {
        self.block_graphs.get("main").unwrap().clone()
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

    pub fn compile(&mut self) -> Graph {
        for statement in self.ast.statements.clone() {
            self.compile_statement(statement);
        }
        self.get_graph()
    }

    fn get_new_anysignal(&mut self) -> IOType {
        let signal = IOType::AnySignal(self.next_anysignal);
        self.next_anysignal += 1;
        signal
    }

    /// Returns a typle:: (output_vid, output_type)
    fn compile_expression(&mut self, mut graph: &mut Graph, expr: &Expression, out_type: Option<IOType>) -> (VId, IOType) {
        match &expr.kind {
            ExpressionKind::Number(n) => {
                let t = IOType::Constant(n.number);
                (graph.push_input_node("".to_string(), t.clone()), t) // TODO: It is ugly with "" being the variable name.
            },
            ExpressionKind::Variable(var) => {
                if let Some(var_node_vid) = self.search_scope(var.name.clone()) {
                    let var_node = graph.get_vertex(&var_node_vid).unwrap().clone();
                    let signal = match var_node {
                        Node::Input(var_output_node) => var_output_node.input.clone(),
                        Node::Output(var_output_node) => var_output_node.output_type.clone(),
                        Node::Inner(_) => panic!("Var nodes should be output or input nodes"),
                    };
                    let vid = graph.push_node(Node::Inner(InnerNode::new()));
                    graph.push_connection(var_node_vid, vid, Connection::Arithmetic(ArithmeticConnection::new_pick(
                                signal.clone()
                                )));
                    (vid, signal)
                }
                else {
                    match &var.variable_type {
                        VariableType::All => todo!(),
                        VariableType::Any => {
                            let signal = self.get_new_anysignal();
                            (graph.push_input_node(var.name.clone(), signal.clone()), signal)
                        },
                        VariableType::Int(s) => {
                            let signal = IOType::Signal(s.clone());
                            (graph.push_input_node(var.name.clone(), signal.clone()), signal)
                        }
                        VariableType::Error => panic!("there should not be any error variables if the compiling step is reached."),
                    }
                }
            },
            ExpressionKind::Pick(pick_expr) => {
                if let Some(var_out_vid) = self.search_scope(pick_expr.from.name.clone()) {
                    let out_type = IOType::Signal(pick_expr.pick_signal.clone());
                    let picked_vid = graph.push_inner_node();
                    graph.push_connection(var_out_vid, picked_vid, Connection::Arithmetic(
                            ArithmeticConnection::new_pick(out_type.clone())
                            ));
                    (picked_vid, out_type)
                }
                else {
                    panic!("pick from variable which not in scope: {:?}.", pick_expr);
                }
            },
            ExpressionKind::Parenthesized(pexpr) => self.compile_expression(&mut graph, &*pexpr.expression, out_type),
            ExpressionKind::Binary(bin_expr) => {
                let (left_vid, left_type) = self.compile_expression(&mut graph, &*bin_expr.left, None);
                let (right_vid, right_type) = self.compile_expression(&mut graph, &*bin_expr.right, None);

                let operation = match bin_expr.operator.kind {
                    BinaryOperatorKind::Plus => ArithmeticOperation::ADD,
                    BinaryOperatorKind::Minus => ArithmeticOperation::SUBTRACT,
                    BinaryOperatorKind::Multiply => ArithmeticOperation::MULTIPLY,
                    BinaryOperatorKind::Divide => ArithmeticOperation::DIVIDE,
                };

                let input = graph.push_inner_node();
                let output = graph.push_inner_node();

                // Pick
                graph.push_connection(left_vid, input, Connection::Arithmetic(
                        ArithmeticConnection::new_pick(left_type.clone())
                ));

                // Pick
                graph.push_connection(right_vid, input, Connection::Arithmetic(
                        ArithmeticConnection::new_pick(right_type.clone())
                ));

                let out_type = if let Some(t) = out_type { t }
                else { self.get_new_anysignal() };

                let arithmetic_connection = ArithmeticConnection::new(left_type, right_type, operation, out_type.clone());
                graph.push_connection(input, output, Connection::Arithmetic(arithmetic_connection));
                (output, out_type)
            },
            ExpressionKind::BlockLink(block_expr) => {
                let vars: Vec<(VId, IOType)> = block_expr.inputs.iter()
                    .map(|expr| 
                         match expr.kind.clone() {
                            ExpressionKind::Variable(variable) => {
                                if let Some(var_vid) = self.search_scope(variable.name.clone()) { 
                                    let t = self.variable_type_to_iotype(&variable.variable_type);
                                    (var_vid, t)
                                }
                                else { panic!("Block links requires defined variables."); }
                            }
                            ExpressionKind::Number(_) => self.compile_expression(&mut graph, expr, None),
                            ExpressionKind::Binary(_) => self.compile_expression(&mut graph, expr, None),
                            ExpressionKind::Parenthesized(_) => self.compile_expression(&mut graph, expr, None),
                            ExpressionKind::Pick(p) => {
                                let all_var = p.from;
                                let signal = p.pick_signal;
                                if let Some(var_vid) = self.search_scope(all_var.name.clone()) { 
                                    let picked_vid = graph.push_inner_node();
                                    let t = IOType::Signal(signal);
                                    graph.push_connection(var_vid, picked_vid, Connection::Arithmetic(ArithmeticConnection::new_pick(t.clone())));
                                    (picked_vid, t)
                                }
                                else { panic!("Block links requires defined variables."); }
                            },
                            ExpressionKind::BlockLink(_) => todo!("You cannot link to blocks when linking to another block."),
                            ExpressionKind::Error => panic!("Compilation should have stopped before this step if errors where encountered."),
                        }
                    ).collect();

                let outputs = match self.get_block_graph(&block_expr.block.name) {
                    Some(name) => {
                        graph.stitch_graph(name, vars).expect("Wrong number of arguments for function")
                    },
                    None => {
                        panic!("Block not defined.");
                    },
                };
                if outputs.len() != 1 { todo!("Blocks with multipule outputs are not implemented yet"); }

                let typed_outputs: Vec<(VId, IOType)> = outputs.iter().map(|(vid, node)| {
                    let t = match node {
                        Node::Output(n) => n.output_type.clone(),
                        _ => panic!("Compiler Error: Nodes should only be outputs here.")
                    };
                    (vid.clone(), t)
                }).collect();

                typed_outputs[0].clone()
            },
            ExpressionKind::Error => panic!("No errors shoud exist when compiling, as they should have stopped the after building the AST."),
        }
    }

    fn variable_type_to_iotype(&mut self, variable_type: &VariableType) -> IOType {
        match variable_type {
            VariableType::Int(s) => IOType::Signal(s.clone()),
            VariableType::Any => self.get_new_anysignal(),
            VariableType::All => IOType::All,
            VariableType::Error => panic!("there should not be any error variables if the compiling step is reached.")
        }
    }

    fn add_block_graph(&mut self, name: String, graph: Graph) {
        self.block_graphs.insert(name, graph); 
    }

    fn get_block_graph(&mut self, name: &String) -> Option<&Graph> {
        self.block_graphs.get(name)
    }

    fn compile_block(&mut self, block: &Block) -> Graph {
        let mut graph = Graph::new();
        self.enter_scope();
        for input_var in block.inputs.clone() {
            let var_name = input_var.name.clone();
            let var_iotype = self.variable_type_to_iotype(&input_var.variable_type.clone());
            let input_vid = graph.push_input_node(var_name.clone(), var_iotype);
            self.add_to_scope(var_name, input_vid)
        }
        for statement in &block.statements {
            match &statement.kind {
                StatementKind::Block(_) => {
                    self.diagnostics_bag.borrow_mut().report_error(&statement.span, "A `block` cannot be defined within another block.");
                },
                StatementKind::Expression(expr) => { self.compile_expression(&mut graph, expr, None); },
                StatementKind::Assignment(assignment) => {
                    let out_type = self.variable_type_to_iotype(&assignment.variable.variable_type);
                    let (mut output_vid, _) = self.compile_expression(&mut graph, &assignment.expression, Some(out_type.clone()));
                    let output_node = graph.get_vertex(&output_vid).unwrap().clone();
                    match output_node {
                        Node::Inner(_n) => { // Convert inner node to output node
                            let input_type = graph.get_single_input(&output_vid).unwrap();
                            let var_out_node = OutputNode::new(assignment.variable.name.clone(), out_type.clone());
                            let middle_vid = output_vid.clone();
                            output_vid = graph.push_node(Node::Inner(InnerNode::new()));
                            graph.push_connection(middle_vid, output_vid, Connection::Arithmetic(
                                    ArithmeticConnection::new_convert(input_type, out_type)
                                    ));
                            graph.override_node(output_vid, Node::Output(var_out_node));
                        },
                        Node::Input(n) => {
                            let var_node = Node::Output(OutputNode::new(assignment.variable.name.clone(), out_type.clone()));
                            let var_node_vid = graph.push_node(var_node);
                            graph.push_connection(output_vid, var_node_vid, 
                                                       Connection::Arithmetic(
                                                           ArithmeticConnection::new_convert(
                                                               n.input,
                                                               out_type
                                                           )
                                                       ));
                            output_vid = var_node_vid;
                        },
                        Node::Output(n) => {
                            let var_node_vid = graph.push_node(Node::Output(n.clone()));
                            graph.push_connection(output_vid, var_node_vid, 
                                                       Connection::Arithmetic(
                                                           ArithmeticConnection::new_convert(
                                                               n.output_type,
                                                               out_type
                                                           )
                                                       ));
                            output_vid = var_node_vid;
                        },
                    }
                    self.add_to_scope(assignment.variable.name.clone(), output_vid);
                },
                StatementKind::Error => panic!("There should not be error statements when compilation has started."),
            }; 
        }
        self.exit_scope();
        graph
    }

    fn compile_statement(&mut self, statement: Statement) {
        match &statement.kind {
            StatementKind::Block(block) => {
                let block_graph = self.compile_block(block);
                self.add_block_graph(block.name.clone(), block_graph);
            }
            _ => todo!("Only block statements implemented for now."),
        }
    }

}
