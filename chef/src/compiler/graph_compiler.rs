use std::collections::HashMap;

use crate::ast::{BinaryOperatorKind, Block, Expression, ExpressionKind, VariableSignalType, AST};
use crate::ast::{Statement, StatementKind, VariableType};
use crate::compiler::graph::*;
use crate::diagnostics::DiagnosticsBagRef;

pub enum ReturnValue<A, B> {
    Int(A),
    Bool(B),
}

pub struct GraphCompiler {
    ast: AST,
    next_anysignal: u64,
    block_graphs: HashMap<String, Graph>,
    scopes: Vec<HashMap<String, NId>>,
    diagnostics_bag: DiagnosticsBagRef,
}

impl GraphCompiler {
    pub fn new(ast: AST, diagnostics_bag: DiagnosticsBagRef) -> Self {
        Self {
            ast,
            diagnostics_bag,
            next_anysignal: 0,
            block_graphs: HashMap::new(),
            scopes: vec![HashMap::new()],
        }
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

    fn add_to_scope(&mut self, variable_name: String, output_vid: NId) {
        if self
            .scopes
            .last_mut()
            .unwrap()
            .insert(variable_name, output_vid)
            .is_some()
        {
            panic!("tried to override a variable in scope.")
        }
    }

    fn search_scope(&self, variable_name: String) -> Option<NId> {
        let scopes_len = self.scopes.len();
        for i in 0..scopes_len {
            let p = scopes_len - i - 1;
            if let Some(vid) = self.scopes.get(p).unwrap().get(&variable_name) {
                return Some(*vid);
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
    fn compile_expression(
        &mut self,
        graph: &mut Graph,
        expr: &Expression,
        out_type: Option<IOType>,
    ) -> (NId, IOType) {
        match &expr.kind {
            // TODO: "" name is awful...
            ExpressionKind::Int(n) => {
                let t = IOType::Constant(n.number);
                (graph.push_input_node("".to_string(), t.clone()), t) // TODO: It is ugly with "" being the variable name.
            },
            ExpressionKind::Bool(b) => {
                let t = IOType::Constant(*b as i32);
                (graph.push_input_node("".to_string(), t.clone()), t) // TODO: It is ugly with "" being the variable name.
            },
            ExpressionKind::Variable(var) => {
                if let Some(var_node_vid) = self.search_scope(var.name.clone()) {
                    let var_node = graph.get_node(&var_node_vid).unwrap().clone();
                    let var_signal = self.variable_type_to_iotype(&var.type_);
                    let input_signal = match var_node {
                        Node::Input(var_output_node) => var_output_node.input,
                        Node::Output(_) => {
                            let inputs = graph.get_inputs(&var_node_vid);
                            if inputs.len() != 1 {
                                panic!("Output nodes should have exactly ONE input");
                            }
                            inputs[0].clone()
                        },
                        Node::Inner(_) => panic!("Var nodes should be output or input nodes"),
                    };
                    let vid = graph.push_node(Node::Inner(InnerNode::new()));
                    graph.push_connection(var_node_vid, vid, Connection::Arithmetic(
                            ArithmeticConnection::new_convert(
                                input_signal,
                                var_signal.clone()
                                )));
                    (vid, var_signal)
                }
                else {
                    match &var.type_ {
                        VariableType::All => todo!(),
                        VariableType::Int(int_type) => match int_type {
                            VariableSignalType::Any => {
                                let signal = self.get_new_anysignal();
                                (graph.push_input_node(var.name.clone(), signal.clone()), signal)
                            },
                            VariableSignalType::Signal(s) => {
                                let signal = IOType::Signal(s.clone());
                                (graph.push_input_node(var.name.clone(), signal.clone()), signal)
                            }
                        },
                        VariableType::Bool(bool_type) => match bool_type {
                            VariableSignalType::Any => {
                                let signal = self.get_new_anysignal();
                                (graph.push_input_node(var.name.clone(), signal.clone()), signal)
                            },
                            VariableSignalType::Signal(s) => {
                                let signal = IOType::Signal(s.clone());
                                (graph.push_input_node(var.name.clone(), signal.clone()), signal)
                            }
                        }
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
            ExpressionKind::Parenthesized(expr) => self.compile_expression(graph, &expr.expression, out_type),
            ExpressionKind::Binary(bin_expr) => {
                let (left_vid, left_type) = self.compile_expression(graph, &bin_expr.left, None);
                let (mut right_vid, mut right_type) = self.compile_expression(graph, &bin_expr.right, None);

                // If the two inputs are of the same type, on mut be altered
                if left_type == right_type {
                    let new_right_vid = graph.push_inner_node();
                    let new_right_type = self.get_new_anysignal();
                    graph.push_connection(right_vid, new_right_vid, Connection::Arithmetic(
                            ArithmeticConnection::new_convert(right_type.clone(), new_right_type.clone())
                            ));
                    right_vid = new_right_vid;
                    right_type = new_right_type;
                }

                let operation = match bin_expr.operator.kind {
                    BinaryOperatorKind::Plus => ReturnValue::Int(ArithmeticOperation::Add),
                    BinaryOperatorKind::Minus => ReturnValue::Int(ArithmeticOperation::Subtract),
                    BinaryOperatorKind::Multiply => ReturnValue::Int(ArithmeticOperation::Multiply),
                    BinaryOperatorKind::Divide => ReturnValue::Int(ArithmeticOperation::Divide),
                    BinaryOperatorKind::LargerThan => ReturnValue::Bool(DeciderOperation::LargerThan),
                    BinaryOperatorKind::LargerThanOrEqual => ReturnValue::Bool(DeciderOperation::LargerThanOrEqual),
                    BinaryOperatorKind::LessThen => ReturnValue::Bool(DeciderOperation::LessThan),
                    BinaryOperatorKind::LessThenOrEqual => ReturnValue::Bool(DeciderOperation::LessThanOrEqual),
                    BinaryOperatorKind::Equals => ReturnValue::Bool(DeciderOperation::Equals),
                    BinaryOperatorKind::NotEquals => ReturnValue::Bool(DeciderOperation::NotEquals),

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

                // The connection doing the actual operation
                let op_connection = match operation {
                    ReturnValue::Int(op) => {
                        let arithmetic_connection = ArithmeticConnection::new(left_type, right_type, op, out_type.clone());
                        Connection::Arithmetic(arithmetic_connection)
                    },
                    ReturnValue::Bool(op) => {
                        let decider_connection = DeciderConnection::new(left_type, right_type, op, out_type.clone());
                        Connection::Decider(decider_connection)
                    },
                };
                graph.push_connection(input, output, op_connection);

                (output, out_type)
            },
            ExpressionKind::BlockLink(block_expr) => {
                let vars: Vec<(NId, IOType)> = block_expr.inputs.iter()
                    .map(|expr|
                         match expr.kind.clone() {
                            ExpressionKind::Variable(variable) => {
                                if let Some(var_vid) = self.search_scope(variable.name.clone()) {
                                    let t = self.variable_type_to_iotype(&variable.type_);
                                    (var_vid, t)
                                }
                                else { panic!("Block links requires defined variables."); }
                            }
                            ExpressionKind::Int(_) => self.compile_expression(graph, expr, None),
                            ExpressionKind::Bool(_) => self.compile_expression(graph, expr, None),
                            ExpressionKind::Binary(_) => self.compile_expression(graph, expr, None),
                            ExpressionKind::Parenthesized(_) => self.compile_expression(graph, expr, None),
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
                            ExpressionKind::BlockLink(_) => self.compile_expression(graph, expr, None),
                            ExpressionKind::Error => panic!("Compilation should have stopped before this step if errors where encountered."),
                        }
                    ).collect();

                let outputs = match self.get_block_graph(&block_expr.block.name) {
                    Some(block_graph) => {
                        graph.stitch_graph(block_graph, vars).expect("Wrong number of arguments for function")
                    },
                    None => {
                        panic!("Block not defined.");
                    },
                };
                if outputs.len() != 1 { todo!("Blocks with multipule outputs are not implemented yet"); }
                outputs[0].clone()
            },
            ExpressionKind::Error => panic!("No errors shoud exist when compiling, as they should have stopped the after building the AST."),
        }
    }

    fn variable_type_to_iotype(&mut self, variable_type: &VariableType) -> IOType {
        match variable_type {
            VariableType::Int(int_type) => match int_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Bool(bool_type) => match bool_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::All => IOType::All,
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
            let var_iotype = self.variable_type_to_iotype(&input_var.type_.clone());
            let input_vid = graph.push_input_node(var_name.clone(), var_iotype);
            self.add_to_scope(var_name, input_vid)
        }
        for statement in &block.statements {
            match &statement.kind {
                StatementKind::Block(_) => {
                    self.diagnostics_bag.borrow_mut().report_error(
                        &statement.span,
                        "A `block` cannot be defined within another block.",
                    );
                }
                StatementKind::Assignment(assignment) => {
                    let out_type = self.variable_type_to_iotype(&assignment.variable.type_);
                    let (mut output_vid, _) = self.compile_expression(
                        &mut graph,
                        &assignment.expression,
                        Some(out_type.clone()),
                    );
                    let output_node = graph.get_node(&output_vid).unwrap().clone();
                    match output_node {
                        Node::Inner(_n) => {
                            // Convert inner node to output node
                            let input_type = graph.get_single_input(&output_vid).unwrap();
                            let var_out_node = OutputNode::new(out_type.clone());
                            let middle_vid = output_vid;
                            output_vid = graph.push_node(Node::Inner(InnerNode::new()));
                            graph.push_connection(
                                middle_vid,
                                output_vid,
                                Connection::Arithmetic(ArithmeticConnection::new_convert(
                                    input_type, out_type,
                                )),
                            );
                            graph.override_node(output_vid, Node::Output(var_out_node));
                        }
                        Node::Input(n) => {
                            let var_node = Node::Output(OutputNode::new(out_type.clone()));
                            let var_node_vid = graph.push_node(var_node);
                            graph.push_connection(
                                output_vid,
                                var_node_vid,
                                Connection::Arithmetic(ArithmeticConnection::new_convert(
                                    n.input, out_type,
                                )),
                            );
                            output_vid = var_node_vid;
                        }
                        Node::Output(n) => {
                            let var_node_vid = graph.push_node(Node::Output(n.clone()));
                            graph.push_connection(
                                output_vid,
                                var_node_vid,
                                Connection::Arithmetic(ArithmeticConnection::new_convert(
                                    n.output_type,
                                    out_type,
                                )),
                            );
                            output_vid = var_node_vid;
                        }
                    }
                    self.add_to_scope(assignment.variable.name.clone(), output_vid);
                }
                StatementKind::Error => {
                    panic!("There should not be error statements when compilation has started.")
                }
                StatementKind::Out(expr) => {
                    let out_variable_type = block
                        .outputs
                        .get(0)
                        .expect("Blocks can only have exactly ONE output for now.");
                    let out_iotype = self.variable_type_to_iotype(out_variable_type);
                    let (out_vid, _out_node) =
                        self.compile_expression(&mut graph, expr, Some(out_iotype.clone()));
                    graph
                        .vertices
                        .insert(out_vid, Node::Output(OutputNode::new(out_iotype)));
                }
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
