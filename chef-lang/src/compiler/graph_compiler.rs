use std::collections::HashMap;

use crate::ast::{
    Assignment, BinaryExpression, BinaryOperator, Block, BlockLinkExpression, Expression,
    ExpressionKind, IndexExpression, Mutation, PickExpression, VarOperation, VariableRef,
    VariableSignalType, WhenExpression, AST,
};
use crate::ast::{Statement, StatementKind, VariableType};
use crate::compiler::graph::*;
use crate::diagnostics::{CompilationError, CompilationResult};

use super::RESERVED_SIGNAL;

const REGISTER_INPUT_TAG: i32 = -1;
const REGISTER_SHIFT_TAG: i32 = -2;

pub struct GraphCompiler {
    ast: AST,
    next_anysignal: u64,
    block_graphs: HashMap<String, Graph>,
    scopes: Vec<HashMap<(String, Option<i32>), NId>>,
}

impl GraphCompiler {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            next_anysignal: 0,
            block_graphs: HashMap::new(),
            scopes: vec![HashMap::new()],
        }
    }

    pub fn compile(&mut self) -> Result<Graph, CompilationError> {
        for block in self.ast.blocks.clone() {
            let block_graph = self.compile_block(&block)?;
            self.add_block_graph(block.name.clone(), block_graph);
        }
        self.get_graph()
    }

    fn compile_block(&mut self, block: &Block) -> Result<Graph, CompilationError> {
        let mut graph = Graph::new();
        self.enter_scope();
        for input_var in block.inputs.clone() {
            let var_name = input_var.name.clone();
            let var_iotype = self.variable_type_to_iotype(&input_var.type_.clone());
            let input_vid = graph.push_input_node(var_iotype);
            self.add_to_scope(var_name, None, input_vid)
        }
        for statement in &block.statements {
            self.compile_statement(&mut graph, statement, None)?;
        }

        let (out_expr_nid, out_expr_type) =
            self.compile_expression(&mut graph, &block.output, None)?;
        let block_out_type = self.variable_type_to_iotype(&block.output_type);
        let block_out_nid = graph.push_output_node(block_out_type.clone());

        let (c_input, c_ouput) =
            graph.push_connection(Connection::new_convert(out_expr_type, block_out_type));

        graph.push_wire(out_expr_nid, c_input, WireKind::Green);

        // TODO: Use correct wire
        graph.push_wire(c_ouput, block_out_nid, WireKind::Green);

        self.exit_scope();
        Ok(graph)
    }

    fn compile_statement(
        &mut self,
        graph: &mut Graph,
        statement: &Statement,
        gate: Option<(NId, IOType)>,
    ) -> Result<(), CompilationError> {
        match statement.kind.clone() {
            StatementKind::Expression(expr) => {
                self.compile_expression(graph, &expr, None)?;
            }
            StatementKind::Assignment(operation) => {
                self.compile_assignment_statement(graph, operation)?;
            }
            StatementKind::Mutation(mutation_statement) => {
                self.compile_mutation_statement(graph, mutation_statement, gate)?;
            }
            StatementKind::Operation(operation) => self.compile_variable_operation(operation)?,
            StatementKind::Out(expr) => {
                let (expr_nid, out_type) = self.compile_expression(graph, &expr, None)?;
                let out_nid = graph.push_output_node(out_type.clone());
                let (c_input, c_ouput) = graph.push_connection(Connection::new_pick(out_type));
                graph.push_wire(expr_nid, c_input, WireKind::Green);
                graph.push_wire(c_ouput, out_nid, WireKind::Green);
            }
            StatementKind::Error => {
                panic!("There should not be error statements when compilation has started.")
            }
        };
        Ok(())
    }

    fn compile_assignment_statement(
        &mut self,
        graph: &mut Graph,
        operation: Assignment,
    ) -> Result<(), CompilationError> {
        let var = &operation.variable;

        match &var.type_ {
            VariableType::Var(_) => {
                let var_type = self.variable_type_to_iotype(&var.type_);
                let var_nid = graph.push_var_node(var_type.clone());
                let (c_input, c_output) = graph.push_connection(Connection::new_pick(var_type));
                graph.push_wire(c_input, c_output, WireKind::Green); // Create loop
                graph.push_wire(c_input, var_nid, WireKind::Green);
                self.add_to_scope(var.name.clone(), None, var_nid);
                return Ok(());
            }
            VariableType::Counter((_, limit_expr)) => {
                let var_type = self.variable_type_to_iotype(&var.type_);

                // Get counter limit
                let (limit_nid, limit_type) = self.compile_expression(graph, limit_expr, None)?;

                let var_nid = graph.push_var_node(var_type.clone());

                assert_ne!(limit_type, var_type);

                // Connect up the memory cell
                let if_less_than_limit = Connection::new_gate(GateCombinator {
                    left: var_type.clone().to_combinator_type(),
                    right: limit_type.clone().to_combinator_type(),
                    operation: DeciderOperation::LessThan,
                    gate_type: var_type.clone(),
                });
                let (memcell_input, memcell_output) = graph.push_connection(if_less_than_limit);
                graph.push_wire(memcell_input, memcell_output, WireKind::Red); // Create loop
                graph.push_wire(memcell_input, var_nid, WireKind::Green);

                // Connect the limit to the memory cell
                graph.push_wire(limit_nid, memcell_input, WireKind::Red);

                // Push constant node to drive the counter
                let driver_nid = graph.push_node(Node::Constant(var_type.to_constant(1).unwrap()));
                graph.push_wire(driver_nid, memcell_input, WireKind::Red);

                self.add_to_scope(var.name.clone(), None, var_nid);
                return Ok(());
            }
            VariableType::Register(size) if operation.attr.is_none() => {
                let mut prev = None;

                for i in 0..*size {
                    let (c1_input, c1_output) =
                        graph.push_combinator(Combinator::Gate(GateCombinator {
                            left: IOType::Signal(RESERVED_SIGNAL.to_string()),
                            right: IOType::Constant(0),
                            operation: DeciderOperation::Equals,
                            gate_type: IOType::Everything,
                        }));

                    let (c2_input, c2_output) =
                        graph.push_combinator(Combinator::Gate(GateCombinator {
                            left: IOType::Signal(RESERVED_SIGNAL.to_string()),
                            right: IOType::Constant(1),
                            operation: DeciderOperation::Equals,
                            gate_type: IOType::Everything,
                        }));

                    // Red wires
                    graph.push_wire(c1_input, c2_input, WireKind::Red);
                    graph.push_wire(c1_output, c2_output, WireKind::Red);

                    // Green wires
                    graph.push_wire(c1_input, c1_output, WireKind::Green);
                    graph.push_wire(c1_output, c2_input, WireKind::Green);

                    if let Some((prev_c1_input, prev_c2_output)) = prev {
                        graph.push_wire(prev_c1_input, c1_input, WireKind::Red);
                        graph.push_wire(prev_c2_output, c2_input, WireKind::Green)
                    } else {
                        // On first iteration only, connect input

                        {
                            // Create input
                            let input_nid = graph.push_inner_node();
                            self.add_to_scope(
                                var.name.clone(),
                                Some(REGISTER_INPUT_TAG),
                                input_nid,
                            );

                            graph.push_wire(input_nid, c1_input, WireKind::Green);
                        }

                        {
                            // Add shift input

                            // Convert whatever the input signal is, to the reserved signal
                            let (shift_nid, n1) =
                                graph.push_combinator(Combinator::Decider(DeciderCombinator {
                                    left: IOType::Anything,
                                    right: IOType::Constant(0),
                                    operation: DeciderOperation::NotEquals,
                                    output: IOType::Signal(RESERVED_SIGNAL.to_string()),
                                }));

                            graph.push_wire(n1, c1_input, WireKind::Green);

                            // Add combonator to only shift once
                            let (n2, n3) = graph.push_combinator(Combinator::Arithmetic(
                                ArithmeticCombinator {
                                    left: IOType::Signal(RESERVED_SIGNAL.to_string()),
                                    right: IOType::Constant(-1),
                                    operation: ArithmeticOperation::Multiply,
                                    output: IOType::Signal(RESERVED_SIGNAL.to_string()),
                                },
                            ));
                            graph.push_wire(n1, n2, WireKind::Green);
                            graph.push_wire(n1, n3, WireKind::Red);

                            self.add_to_scope(var.name.clone(), Some(REGISTER_SHIFT_TAG), shift_nid)
                        }
                    }

                    self.add_to_scope(var.name.clone(), Some(i as i32), c2_output);

                    prev = Some((c1_input, c2_output));
                }
                Ok(())
            }
            // Attribute definitions
            VariableType::Register(_) => {
                let nid = match operation
                    .attr
                    .expect("Should have been handled by the case above")
                    .as_str()
                {
                    "input" => {
                        self.search_scope(operation.variable.name.clone(), Some(REGISTER_INPUT_TAG))
                    }
                    "shift" => {
                        self.search_scope(operation.variable.name.clone(), Some(REGISTER_SHIFT_TAG))
                    }
                    _ => panic!(
                        "Invalid attribute. This should have been caught by the type checker."
                    ),
                }
                // TODO: Make localized
                .ok_or(CompilationError::new_generic(format!(
                    "Can not assign attrubute to undefined variable: `{}`.",
                    &var.name
                )))?;
                let expr = operation.expression.unwrap();

                // TODO: Define output type for input
                let (expr_out_nid, _) = self.compile_expression(graph, &expr, None)?;

                graph.push_wire(expr_out_nid, nid, WireKind::Green);

                Ok(())
            }
            VariableType::ConstInt(_) | VariableType::ConstBool(_) => Ok(()),
            VariableType::Bool(_) | VariableType::Int(_) | VariableType::All => {
                let var_type = self.variable_type_to_iotype(&var.type_);

                let (expr_out_nid, expr_out_type) = self.compile_expression(
                    graph,
                    &operation.expression.unwrap(),
                    Some(var_type.clone()),
                )?;

                let var_node_nid = graph.push_var_node(var_type.clone());

                // Connect the output of the expression to the input of the variable
                let (c_input, c_output) = graph.push_connection(Connection::new_arithmetic(
                    ArithmeticCombinator::new_convert(expr_out_type, var_type),
                ));

                graph.push_wire(c_input, expr_out_nid, WireKind::Green);
                graph.push_wire(c_output, var_node_nid, WireKind::Green);

                self.add_to_scope(operation.variable.name.clone(), None, var_node_nid);
                Ok(())
            }
            VariableType::Attr(_) => todo!(),
        }
    }

    fn compile_mutation_statement(
        &mut self,
        graph: &mut Graph,
        mutation_statement: Mutation,
        gate: Option<(NId, IOType)>,
    ) -> Result<(), CompilationError> {
        // TODO: Create test for expect
        let var_nid = self.search_scope(mutation_statement.var_ref.var.name.clone(), None).expect("The parser should make sure that mutation statements only happen on defined variables.");
        let var_type = mutation_statement.var_ref.var.type_.clone();
        let var_iotype = self.variable_type_to_iotype(&var_type);

        let (expr_out_nid, expr_out_type) =
            self.compile_expression(graph, &mutation_statement.expression, None)?;

        // TODO: convert expr_out_type if this happens
        assert_ne!(&var_iotype, &expr_out_type);

        let conn = match mutation_statement.operator {
            crate::ast::MutationOperator::Add => {
                Connection::new_convert(expr_out_type.clone(), var_iotype.clone())
            }

            // Multiply by -1 if subtracting
            crate::ast::MutationOperator::Subtract => {
                Connection::new_arithmetic(ArithmeticCombinator::new(
                    expr_out_type.clone(),
                    IOType::Constant(-1),
                    ArithmeticOperation::Multiply,
                    var_iotype.clone(),
                ))
            }
        };

        match gate {
            Some((condition_nid, condition_type)) => {
                // TODO: convert one if this happens
                assert_ne!(&condition_type, &var_iotype);

                let (c_input, new_out_nid) = graph.push_connection(conn);
                graph.push_wire(expr_out_nid, c_input, WireKind::Green);
                let expr_out_nid = new_out_nid; // Expression output is now gated

                // Add the gate
                let (gate_input, gate_output) =
                    graph.push_gate_connection(condition_type, var_iotype);

                // Wire up the gate
                graph.push_wire(gate_input, condition_nid, WireKind::Green);
                graph.push_wire(gate_input, expr_out_nid, WireKind::Green);
                graph.push_wire(gate_output, var_nid, WireKind::Green);
            }
            None => {
                let (c_input, c_output) = graph.push_connection(conn);
                graph.push_wire(c_input, expr_out_nid, WireKind::Green);
                graph.push_wire(c_output, var_nid, WireKind::Green);
            }
        };

        Ok(())
    }

    fn compile_variable_operation(&mut self, operation: VarOperation) -> CompilationResult<()> {
        let var = operation.var_ref.var;

        match &var.type_ {
            VariableType::Register(_) => todo!(),
            t => panic!("Cannot operate on type '{t}'."),
        }
    }

    /// Returns a typle:: (output_vid, output_type)
    fn compile_expression(
        &mut self,
        graph: &mut Graph,
        expr: &Expression,
        out_type: Option<IOType>,
    ) -> Result<(NId, IOType), CompilationError> {
        match &expr.kind {
            ExpressionKind::Int(n) => self.compile_constant(graph, *n),
            ExpressionKind::Bool(b) => self.compile_constant(graph, *b as i32),
            ExpressionKind::VariableRef(var_ref) => self.compile_variable_ref_expression(graph, var_ref), // 
            ExpressionKind::Pick(pick_expr) => self.compile_pick_expression(graph, pick_expr),
            ExpressionKind::Index(index_expr) => self.compile_index_expression(graph, index_expr),
            ExpressionKind::Parenthesized(expr) => self.compile_expression(graph, &expr.expression, out_type),
            ExpressionKind::Negative(expr) => self.compile_negative_expression(graph, expr, out_type),
            ExpressionKind::Binary(bin_expr) => self.compile_binary_expression(graph, bin_expr, out_type),
            // TODO: use out_type in all compilation functions
            ExpressionKind::BlockLink(block_link_expr) => self.compile_block_link_expression(graph, block_link_expr),
            ExpressionKind::When(when) => self.compile_when_expression(graph, when),
            ExpressionKind::Error => panic!("No errors shoud exist when compiling, as they should have stopped the after building the AST."),
        }
    }

    fn compile_constant(
        &mut self,
        graph: &mut Graph,
        number: i32,
    ) -> Result<(NId, IOType), CompilationError> {
        let iotype = self.get_new_const_anysignal(number);
        let const_nid = graph.push_node(Node::Constant(iotype.clone()));
        Ok((const_nid, iotype))
    }

    fn compile_variable_ref_expression(
        &mut self,
        graph: &mut Graph,
        var_ref: &VariableRef,
    ) -> Result<(NId, IOType), CompilationError> {
        // Get the referenced variable.
        let var = var_ref.var.clone();
        let var_node_nid = self
            .search_scope(var.name.clone(), None)
            .expect("Variable references should always point to defined variables");
        let var_node = graph.get_node(&var_node_nid).unwrap();

        // Get the signal type of the var node.
        let var_type = match var_node {
            Node::InputVariable(input_type) => input_type,
            Node::Variable(var_type) => var_type,
            _ => panic!("Var nodes should be `Variable` or `InputVariable` nodes"),
        };

        Ok((var_node_nid, var_type.clone()))
    }

    fn compile_negative_expression(
        &mut self,
        graph: &mut Graph,
        expr: &Expression,
        out_type: Option<IOType>,
    ) -> Result<(NId, IOType), CompilationError> {
        let out_type = match out_type {
            Some(t) => t,
            None => self.get_new_anysignal(),
        };

        let (expr_out_nid, _) = self.compile_expression(graph, expr, Some(out_type.clone()))?;

        let (c_input, negative_out_nid) = graph.push_connection(
            // expr_out_nid,
            // negative_out_nid,
            Connection::new_arithmetic(ArithmeticCombinator::new(
                out_type.clone(),
                IOType::Constant(-1),
                ArithmeticOperation::Multiply,
                out_type.clone(),
            )),
        );

        graph.push_wire(expr_out_nid, c_input, WireKind::Green);

        Ok((negative_out_nid, out_type))
    }

    fn compile_pick_expression(
        &mut self,
        graph: &mut Graph,
        pick_expr: &PickExpression,
    ) -> Result<(NId, IOType), CompilationError> {
        let var_ref = pick_expr.from.clone();
        if let Some(var_out_nid) = self.search_scope(var_ref.var.name.clone(), None) {
            let out_type = IOType::signal(pick_expr.pick_signal.clone());
            let (c_input, picked_nid) = graph.push_connection(Connection::new_arithmetic(
                ArithmeticCombinator::new_pick(out_type.clone().to_combinator_type()),
            ));
            graph.push_wire(var_out_nid, c_input, WireKind::Green);
            Ok((picked_nid, out_type))
        } else {
            Err(CompilationError::new_localized(
                format!("No variable with the name \'{}\', ", var_ref.var.name),
                var_ref.span,
            ))
        }
    }

    fn compile_index_expression(
        &mut self,
        _graph: &mut Graph,
        index_expr: &IndexExpression,
    ) -> Result<(NId, IOType), CompilationError> {
        let index = index_expr.index;
        if let Some(indexed_output) =
            self.search_scope(index_expr.var_ref.var.name.clone(), Some(index as i32))
        {
            // TODO: Make type depend on register input type
            Ok((indexed_output, IOType::Everything))
        } else {
            Err(CompilationError::new_localized(
                "Index out of range.",
                index_expr.var_ref.span.clone(),
            ))
        }
    }

    fn compile_binary_expression(
        &mut self,
        graph: &mut Graph,
        bin_expr: &BinaryExpression,
        out_type: Option<IOType>,
    ) -> Result<(NId, IOType), CompilationError> {
        let (left_nid, left_type) = self.compile_expression(graph, &bin_expr.left, None)?;
        let (mut right_nid, mut right_type) =
            self.compile_expression(graph, &bin_expr.right, None)?;

        // If the two inputs are of the same type, one must be converted.
        if left_type == right_type {
            todo!("Convert binary expressons on the same type");
            //     let new_right_vid = graph.push_inner_node();
            //     let new_right_type = self.get_new_anysignal();
            //     graph.push_connection(
            //         right_vid,
            //         new_right_vid,
            //         Connection::new_arithmetic(ArithmeticCombinator::new_convert(
            //             right_type.clone(),
            //             new_right_type.clone(),
            //         )),
            //     );
            //     right_vid = new_right_vid;
            //     right_type = new_right_type;
        }

        // let input_nid = graph.push_inner_node();
        // let output_nid = graph.push_inner_node();

        // Connect the outputs of the left and right expressions to the inputs.
        // graph.push_connection(
        //     left_vid,
        //     input_nid,
        //     Connection::new_arithmetic(ArithmeticCombinator::new_pick(left_type.clone())),
        // );
        // graph.push_connection(
        //     right_vid,
        //     input_nid,
        //     Connection::new_arithmetic(ArithmeticCombinator::new_pick(right_type.clone())),
        // );

        // Use the outtype if any was provided.
        let out_type = if let Some(t) = out_type {
            t
        } else {
            self.get_new_anysignal()
        };

        // Get the combinator operation
        let operation = match bin_expr.operator {
            BinaryOperator::Add => ReturnValue::Int(ArithmeticOperation::Add),
            BinaryOperator::Subtract => ReturnValue::Int(ArithmeticOperation::Subtract),
            BinaryOperator::Multiply => ReturnValue::Int(ArithmeticOperation::Multiply),
            BinaryOperator::Divide => ReturnValue::Int(ArithmeticOperation::Divide),
            BinaryOperator::LargerThan => ReturnValue::Bool(DeciderOperation::LargerThan),
            BinaryOperator::LargerThanOrEqual => {
                ReturnValue::Bool(DeciderOperation::LargerThanOrEqual)
            }
            BinaryOperator::LessThan => ReturnValue::Bool(DeciderOperation::LessThan),
            BinaryOperator::LessThanOrEqual => ReturnValue::Bool(DeciderOperation::LessThanOrEqual),
            BinaryOperator::Equals => ReturnValue::Bool(DeciderOperation::Equals),
            BinaryOperator::NotEquals => ReturnValue::Bool(DeciderOperation::NotEquals),
            BinaryOperator::Combine => ReturnValue::Group,
        };

        // The connection doing the actual operation
        Ok(match operation {
            ReturnValue::Int(op) => {
                let arithmetic_connection = ArithmeticCombinator::new(
                    left_type.to_combinator_type(),
                    right_type.to_combinator_type(),
                    op,
                    out_type.clone(),
                );
                let op_connection = Connection::new_arithmetic(arithmetic_connection);
                let (c_input, output_nid) = graph.push_connection(op_connection);
                graph.push_wire(c_input, left_nid, WireKind::Green);
                graph.push_wire(c_input, right_nid, WireKind::Green);
                (output_nid, out_type)
            }
            ReturnValue::Bool(op) => {
                let decider_connection = DeciderCombinator::new(
                    left_type.to_combinator_type(),
                    right_type.to_combinator_type(),
                    op,
                    out_type.clone(),
                );
                let op_connection = Connection::new_decider(decider_connection);
                let (c_input, output_nid) = graph.push_connection(op_connection);
                graph.push_wire(c_input, left_nid, WireKind::Green);
                graph.push_wire(c_input, right_nid, WireKind::Green);
                (output_nid, out_type)
            }
            ReturnValue::Group => {
                graph.push_wire(left_nid, right_nid, WireKind::Green);
                let common_nid = left_nid;
                (common_nid, IOType::Everything)
            }
        })
    }

    fn compile_block_link_expression(
        &mut self,
        graph: &mut Graph,
        block_link_expr: &BlockLinkExpression,
    ) -> Result<(NId, IOType), CompilationError> {
        let mut vars: Vec<(NId, IOType)> = Vec::new();
        for expr in block_link_expr.inputs.iter() {
            let pair = self.compile_expression(graph, expr, None)?;
            vars.push(pair);
        }

        let outputs = match self.get_block_graph(&block_link_expr.block.name) {
            Some(block_graph) => match graph.stitch_graph(block_graph, vars) {
                Ok(v) => v,
                Err(e) => {
                    panic!("Errored in stitch_graph: {}.", e) // TODO: handle correctly
                                                              // return Err(CompilationError::new(e, expr.span.clone()))
                }
            },
            None => {
                panic!("Block not defined.");
            }
        };
        if outputs.len() != 1 {
            todo!("Blocks with multipule outputs are not implemented yet");
        }
        Ok(outputs[0].clone())
    }

    fn compile_when_expression(
        &mut self,
        graph: &mut Graph,
        when: &WhenExpression,
    ) -> Result<(NId, IOType), CompilationError> {
        self.enter_scope();

        // Compile condition expression.
        let cond_pair = self.compile_expression(graph, &when.condition, None)?;

        // Compile statements
        for statement in &when.statements {
            self.compile_statement(graph, statement, Some(cond_pair.clone()))?;
        }

        let out_expr = match &when.out {
            Some(e) => e,
            None => {
                // If the there is no output, we can skip creating the gate. In this case we just return
                // the condition output node.
                return Ok(cond_pair);
            }
        };

        // Compile output expression, we will attatch a gate to the output of this.
        let (expr_out_nid, gated_type) = self.compile_expression(graph, out_expr, None)?;

        // If the gated type is a constant, convert it as we can not gate a constant value
        if let IOType::Constant(count) = gated_type {
            todo!()
            // // let convertion_node = graph.push_inner_node();
            // gated_type = self.get_new_anysignal();
            // let (conversion_node) = graph.push_connection(
            //     // convertion_node,
            //     // gated_input_nid,
            //     Connection::Combinator(Combinator::Constant(ConstantCombinator {
            //         type_: gated_type.clone(),
            //         count,
            //     })),
            // );
        }

        let (cond_out_nid, cond_out_type) = cond_pair;

        // We make sure that the cond type is not the same as the gated type
        assert_ne!(gated_type, cond_out_type); // TODO: catch

        // Connect the condition output to the gate input, so the gate can read the condition
        // state.
        graph.push_wire(cond_out_nid, expr_out_nid, WireKind::Green);

        // Push the actual gate opteration. Here we only let the signal through,
        // if the condition returns a value larger than zero.
        let (gate_input, out_nid) = graph.push_gate_connection(cond_out_type, gated_type.clone());
        graph.push_wire(expr_out_nid, gate_input, WireKind::Green);

        self.exit_scope();
        Ok((out_nid, gated_type))
    }

    fn variable_type_to_iotype(&mut self, variable_type: &VariableType) -> IOType {
        match variable_type {
            VariableType::Bool(bool_type) => match bool_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Int(int_type) => match int_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Var(var_type) => match var_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Attr(var_type) => match var_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Counter((var_type, _lim)) => match var_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::All => IOType::Everything,
            VariableType::Register(_) => IOType::Everything, // TODO: make dependent on the type of input
            // expression
            VariableType::ConstInt(_) => {
                panic!("ConstInt expression should never need to be converted to IOType.")
            }
            VariableType::ConstBool(_) => {
                panic!("ConstBool expression should never need to be converted to IOType.")
            }
        }
    }

    pub fn get_graph(&self) -> Result<Graph, CompilationError> {
        match self.block_graphs.get("main") {
            Some(g) => Ok(g.clone()),
            None => match self.ast.blocks.is_empty() {
                true => Err(CompilationError::new_generic("No statements in program.")),
                false => Err(CompilationError::new_generic(
                    "No `main` block found. All chef programs must have a `main` block.",
                )),
            },
        }
    }

    fn add_block_graph(&mut self, name: String, graph: Graph) {
        self.block_graphs.insert(name, graph);
    }

    fn get_block_graph(&mut self, name: &String) -> Option<&Graph> {
        self.block_graphs.get(name)
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    fn add_to_scope(&mut self, variable_name: String, tag: Option<i32>, nid: NId) {
        if self
            .scopes
            .last_mut()
            .unwrap()
            .insert((variable_name, tag), nid)
            .is_some()
        {
            panic!("tried to override a variable in scope.")
        }
    }

    fn search_scope(&self, variable_name: String, tag: Option<i32>) -> Option<NId> {
        let scopes_len = self.scopes.len();
        for i in 0..scopes_len {
            let p = scopes_len - i - 1;
            if let Some(vid) = self
                .scopes
                .get(p)
                .unwrap()
                .get(&(variable_name.clone(), tag))
            {
                return Some(*vid);
            }
        }
        None
    }

    fn get_new_anysignal(&mut self) -> IOType {
        let signal = IOType::AnySignal(self.next_anysignal);
        self.next_anysignal += 1;
        signal
    }

    fn get_new_const_anysignal(&mut self, n: i32) -> IOType {
        let signal = IOType::ConstantAny((self.next_anysignal, n));
        self.next_anysignal += 1;
        signal
    }
}

pub enum ReturnValue<A, B> {
    Int(A),
    Bool(B),
    Group,
}
