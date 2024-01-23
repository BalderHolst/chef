use std::collections::HashMap;

use crate::ast::{
    Assignment, AssignmentKind, BinaryExpression, BinaryOperator, Block, BlockLinkExpression,
    Expression, ExpressionKind, IndexExpression, Mutation, PickExpression, VariableRef,
    VariableSignalType, WhenExpression, AST,
};
use crate::ast::{Statement, StatementKind, VariableType};
use crate::compiler::graph::*;
use crate::diagnostics::CompilationError;

/// A signal used for gates that allow all signals through. All except this reserved signal of
/// course.
const RESERVED_GATE_SIGNAL: &str = "signal-dot";

pub struct GraphCompiler {
    ast: AST,
    next_anysignal: u64,
    block_graphs: HashMap<String, Graph>,
    scopes: Vec<HashMap<String, NId>>,
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
            self.add_to_scope(var_name, input_vid)
        }
        for statement in &block.statements {
            self.compile_statement(&mut graph, statement, None)?;
        }

        let (out_expr_nid, out_expr_type) =
            self.compile_expression(&mut graph, &block.output, None)?;
        let block_out_type = self.variable_type_to_iotype(&block.output_type);
        let block_out_nid = graph.push_output_node(block_out_type.clone());
        graph.push_connection(
            out_expr_nid,
            block_out_nid,
            Connection::new_convert(out_expr_type, block_out_type),
        );

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
            StatementKind::Assignment(assignment) => {
                self.compile_assignment_statement(graph, assignment)?;
            }
            StatementKind::Mutation(mutation_statement) => {
                self.compile_mutation_statement(graph, mutation_statement, gate)?;
            }
            StatementKind::Out(expr) => {
                let (expr_nid, out_type) = self.compile_expression(graph, &expr, None)?;
                let out_nid = graph.push_output_node(out_type.clone());
                graph.push_connection(expr_nid, out_nid, Connection::new_pick(out_type));
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
        assignment: Assignment,
    ) -> Result<(), CompilationError> {
        let var = &assignment.variable;

        if matches!(
            &var.type_,
            VariableType::ConstInt(_) | VariableType::ConstBool(_)
        ) {
            return Ok(());
        }

        let var_type = self.variable_type_to_iotype(&var.type_);

        // Add connections to self for counters and var variables
        match &assignment.kind {
            AssignmentKind::Sig => {}
            AssignmentKind::Var => {
                let var_nid = graph.push_var_node(var_type.clone());
                graph.push_connection(var_nid, var_nid, Connection::new_pick(var_type));
                self.add_to_scope(var.name.clone(), var_nid);
                return Ok(());
            }
            AssignmentKind::Counter => {
                let (limit_nid, limit_type) =
                    if let VariableType::Counter((_, limit_expr)) = &var.type_ {
                        self.compile_expression(graph, limit_expr, None)?
                    } else {
                        panic!("Counter assignment should be to counter variables.")
                    };
                let var_nid = graph.push_var_node(var_type.clone());

                // Connect up the memory cell
                let if_less_than_limit = Connection::new_gate(GateCombinator {
                    left: var_type.clone(),
                    right: limit_type.clone(),
                    operation: DeciderOperation::LessThan,
                    gate_type: var_type.clone(),
                });
                graph.push_connection(var_nid, var_nid, if_less_than_limit);

                // Connect the limit to the memory cell
                graph.push_connection(limit_nid, var_nid, Connection::new_pick(limit_type));

                // Push constant node, to drive the counter.
                let driver_nid = graph.push_node(Node::Constant(var_type.clone()));
                let conn = Connection::Combinator(Combinator::Constant(ConstantCombinator {
                    type_: var_type,
                    count: 1,
                }));
                graph.push_connection(driver_nid, var_nid, conn);

                self.add_to_scope(var.name.clone(), var_nid);
                return Ok(());
            }
            AssignmentKind::Register => todo!(),
        }

        let (expr_out_vid, expr_out_type) =
            self.compile_expression(graph, &assignment.expression, Some(var_type.clone()))?;

        let var_node_vid = graph.push_var_node(var_type.clone());

        // Connect the output of the expression to the input of the variable
        graph.push_connection(
            expr_out_vid,
            var_node_vid,
            Connection::new_arithmetic(ArithmeticCombinator::new_convert(expr_out_type, var_type)),
        );

        self.add_to_scope(assignment.variable.name.clone(), var_node_vid);
        Ok(())
    }

    fn compile_mutation_statement(
        &mut self,
        graph: &mut Graph,
        mutation_statement: Mutation,
        gate: Option<(NId, IOType)>,
    ) -> Result<(), CompilationError> {
        // TODO: Create test for expect
        let var_nid = self.search_scope(mutation_statement.var_ref.var.name.clone()).expect("The parser should make sure that mutation statements only happen on defined variables.");
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

                let expr_out_nid = {
                    let new_out_nid = graph.push_inner_node();
                    graph.push_connection(expr_out_nid, new_out_nid, conn);
                    new_out_nid
                };

                graph.push_connection(
                    condition_nid,
                    expr_out_nid,
                    Connection::new_pick(condition_type.clone()),
                );
                graph.push_gate_connection(expr_out_nid, var_nid, condition_type, var_iotype)
            }
            None => graph.push_connection(expr_out_nid, var_nid, conn),
        };

        Ok(())
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
        let iotype = IOType::Constant(number);
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
            .search_scope(var.name.clone())
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
        let negative_out_nid = graph.push_inner_node();
        graph.push_connection(
            expr_out_nid,
            negative_out_nid,
            Connection::new_arithmetic(ArithmeticCombinator::new(
                out_type.clone(),
                IOType::Constant(-1),
                ArithmeticOperation::Multiply,
                out_type.clone(),
            )),
        );

        Ok((negative_out_nid, out_type))
    }

    fn compile_pick_expression(
        &mut self,
        graph: &mut Graph,
        pick_expr: &PickExpression,
    ) -> Result<(NId, IOType), CompilationError> {
        let var_ref = pick_expr.from.clone();
        if let Some(var_out_vid) = self.search_scope(var_ref.var.name.clone()) {
            let out_type = IOType::signal(pick_expr.pick_signal.clone());
            let picked_vid = graph.push_inner_node();
            graph.push_connection(
                var_out_vid,
                picked_vid,
                Connection::new_arithmetic(ArithmeticCombinator::new_pick(out_type.clone())),
            );
            Ok((picked_vid, out_type))
        } else {
            Err(CompilationError::new_localized(
                format!("No variable with the name \'{}\', ", var_ref.var.name),
                var_ref.span,
            ))
        }
    }

    fn compile_index_expression(
        &mut self,
        graph: &mut Graph,
        index_expr: &IndexExpression,
    ) -> Result<(NId, IOType), CompilationError> {
        match &index_expr.var_ref.var.type_ {
            VariableType::Register(index) => {
                todo!("indexing: {index}")
            }
            _ => {
                return Err(CompilationError::new_localized(
                    format!(
                        "Cannot index variable type: {}",
                        index_expr.var_ref.var.type_
                    ),
                    index_expr.var_ref.span.clone(),
                ))
            }
        }
    }

    fn compile_binary_expression(
        &mut self,
        graph: &mut Graph,
        bin_expr: &BinaryExpression,
        out_type: Option<IOType>,
    ) -> Result<(NId, IOType), CompilationError> {
        let (left_vid, left_type) = self.compile_expression(graph, &bin_expr.left, None)?;
        let (mut right_vid, mut right_type) =
            self.compile_expression(graph, &bin_expr.right, None)?;

        // If the two inputs are of the same type, one must be converted.
        if left_type == right_type {
            let new_right_vid = graph.push_inner_node();
            let new_right_type = self.get_new_anysignal();
            graph.push_connection(
                right_vid,
                new_right_vid,
                Connection::new_arithmetic(ArithmeticCombinator::new_convert(
                    right_type.clone(),
                    new_right_type.clone(),
                )),
            );
            right_vid = new_right_vid;
            right_type = new_right_type;
        }

        let input_nid = graph.push_inner_node();
        let output_nid = graph.push_inner_node();

        // Connect the outputs of the left and right expressions to the inputs.
        graph.push_connection(
            left_vid,
            input_nid,
            Connection::new_arithmetic(ArithmeticCombinator::new_pick(left_type.clone())),
        );
        graph.push_connection(
            right_vid,
            input_nid,
            Connection::new_arithmetic(ArithmeticCombinator::new_pick(right_type.clone())),
        );

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
                let arithmetic_connection =
                    ArithmeticCombinator::new(left_type, right_type, op, out_type.clone());
                let op_connection = Connection::new_arithmetic(arithmetic_connection);
                graph.push_connection(input_nid, output_nid, op_connection);
                (output_nid, out_type)
            }
            ReturnValue::Bool(op) => {
                let decider_connection =
                    DeciderCombinator::new(left_type, right_type, op, out_type.clone());
                let op_connection = Connection::new_decider(decider_connection);
                graph.push_connection(input_nid, output_nid, op_connection);
                (output_nid, out_type)
            }
            ReturnValue::Group => (input_nid, IOType::All),
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
        let (gated_input_nid, mut gated_type) = self.compile_expression(graph, out_expr, None)?;

        // If the gated type is a constant, convert it as we can not gate a constant value
        if let IOType::Constant(count) = gated_type {
            let convertion_node = graph.push_inner_node();
            gated_type = self.get_new_anysignal();
            graph.push_connection(
                convertion_node,
                gated_input_nid,
                Connection::Combinator(Combinator::Constant(ConstantCombinator {
                    type_: gated_type.clone(),
                    count,
                })),
            )
        }

        // Output of the gate. This output will be turned on and off by the condition statement.
        let out_nid = graph.push_inner_node();

        let (cond_out_nid, cond_out_type) = cond_pair;

        // We make sure that the cond type is not the same as the gated type
        assert_ne!(gated_type, cond_out_type); // TODO: catch

        // Connect the condition output to the gate input, so the gate can read the condition
        // state.
        graph.push_connection(
            cond_out_nid,
            gated_input_nid,
            Connection::new_pick(cond_out_type.clone()),
        );

        // Push the actual gate opteration. Here we only let the signal through,
        // if the condition returns a value larger than zero.
        graph.push_gate_connection(gated_input_nid, out_nid, cond_out_type, gated_type.clone());

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
            VariableType::Counter((var_type, _lim)) => match var_type {
                VariableSignalType::Signal(s) => IOType::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::All => IOType::All,
            VariableType::ConstInt(_) => {
                panic!("ConstInt expression should never need to be converted to IOType.")
            }
            VariableType::ConstBool(_) => {
                panic!("ConstBool expression should never need to be converted to IOType.")
            }
            VariableType::Register(size) => {
                todo!()
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

    fn get_new_anysignal(&mut self) -> IOType {
        let signal = IOType::AnySignal(self.next_anysignal);
        self.next_anysignal += 1;
        signal
    }
}

pub enum ReturnValue<A, B> {
    Int(A),
    Bool(B),
    Group,
}
