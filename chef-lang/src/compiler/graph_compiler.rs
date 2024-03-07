use std::collections::{HashMap, HashSet};

use crate::ast::{
    BinaryExpression, BinaryOperator, Block, BlockLinkExpression, Declaration,
    DeclarationDefinition, Definition, Expression, ExpressionKind, IndexExpression, Mutation,
    PickExpression, VariableId, VariableRef, VariableSignalType, WhenExpression, AST,
};
use crate::ast::{Statement, StatementKind, VariableType};
use crate::compiler::graph::*;
use crate::diagnostics::{CompilationError, CompilationResult};

use super::RESERVED_SIGNAL;

struct Scope {
    variables: HashMap<VariableId, (NId, IOType)>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    /// TODO: DRY up these variable definitions
    fn declare_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        var_type: IOType,
    ) -> CompilationResult<()> {
        let nid = graph.push_var_node(var_type.clone());
        match self.variables.insert(var_id, (nid, var_type)) {
            Some(_) => Err(CompilationError::new_generic(
                "Variable already declared in this scope.",
            )),
            None => Ok(()),
        }
    }

    /// TODO: DRY up these variable definitions
    fn declare_input_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        var_type: IOType,
    ) -> CompilationResult<()> {
        let nid = graph.push_input_node(var_type.clone());
        match self.variables.insert(var_id, (nid, var_type)) {
            Some(_) => Err(CompilationError::new_generic(
                "Variable already declared in this scope.",
            )),
            None => Ok(()),
        }
    }

    /// TODO: DRY up these variable definitions
    fn declare_output_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        var_type: IOType,
    ) -> CompilationResult<()> {
        let nid = graph.push_output_node(var_type.clone());
        match self.variables.insert(var_id, (nid, var_type)) {
            Some(_) => Err(CompilationError::new_generic(
                "Variable already declared in this scope.",
            )),
            None => Ok(()),
        }
    }

    fn define_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        nid: NId,
        wk: WireKind,
    ) -> CompilationResult<()> {
        let (var_nid, _var_type) =
            self.variables
                .get(&var_id)
                .ok_or(CompilationError::new_generic(
                    "Variable not declared in this scope.",
                ))?;

        graph.push_wire_kind(nid, *var_nid, wk);

        Ok(())
    }

    fn variable_is_declared(&self, var_id: VariableId) -> bool {
        self.variables.contains_key(&var_id)
    }

    fn search(&self, var_id: VariableId) -> Option<(NId, IOType)> {
        self.variables.get(&var_id).cloned()
    }
}

pub struct GraphCompiler {
    ast: AST,
    next_anysignal: u64,
    block_graphs: HashMap<String, Graph>,
    scopes: Vec<Scope>,
}

impl GraphCompiler {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            next_anysignal: 0,
            block_graphs: HashMap::new(),
            scopes: vec![Scope::new()],
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
            let t = self.variable_type_to_iotype(&input_var.type_);
            self.declare_input_variable(&mut graph, input_var.id, t)?;
        }

        for output_var in block.outputs.clone() {
            let t = self.variable_type_to_iotype(&output_var.type_);
            self.declare_output_variable(&mut graph, output_var.id, t)?;
        }

        for statement in &block.statements {
            self.compile_statement(&mut graph, statement, None)?;
        }

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
            StatementKind::Declaration(dec) => {
                self.compile_declaration_statement(graph, dec)?;
            }
            StatementKind::DeclarationDefinition(dec_def) => {
                self.compile_declaration_definition_statement(graph, dec_def)?;
            }
            StatementKind::Definition(def) => {
                self.compile_definition_statement(graph, def)?;
            }
            StatementKind::Mutation(mutation_statement) => {
                self.compile_mutation_statement(graph, mutation_statement, gate)?;
            }
            StatementKind::Out(expr) => {
                let (expr_nid, out_type) = self.compile_expression(graph, &expr, None)?;
                let out_nid = graph.push_output_node(out_type.clone());
                let (c_input, c_ouput) = graph.push_connection(Connection::new_pick(out_type));
                graph.push_wire(expr_nid, c_input);
                graph.push_wire(c_ouput, out_nid);
            }
            StatementKind::Error => {
                // TODO: Remove error statements
                panic!("There should not be error statements when compilation has started.")
            }
        };
        Ok(())
    }

    fn compile_declaration_statement(
        &mut self,
        graph: &mut Graph,
        dec: Declaration,
    ) -> Result<(), CompilationError> {
        let var = &dec.variable;
        let var_type = self.variable_type_to_iotype(&var.type_);
        self.declare_variable(graph, var.id, var_type)?;
        Ok(())
    }

    fn compile_declaration_definition_statement(
        &mut self,
        graph: &mut Graph,
        dec_def: DeclarationDefinition,
    ) -> Result<(), CompilationError> {
        let var = &dec_def.variable;
        let var_type = self.variable_type_to_iotype(&var.type_);
        self.declare_variable(graph, var.id, var_type)?;
        let definition = dec_def.to_definition();
        self.compile_definition_statement(graph, definition)
    }

    fn compile_definition_statement(
        &mut self,
        graph: &mut Graph,
        def: Definition,
    ) -> Result<(), CompilationError> {
        let var = &def.variable;

        let iotype = self.variable_type_to_iotype(&var.type_);
        let (var_nid, _) = self.compile_expression(graph, &def.expression, Some(iotype))?;

        let wk = match def.kind {
            crate::ast::DefinitionKind::Red => WireKind::Red,
            crate::ast::DefinitionKind::Green => WireKind::Green,
        };

        self.define_variable(graph, var.id, var_nid, wk)
    }

    fn compile_mutation_statement(
        &mut self,
        graph: &mut Graph,
        mutation_statement: Mutation,
        gate: Option<(NId, IOType)>,
    ) -> Result<(), CompilationError> {
        // TODO: Create test for expect
        let var_nid = self.search_scope(mutation_statement.var_ref.var.id).expect("The parser should make sure that mutation statements only happen on defined variables.");
        let var_type = mutation_statement.var_ref.var.type_.clone();
        let var_iotype = self.variable_type_to_iotype(&var_type);

        let (expr_out_nid, expr_out_type) =
            self.compile_expression(graph, &mutation_statement.expression, None)?;

        // TODO: convert expr_out_type if this happens
        assert_ne!(&var_iotype, &expr_out_type);

        todo!();

        // let conn = match mutation_statement.operator {
        //     crate::ast::MutationOperator::Add => Connection::new_convert(
        //         expr_out_type.clone().to_combinator_type(),
        //         var_iotype.clone().to_combinator_type(),
        //     ),

        //     // Multiply by -1 if subtracting
        //     crate::ast::MutationOperator::Subtract => {
        //         Connection::new_arithmetic(ArithmeticCombinator::new(
        //             expr_out_type.clone().to_combinator_type(),
        //             IOType::Constant(-1),
        //             ArithmeticOperation::Multiply,
        //             var_iotype.clone().to_combinator_type(),
        //         ))
        //     }
        // };

        // match gate {
        //     Some((condition_nid, condition_type)) => {
        //         // TODO: convert one if this happens
        //         assert_ne!(&condition_type, &var_iotype);

        //         let (c_input, new_out_nid) = graph.push_connection(conn);
        //         graph.push_wire(expr_out_nid, c_input, WireKind::Green);
        //         let expr_out_nid = new_out_nid; // Expression output is now gated

        //         // Add the gate
        //         let (gate_input, gate_output) =
        //             graph.push_gate_connection(condition_type, var_iotype);

        //         // Wire up the gate
        //         graph.push_wire(gate_input, condition_nid, WireKind::Green);
        //         graph.push_wire(gate_input, expr_out_nid, WireKind::Green);
        //         graph.push_wire(gate_output, var_nid, WireKind::Green);
        //     }
        //     None => {
        //         let (c_input, c_output) = graph.push_connection(conn);
        //         graph.push_wire(c_input, expr_out_nid, WireKind::Green);
        //         graph.push_wire(c_output, var_nid, WireKind::Green);
        //     }
        // };

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
            ExpressionKind::Pick(pick_expr) => self.compile_pick_expression(graph, pick_expr, out_type),
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
        let (var_nid, var_type) = self
            .search_scope(var.id)
            .expect("Variable references should always point to defined variables");
        let var_node = graph.get_node(&var_nid).unwrap();

        Ok((var_nid, var_type.clone()))
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

        graph.push_wire(expr_out_nid, c_input);

        Ok((negative_out_nid, out_type))
    }

    fn compile_pick_expression(
        &mut self,
        graph: &mut Graph,
        pick_expr: &PickExpression,
        out_type: Option<IOType>,
    ) -> Result<(NId, IOType), CompilationError> {
        let var_ref = pick_expr.from.clone();

        let (var_out_nid, _) =
            self.search_scope(var_ref.var.id)
                .ok_or(CompilationError::new_localized(
                    format!("No variable with the name \'{}\', ", var_ref.var.name),
                    var_ref.span,
                ))?;

        let pick_type = IOType::signal(pick_expr.pick_signal.clone());

        let (com_input, picked_nid) = graph.push_connection(Connection::new_arithmetic(
            ArithmeticCombinator::new_pick(pick_type.clone().to_combinator_type()),
        ));
        graph.push_wire(var_out_nid, com_input);

        let (out_nid, out_type) = match out_type {
            Some(out_type) if out_type == pick_type => (picked_nid, out_type),
            Some(out_type) => {
                let (c_input, c_output) = graph.push_connection(Connection::new_convert(
                    pick_type.clone().to_combinator_type(),
                    out_type.clone().to_combinator_type(),
                ));
                graph.push_wire(picked_nid, c_input);
                (c_output, out_type)
            }
            None => (picked_nid, pick_type),
        };

        Ok((out_nid, out_type))
    }

    fn compile_index_expression(
        &mut self,
        _graph: &mut Graph,
        index_expr: &IndexExpression,
    ) -> Result<(NId, IOType), CompilationError> {
        todo!()
        // let index = index_expr.index;
        // if let Some((indexed_output, indexed_output_type)) = self.search_scope(index_expr.var_ref.var.id) {
        //     // TODO: Make type depend on register input type
        //     Ok((indexed_output, IOType::Everything))
        // } else {
        //     Err(CompilationError::new_localized(
        //         "Index out of range.",
        //         index_expr.var_ref.span.clone(),
        //     ))
        // }
    }

    fn compile_binary_expression(
        &mut self,
        graph: &mut Graph,
        bin_expr: &BinaryExpression,
        out_type: Option<IOType>,
    ) -> Result<(NId, IOType), CompilationError> {
        let (left_nid, left_type) = self.compile_expression(graph, &bin_expr.left, None)?;
        let (right_nid, right_type) = self.compile_expression(graph, &bin_expr.right, None)?;

        // TODO: Report correctly
        if left_type == right_type {
            panic!(
                "Left and right types should not be the same. Found {} and {}.",
                left_type, right_type
            );
        }

        // Use the outtype if any was provided.
        let out_type = match out_type {
            Some(t) => t,
            None => self.get_new_anysignal(),
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

            BinaryOperator::EveryEquals => ReturnValue::Bool(DeciderOperation::EveryEquals),
            BinaryOperator::EveryLargerThan => ReturnValue::Bool(DeciderOperation::EveryLargerThan),
            BinaryOperator::EveryLargerThanEquals => {
                ReturnValue::Bool(DeciderOperation::EveryLargerThanEquals)
            }
            BinaryOperator::EveryLessThan => ReturnValue::Bool(DeciderOperation::EveryLessThan),
            BinaryOperator::EveryLessThanEquals => {
                ReturnValue::Bool(DeciderOperation::EveryLessThanEquals)
            }
            BinaryOperator::EveryNotEquals => ReturnValue::Bool(DeciderOperation::EveryNotEquals),

            BinaryOperator::AnyEquals => ReturnValue::Bool(DeciderOperation::AnyEquals),
            BinaryOperator::AnyLargerThan => ReturnValue::Bool(DeciderOperation::AnyLargerThan),
            BinaryOperator::AnyLargerThanEquals => {
                ReturnValue::Bool(DeciderOperation::AnyLargerThanEquals)
            }
            BinaryOperator::AnyLessThan => ReturnValue::Bool(DeciderOperation::AnyLessThan),
            BinaryOperator::AnyLessThanEquals => {
                ReturnValue::Bool(DeciderOperation::AnyLessThanEquals)
            }
            BinaryOperator::AnyNotEquals => ReturnValue::Bool(DeciderOperation::AnyNotEquals),

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
                graph.push_wire(c_input, left_nid);
                graph.push_wire(c_input, right_nid);
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
                graph.push_wire(c_input, left_nid);
                graph.push_wire(c_input, right_nid);
                (output_nid, out_type)
            }
            ReturnValue::Group => {
                graph.push_wire(left_nid, right_nid);
                let common_nid = left_nid;
                (common_nid, IOType::Many)
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
        if let IOType::Constant(_count) = gated_type {
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
        graph.push_wire(cond_out_nid, expr_out_nid);

        // Push the actual gate operation. Here we only let the signal through,
        // if the condition returns a value larger than zero.
        let (gate_input, out_nid) = graph.push_gate_connection(cond_out_type, gated_type.clone());
        graph.push_wire(expr_out_nid, gate_input);

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
            VariableType::Many => IOType::Many,
            VariableType::Register(_) => IOType::Many, // TODO: make dependent on the type of input
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
        self.scopes.push(Scope::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop().unwrap();
    }

    /// Declare a variable in the current scope.
    fn declare_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        var_type: IOType,
    ) -> CompilationResult<()> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_variable(graph, var_id, var_type)
    }

    /// Declare an INPUT variable in the current scope.
    fn declare_input_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        var_type: IOType,
    ) -> CompilationResult<()> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_input_variable(graph, var_id, var_type)
    }

    /// Declare an OUTPUT variable in the current scope.
    fn declare_output_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        var_type: IOType,
    ) -> CompilationResult<()> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_output_variable(graph, var_id, var_type)
    }

    fn define_variable(
        &mut self,
        graph: &mut Graph,
        var_id: VariableId,
        nid: NId,
        wk: WireKind,
    ) -> CompilationResult<()> {
        self.scopes
            .last_mut()
            .unwrap()
            .define_variable(graph, var_id, nid, wk)
    }

    fn search_scope(&self, var_id: VariableId) -> Option<(NId, IOType)> {
        let scopes_len = self.scopes.len();
        for i in 0..scopes_len {
            let p = scopes_len - i - 1;
            if let Some(var) = self.scopes.get(p).unwrap().search(var_id) {
                return Some(var);
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
