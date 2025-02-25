use std::collections::HashMap;

use crate::ast::{
    AssignmentType, BinaryExpression, BinaryOperator, Block, BlockLinkExpression, Declaration,
    DeclarationDefinition, Definition, DefinitionKind, DelayExpression, Directive, DynBlockVersion,
    Expression, ExpressionKind, GateExpression, IndexExpression, NegativeExpression,
    PickExpression, SizeOfExpression, Statement, StatementKind, TupleDeclarationDefinition,
    VariableId, VariableRef, VariableSignalType, VariableType, WhenStatement, AST,
};
use crate::compiler::graph::*;
use crate::diagnostics::{CompilationError, CompilationResult};
use crate::error;

struct Scope {
    variables: HashMap<VariableId, (NId, LooseSig)>,
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
        graph: &mut Graph<LooseSig>,
        var_id: VariableId,
        var_type: LooseSig,
        name: String,
    ) -> CompilationResult<()> {
        let nid = graph.push_var_node(var_type.clone(), name.clone());
        match self.variables.insert(var_id, (nid, var_type)) {
            Some(_) => Err(error!("Variable '{name}' already declared in this scope.")),
            None => Ok(()),
        }
    }

    /// TODO: DRY up these variable definitions
    fn declare_input_variable(
        &mut self,
        graph: &mut Graph<LooseSig>,
        var_id: VariableId,
        var_type: LooseSig,
        name: String,
    ) -> CompilationResult<()> {
        let nid = graph.push_input_node(name, var_type.clone());
        match self.variables.insert(var_id, (nid, var_type)) {
            Some(_) => Err(error!("Variable already declared in this scope.")),
            None => Ok(()),
        }
    }

    /// TODO: DRY up these variable definitions
    fn declare_output_variable(
        &mut self,
        name: String,
        graph: &mut Graph<LooseSig>,
        var_id: VariableId,
        var_type: LooseSig,
    ) -> CompilationResult<()> {
        let nid = graph.push_output_node(name, var_type.clone());
        match self.variables.insert(var_id, (nid, var_type)) {
            Some(_) => Err(error!("Variable already declared in this scope.")),
            None => Ok(()),
        }
    }

    fn define_variable(
        &mut self,
        graph: &mut Graph<LooseSig>,
        var_id: VariableId,
        nid: NId,
        nid_type: LooseSig,
        def_kind: DefinitionKind,
    ) -> bool {
        match self.variables.get_mut(&var_id) {
            Some((var_nid, var_type)) => {
                match def_kind {
                    DefinitionKind::Wire(wk) => {
                        graph.push_wire_kind(nid, *var_nid, wk);
                        true
                    }

                    DefinitionKind::Convert(wk) => {
                        let (c1, c2) = graph.push_connection(Connection::new_convert(
                            nid_type.to_combinator_type(),
                            var_type.to_combinator_type(),
                        ));
                        graph.push_wire_kind(nid, c1, wk);
                        graph.push_wire_kind(c2, *var_nid, wk);
                        true
                    }

                    DefinitionKind::Equal => {
                        // Set variable nid to BE the expression output
                        *var_nid = nid;
                        true
                    }
                }
            }
            None => false,
        }
    }

    fn search(&self, var_id: VariableId) -> Option<(NId, LooseSig)> {
        self.variables.get(&var_id).cloned()
    }
}

#[derive(Clone)]
enum NamespaceItem {
    Block(CompiledBlock),
    DynBlock(Vec<CompiledBlock>),
    _Namespace(Namespace),
    _Constant,
}

impl NamespaceItem {
    fn type_name(&self) -> &str {
        match self {
            NamespaceItem::Block(_) => "block",
            NamespaceItem::DynBlock(_) => "dyn block",
            NamespaceItem::_Namespace(_) => "namespace",
            NamespaceItem::_Constant => "constant",
        }
    }
}
type CompiledBlock = Graph<LooseSig>;

#[derive(Clone)]
struct Namespace(HashMap<String, NamespaceItem>);
impl Namespace {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn get(&self, name: &str) -> Option<&NamespaceItem> {
        self.0.get(name)
    }

    fn insert(&mut self, name: String, item: NamespaceItem) -> Option<NamespaceItem> {
        self.0.insert(name, item)
    }
}

pub struct GraphCompiler {
    ast: AST,
    next_anysignal: u64,
    namespaces: HashMap<String, Namespace>,
    scopes: Vec<Scope>,
}

impl GraphCompiler {
    pub fn new(ast: AST) -> Self {
        Self {
            ast,
            next_anysignal: 0,
            namespaces: HashMap::new(),
            scopes: vec![Scope::new()],
        }
    }

    pub fn compile(&mut self) -> Result<(), CompilationError> {
        for directive in self.ast.directives.clone() {
            self.compile_directive(&directive)?
        }
        Ok(())
    }

    fn compile_directive(&mut self, directive: &Directive) -> Result<(), CompilationError> {
        match directive {
            Directive::Block(block) => {
                let block_graph = self.compile_block(block)?;
                let item = NamespaceItem::Block(block_graph);
                self.add_to_namespace(block.name.clone(), None, item)
            }
            Directive::DynBlock(dyn_block) => {
                let graphs = dyn_block
                    .versions
                    .iter()
                    .map(|block| self.compile_block(block))
                    .collect::<Result<Vec<_>, _>>()?;
                let item = NamespaceItem::DynBlock(graphs);
                self.add_to_namespace(dyn_block.name.clone(), None, item)
            }
            Directive::Import(import) => {
                let mut compiler = GraphCompiler::new(import.ast.clone());
                compiler.compile()?;
                let namespace = import.namespace.clone();
                let root_items = compiler.namespaces.remove("").unwrap();
                for (name, item) in root_items.0.into_iter() {
                    self.add_to_namespace(name, namespace.clone(), item)?;
                }
                Ok(())
            }
            Directive::Constant => Ok(()),
            Directive::Unknown => todo!(),
        }
    }

    fn compile_block(&mut self, block: &Block) -> Result<Graph<LooseSig>, CompilationError> {
        let mut graph = Graph::new();
        self.enter_scope();
        for input_var in block.inputs.clone() {
            let input_var = input_var.borrow();
            let t = self.variable_type_to_loose_sig(&input_var.type_);
            self.declare_input_variable(&mut graph, input_var.id, t, input_var.name.clone())?;
        }

        for output_var in block.outputs.clone() {
            let output_var = output_var.borrow();
            let t = self.variable_type_to_loose_sig(&output_var.type_);
            self.declare_output_variable(&mut graph, output_var.name.clone(), output_var.id, t)?;
        }

        for statement in &block.statements {
            self.compile_statement(&mut graph, statement, None)?;
        }

        self.exit_scope();
        Ok(graph)
    }

    fn compile_statement(
        &mut self,
        graph: &mut Graph<LooseSig>,
        statement: &Statement,
        gate: Option<(NId, LooseSig)>,
    ) -> Result<(), CompilationError> {
        match statement.kind.clone() {
            StatementKind::Declaration(dec) => {
                self.compile_declaration_statement(graph, dec)?;
            }
            StatementKind::DeclarationDefinition(dec_def) => {
                self.compile_declaration_definition_statement(graph, dec_def, gate)?;
            }
            StatementKind::Definition(def) => {
                self.compile_definition_statement(graph, def, gate)?;
            }
            StatementKind::TupleDeclarationDefinition(tuple_dec_def) => {
                self.compile_tuple_block_link_statement(graph, tuple_dec_def, gate)?;
            }
            StatementKind::When(when_statement) => {
                self.compile_when_statement(graph, &when_statement)?
            }
        };
        Ok(())
    }

    fn compile_declaration_statement(
        &mut self,
        graph: &mut Graph<LooseSig>,
        dec: Declaration,
    ) -> Result<(), CompilationError> {
        let var = &dec.variable.borrow();
        let var_type = self.variable_type_to_loose_sig(&var.type_);

        // Wire up and define memory cell variables.
        match &var.type_ {
            VariableType::Var(_) => {
                let (var_input_nid, var_nid) =
                    graph.push_operation(Operation::new_pick(var_type.clone()));
                graph.push_wire(var_input_nid, var_nid);
                self.declare_variable(graph, var.id, var_type.clone(), var.name.clone())?;
                self.define_variable(
                    graph,
                    var.id,
                    var_nid,
                    var_type.clone(),
                    DefinitionKind::Wire(WireKind::Red),
                )?;
                self.define_variable(
                    graph,
                    var.id,
                    var_nid,
                    var_type,
                    DefinitionKind::Wire(WireKind::Red),
                )?;
                Ok(())
            }
            _ => self.declare_variable(graph, var.id, var_type, var.name.clone()),
        }
    }

    fn compile_declaration_definition_statement(
        &mut self,
        graph: &mut Graph<LooseSig>,
        dec_def: DeclarationDefinition,
        gate: Option<(NId, LooseSig)>,
    ) -> Result<(), CompilationError> {
        let var = &dec_def.variable.borrow();
        let var_type = self.variable_type_to_loose_sig(&var.type_);

        // Infer types of constant when directly assigned to variable
        let (expr_out_nid, _expr_type) = match &dec_def.expression.kind {
            ExpressionKind::Int(n) => self.compile_constant(graph, *n, Some(var_type.clone()))?,
            ExpressionKind::Bool(b) => {
                self.compile_constant(graph, *b as i32, Some(var_type.clone()))?
            }
            _ => self.compile_expression(graph, &dec_def.expression, None)?,
        };

        self.declare_variable(graph, var.id, var_type.clone(), var.name.clone())?;

        self.compile_definition(graph, gate, expr_out_nid, var_type, var.id, dec_def.kind)?;

        Ok(())
    }

    fn compile_tuple_block_link_statement(
        &mut self,
        graph: &mut Graph<LooseSig>,
        tuple_dec_def: TupleDeclarationDefinition,
        _gate: Option<(NId, LooseSig)>, // TODO: Implement gate
    ) -> Result<(), CompilationError> {
        let block_link = tuple_dec_def.block_link;

        let mut args: Vec<(NId, LooseSig)> = vec![];
        for sub_input in block_link.inputs {
            args.push(self.compile_expression(graph, &sub_input, None)?);
        }

        let ast_block = self
            .ast
            .get_block_by_name(block_link.name.as_str(), None)
            .unwrap()
            .clone();
        let block = self.compile_block(&ast_block)?;

        let output_nodes = graph.stitch_graph(&block, args)?;

        if output_nodes.len() != tuple_dec_def.defs.len() {
            // TODO: Make localized
            return Err(error!(
                "Number of variables in tuple declaration does not match number of outputs."
            ));
        }

        for (i, _) in output_nodes.iter().enumerate() {
            let (output_nid, out_type) = &output_nodes[i];
            let def = &tuple_dec_def.defs[i];
            let var = &def.variable.borrow();
            let (var_nid, var_type) = match &def.assignment_type {
                AssignmentType::Declaration => {
                    let t = self.variable_type_to_loose_sig(&var.type_);
                    let nid = graph.push_var_node(t.clone(), var.name.clone());
                    self.declare_variable(graph, var.id, t.clone(), var.name.clone())?;
                    self.define_variable(
                        graph,
                        var.id,
                        nid,
                        t.clone(),
                        tuple_dec_def.def_kind.clone(),
                    )?;
                    (nid, t)
                }
                AssignmentType::Definition => self
                    .search_scope(var.id)
                    .ok_or(error!("Variable not declared in this scope."))?,
            };

            // Convert to variable type
            let (trans_input, trans_output) =
                graph.push_operation(Operation::new_convert(out_type.clone(), var_type.clone()));
            graph.push_wire(*output_nid, trans_input);

            // Create and connect to variable node
            graph.push_wire(trans_output, var_nid);
        }

        Ok(())
    }

    fn compile_definition_statement(
        &mut self,
        graph: &mut Graph<LooseSig>,
        def: Definition,
        gate: Option<(NId, LooseSig)>,
    ) -> Result<(), CompilationError> {
        let var = &def.variable.borrow();

        let (_var_nid, var_type) = match self.search_scope(var.id) {
            Some(t) => t,
            None => return Err(error!("Variable not declared in this scope.")),
        };

        // Infer types of constant when directly assigned to variable
        let (expr_out_nid, expr_type) = match &def.expression.kind {
            ExpressionKind::Int(n) => self.compile_constant(graph, *n, Some(var_type.clone()))?,
            ExpressionKind::Bool(b) => {
                self.compile_constant(graph, *b as i32, Some(var_type.clone()))?
            }
            _ => self.compile_expression(graph, &def.expression, None)?,
        };

        self.compile_definition(graph, gate, expr_out_nid, expr_type, var.id, def.kind)?;
        Ok(())
    }

    fn compile_definition(
        &mut self,
        graph: &mut Graph<LooseSig>,
        gate: Option<(NId, LooseSig)>,
        expr_out_nid: NId,
        mut expr_type: LooseSig,
        var_id: VariableId,
        def_kind: DefinitionKind,
    ) -> Result<(), CompilationError> {
        // Handle gated assignments
        let output_nid = match gate {
            Some((condition_nid, condition_type)) => {
                expr_type = expr_type.to_signal();

                // TODO: Report error
                assert_ne!(&condition_type, &expr_type);

                // Add the gate
                let (gate_input, gate_output) =
                    graph.push_gate_connection(condition_type, expr_type.clone());

                // Wire up the gate
                graph.push_wire(gate_input, condition_nid);
                graph.push_wire(gate_input, expr_out_nid);

                // Variable is the output of the gate
                gate_output
            }
            None => expr_out_nid,
        };

        self.define_variable(graph, var_id, output_nid, expr_type.clone(), def_kind)
    }

    /// Returns a tuple: (output_vid, output_type)
    fn compile_expression(
        &mut self,
        graph: &mut Graph<LooseSig>,
        expr: &Expression,
        mut out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        if out_type == Some(LooseSig::Many) {
            out_type = None;
        }

        match &expr.kind {
            ExpressionKind::Int(n) => self.compile_constant(graph, *n, out_type),
            ExpressionKind::Bool(b) => self.compile_constant(graph, *b as i32, out_type),
            ExpressionKind::VariableRef(var_ref) => {
                self.compile_variable_ref_expression(graph, var_ref, out_type)
            }
            ExpressionKind::Pick(pick_expr) => {
                self.compile_pick_expression(graph, pick_expr, out_type)
            }
            ExpressionKind::Index(index_expr) => self.compile_index_expression(graph, index_expr),
            ExpressionKind::Parenthesized(expr) => {
                self.compile_expression(graph, &expr.expression, out_type)
            }
            ExpressionKind::Negative(expr) => {
                self.compile_negative_expression(graph, expr, out_type)
            }
            ExpressionKind::Binary(bin_expr) => {
                self.compile_binary_expression(graph, bin_expr, out_type)
            }
            ExpressionKind::BlockLink(block_link_expr) => {
                self.compile_block_link_expression(graph, block_link_expr, out_type)
            }
            ExpressionKind::Delay(delay_expr) => {
                self.compile_delay_expression(graph, delay_expr, out_type)
            }
            ExpressionKind::SizeOf(size_of_expr) => {
                self.compile_size_of_expression(graph, size_of_expr, out_type)
            }
            ExpressionKind::Gate(gate_expr) => {
                self.compile_gate_expression(graph, gate_expr, out_type)
            }
        }
    }

    fn compile_constant(
        &mut self,
        graph: &mut Graph<LooseSig>,
        number: i32,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        let iotype = match out_type {
            Some(out_type) => out_type.to_constant(number).unwrap(),
            None => self.get_new_const_anysignal(number),
        };

        let const_nid = graph.push_node(Node::Constant(iotype.clone()));
        Ok((const_nid, iotype))
    }

    fn compile_variable_ref_expression(
        &mut self,
        _graph: &mut Graph<LooseSig>,
        var_ref: &VariableRef,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        // Get the referenced variable.
        let var = var_ref.var.borrow();
        let (var_ref_nid, var_type) = self
            .search_scope(var.id)
            .unwrap_or_else(|| panic!("Variable references should always point to defined variables. Could not find var '{}' with id {}.", var.name, var.id));

        if let Some(out_type) = out_type {
            assert_eq!(var_type, out_type, "Variable type mismatch in compiler. This should have been caught by the type checker.");
        }

        Ok((var_ref_nid, var_type.clone()))
    }

    fn compile_negative_expression(
        &mut self,
        graph: &mut Graph<LooseSig>,
        neg_expr: &NegativeExpression,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        let expr = &neg_expr.expression;

        let (expr_out_nid, expr_out_type) = self.compile_expression(graph, expr, None)?;

        debug_assert!(out_type.is_none() || Some(expr_out_type.clone()) == out_type, "Negative expression type mismatch in compiler. This should have been caught by the type checker.");

        let (c_input, negative_out_nid) =
            graph.push_connection(Connection::new_arithmetic(ArithmeticOp::new(
                expr_out_type.clone(),
                LooseSig::Constant(-1),
                ArithmeticOperation::Multiply,
                expr_out_type.clone(),
            )));

        graph.push_wire(expr_out_nid, c_input);

        Ok((negative_out_nid, expr_out_type))
    }

    fn compile_pick_expression(
        &mut self,
        graph: &mut Graph<LooseSig>,
        pick_expr: &PickExpression,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        let var_ref = pick_expr.from.clone();
        let var = var_ref.var.borrow();

        let (var_out_nid, _) = self.search_scope(var.id).ok_or(
            error!("No variable with the name \'{}\', ", var.name
                => var_ref.span
            ),
        )?;

        let pick_type = LooseSig::signal(pick_expr.pick_signal.clone());

        let (com_input, picked_nid) =
            graph.push_connection(Connection::new_pick(pick_type.clone().to_combinator_type()));
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
        _graph: &mut Graph<LooseSig>,
        _index_expr: &IndexExpression,
    ) -> Result<(NId, LooseSig), CompilationError> {
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
        graph: &mut Graph<LooseSig>,
        bin_expr: &BinaryExpression,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        let (left_nid, left_type) = self.compile_expression(graph, &bin_expr.left, None)?;
        let (right_nid, right_type) = self.compile_expression(graph, &bin_expr.right, None)?;

        // TODO: Report correctly
        if left_type == right_type {
            panic!(
                "Left and right types should not be the same. Found {} and {}.",
                left_type, right_type
            );
        }

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
            BinaryOperator::Equals => ReturnValue::Bool(DeciderOperation::Equal),
            BinaryOperator::NotEquals => ReturnValue::Bool(DeciderOperation::NotEqual),

            BinaryOperator::EveryEquals => ReturnValue::Bool(DeciderOperation::EveryEqual),
            BinaryOperator::EveryLargerThan => ReturnValue::Bool(DeciderOperation::EveryLargerThan),
            BinaryOperator::EveryLargerThanEquals => {
                ReturnValue::Bool(DeciderOperation::EveryLargerThanOrEqual)
            }
            BinaryOperator::EveryLessThan => ReturnValue::Bool(DeciderOperation::EveryLessThan),
            BinaryOperator::EveryLessThanEquals => {
                ReturnValue::Bool(DeciderOperation::EveryLessThanOrEqual)
            }
            BinaryOperator::EveryNotEquals => ReturnValue::Bool(DeciderOperation::EveryNotEqual),

            BinaryOperator::AnyEquals => ReturnValue::Bool(DeciderOperation::AnyEqual),
            BinaryOperator::AnyLargerThan => ReturnValue::Bool(DeciderOperation::AnyLargerThan),
            BinaryOperator::AnyLargerThanEquals => {
                ReturnValue::Bool(DeciderOperation::AnyLargerThanOrEqual)
            }
            BinaryOperator::AnyLessThan => ReturnValue::Bool(DeciderOperation::AnyLessThan),
            BinaryOperator::AnyLessThanEquals => {
                ReturnValue::Bool(DeciderOperation::AnyLessThanOrEqual)
            }
            BinaryOperator::AnyNotEquals => ReturnValue::Bool(DeciderOperation::AnyNotEqual),

            BinaryOperator::Combine => ReturnValue::Group,
        };

        // Use the out_type if any was provided.
        let out_type = match out_type {
            Some(t) => t,
            None => self.get_new_anysignal(),
        };

        // The connection doing the actual operation
        Ok(match operation {
            ReturnValue::Int(op) => {
                let arithmetic_connection = ArithmeticOp::new(
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
                let decider_connection = DeciderOp::new(
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
                (common_nid, LooseSig::Many)
            }
        })
    }

    /// Compile a link to a block which only returns one value.
    fn compile_block_link_expression(
        &mut self,
        graph: &mut Graph<LooseSig>,
        block_link_expr: &BlockLinkExpression,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        assert!(out_type.is_none(), "found: {:?}", out_type);

        let mut args: Vec<(NId, LooseSig)> = Vec::new();

        for arg_expr in block_link_expr.inputs.iter() {
            let (arg_nid, arg_type) = self.compile_expression(graph, arg_expr, None)?;
            args.push((arg_nid, arg_type));
        }

        let ast_block = self
            .ast
            .get_block_by_name(&block_link_expr.name, block_link_expr.dyn_block_version)
            .ok_or(error!(
                "Block with name '{}' not found.", // TODO: Make localized
                block_link_expr.name
            ))?;

        let block_name = ast_block.name.clone();
        let out = ast_block.outputs[0].borrow().clone();
        let out_type = self.variable_type_to_loose_sig(&out.type_);
        let block_graph = self.get_block_graph(
            &block_name,
            block_link_expr.namespace.as_ref(),
            block_link_expr.dyn_block_version,
        )?;

        let mut outputs = graph.stitch_graph(block_graph, args)?;

        if outputs.len() != 1 {
            // TODO: Make localized
            return Err(error!(
                "Blocks used withing expressions must output exactly one value."
            ));
        }

        let (block_out_nid, block_out_type) = outputs.remove(0);

        match &block_out_type {
            LooseSig::AnySignal(id) => graph.assign_anysignal(id, out_type.clone()),
            LooseSig::ConstantAny((id, c)) => {
                graph.assign_anysignal(id, out_type.to_constant(*c).unwrap())
            }
            LooseSig::Many => (),
            _ => {
                return Err(error!(
                    "Cannot coerce block output `{block_out_type}` into `{out_type}`."
                ))
            }
        }
        Ok((block_out_nid, out_type))
    }

    fn compile_delay_expression(
        &mut self,
        graph: &mut Graph<LooseSig>,
        delay_expr: &DelayExpression,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        let (delay_nid, delay_type) =
            self.compile_expression(graph, &delay_expr.expression, out_type)?;

        let mut out_nid = delay_nid;

        for _ in 0..delay_expr.delay {
            let (delay_input, delay_output) =
                graph.push_operation(Operation::new_delay(delay_type.clone()));
            graph.push_wire(out_nid, delay_input);
            out_nid = delay_output;
        }

        Ok((out_nid, delay_type))
    }

    fn compile_size_of_expression(
        &mut self,
        graph: &mut Graph<LooseSig>,
        size_of_expr: &SizeOfExpression,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        let (size_of_nid, size_of_type) =
            self.compile_expression(graph, &size_of_expr.expression, None)?;

        // This should be checked by the type checker before compiling
        debug_assert_eq!(size_of_type, LooseSig::Many);

        let out_type = match out_type {
            Some(t) => t,
            None => self.get_new_anysignal(),
        };

        let (size_of_input, size_of_output) =
            graph.push_operation(Operation::Sum(SumOp::new(out_type.clone())));

        graph.push_wire(size_of_nid, size_of_input);

        Ok((size_of_output, out_type))
    }

    fn compile_gate_expression(
        &mut self,
        graph: &mut Graph<LooseSig>,
        gate: &GateExpression,
        out_type: Option<LooseSig>,
    ) -> Result<(NId, LooseSig), CompilationError> {
        let (input_nid, input_type) = self.compile_expression(graph, &gate.gated_expr, out_type)?;

        let (left_nid, left_type) = self.compile_expression(graph, &gate.left, None)?;
        let (right_nid, right_type) = self.compile_expression(graph, &gate.right, None)?;

        let input_type = input_type.to_signal();
        let left_type = left_type.to_signal();
        let right_type = right_type.to_signal();

        // TODO: Report this as an error
        assert_ne!(left_type, right_type);

        let (gate_input, gate_output) = graph.push_connection(Connection::new_gate(GateOp {
            left: left_type,
            right: right_type,
            operation: gate.operator.clone(),
            gate_type: input_type.clone(),
        }));

        graph.push_wire(left_nid, gate_input);
        graph.push_wire(right_nid, gate_input);
        graph.push_wire(input_nid, gate_input);

        Ok((gate_output, input_type))
    }

    fn compile_when_statement(
        &mut self,
        graph: &mut Graph<LooseSig>,
        when_statement: &WhenStatement,
    ) -> Result<(), CompilationError> {
        self.enter_scope();

        // Compile condition expression.
        let (cond_nid, cond_type) =
            self.compile_expression(graph, &when_statement.condition, None)?;

        // Compile statements
        for statement in &when_statement.statements {
            self.compile_statement(graph, statement, Some((cond_nid, cond_type.clone())))?;
        }

        self.exit_scope();

        Ok(())
    }

    fn variable_type_to_loose_sig(&mut self, variable_type: &VariableType) -> LooseSig {
        match variable_type {
            VariableType::Bool(bool_type) => match bool_type {
                VariableSignalType::Signal(s) => LooseSig::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Int(int_type) => match int_type {
                VariableSignalType::Signal(s) => LooseSig::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Var(var_type) => match var_type {
                VariableSignalType::Signal(s) => LooseSig::Signal(s.clone()),
                VariableSignalType::Any => self.get_new_anysignal(),
            },
            VariableType::Many => LooseSig::Many,
            VariableType::_Tuple(_) => todo!(),
            VariableType::Inferred => todo!(),
        }
    }

    fn add_to_namespace(
        &mut self,
        name: String,
        namespace: Option<String>,
        item: NamespaceItem,
    ) -> CompilationResult<()> {
        let mut ret = Ok(());
        self.namespaces
            .entry(namespace.unwrap_or("".to_string()).to_string())
            .and_modify(|namespace| {
                if namespace.insert(name.clone(), item.clone()).is_some() {
                    ret = Err(error!("Block with name '{name}' already defined."));
                };
            })
            .or_insert({
                let mut namespace = Namespace::new();
                namespace.insert(name, item);
                namespace
            });
        ret
    }

    fn get_namespace_item(
        &mut self,
        name: &str,
        namespace: Option<&String>,
    ) -> Option<&NamespaceItem> {
        self.namespaces
            .get(namespace.unwrap_or(&"".to_string()))?
            .get(name)
    }

    pub fn get_block_graph(
        &mut self,
        name: &str,
        namespace: Option<&String>,
        dyn_block_version: Option<DynBlockVersion>,
    ) -> CompilationResult<&CompiledBlock> {
        match (self.get_namespace_item(name, namespace), dyn_block_version) {
            (Some(NamespaceItem::Block(block_graph)), None) => Ok(block_graph),
            (Some(NamespaceItem::DynBlock(blocks)), Some(version)) => Ok(&blocks[version]),
            (Some(NamespaceItem::Block(_)), Some(_)) => Err(error!(
                "Internal error: Block version specified for non-dynamic block."
            )),
            (Some(NamespaceItem::DynBlock(_)), None) => Err(error!(
                "Internal error: Dynamic block version not specified."
            )),
            (Some(other), _) => Err(error!(
                "'{}' is not a block. It is a `{}`.",
                name,
                other.type_name()
            )),
            (None, _) if namespace.is_none() => Err(
                // TODO: Make localized
                error!("No block with name '{}' was found.", name),
            ),
            (None, _) => Err(error!(
                // TODO: Make localized
                "No block with name '{}' was found in namespace '{}'.",
                name,
                namespace.unwrap()
            )),
        }
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
        graph: &mut Graph<LooseSig>,
        var_id: VariableId,
        var_type: LooseSig,
        name: String,
    ) -> CompilationResult<()> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_variable(graph, var_id, var_type, name)
    }

    /// Declare an INPUT variable in the current scope.
    fn declare_input_variable(
        &mut self,
        graph: &mut Graph<LooseSig>,
        var_id: VariableId,
        var_type: LooseSig,
        name: String,
    ) -> CompilationResult<()> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_input_variable(graph, var_id, var_type, name)
    }

    /// Declare an OUTPUT variable in the current scope.
    fn declare_output_variable(
        &mut self,
        graph: &mut Graph<LooseSig>,
        name: String,
        var_id: VariableId,
        var_type: LooseSig,
    ) -> CompilationResult<()> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_output_variable(name, graph, var_id, var_type)
    }

    fn define_variable(
        &mut self,
        graph: &mut Graph<LooseSig>,
        var_id: VariableId,
        nid: NId,
        nid_type: LooseSig,
        def_kind: DefinitionKind,
    ) -> CompilationResult<()> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.define_variable(graph, var_id, nid, nid_type.clone(), def_kind.clone()) {
                return Ok(());
            }
        }

        panic!("Variable with id {} not declared.", var_id)
    }

    fn search_scope(&self, var_id: VariableId) -> Option<(NId, LooseSig)> {
        let scopes_len = self.scopes.len();
        for i in 0..scopes_len {
            let p = scopes_len - i - 1;
            if let Some(var) = self.scopes.get(p).unwrap().search(var_id) {
                return Some(var);
            }
        }
        None
    }

    fn get_new_anysignal(&mut self) -> LooseSig {
        let signal = LooseSig::AnySignal(self.next_anysignal);
        self.next_anysignal += 1;
        signal
    }

    fn get_new_const_anysignal(&mut self, n: i32) -> LooseSig {
        let signal = LooseSig::ConstantAny((self.next_anysignal, n));
        self.next_anysignal += 1;
        signal
    }
}

pub enum ReturnValue<A, B> {
    Int(A),
    Bool(B),
    Group,
}
