//! This module contains the [Determiner] struct which is responsible for converting an [AST] with possibly undetermined variable types into an [AST] with determined immutable variable types.

use std::rc::Rc;

use fnv::FnvHashMap;

use crate::ast::{
    BinaryExpression, Block, BlockLinkExpression, Declaration, DeclarationDefinition, Definition,
    DelayExpression, DetVar, Expression, ExpressionKind, IndexExpression, MutVar,
    NegativeExpression, OutputAssignment, ParenthesizedExpression, PickExpression,
    SizeOfExpression, Statement, StatementKind, TupleDeclarationDefinition, Variable, VariableId,
    VariableRef, WhenStatement, AST,
};

use super::{Directive, DynBlock, GateExpression, Import};

/// Determine the variable types in the given [AST].
pub fn determine(mut_ast: AST<MutVar>) -> AST<DetVar> {
    Determiner::determine(mut_ast)
}

/// The determiner.
struct Determiner {
    mut_ast: AST<MutVar>,
    det_ast: AST<DetVar>,
    vars: FnvHashMap<VariableId, Rc<DetVar>>,
}

impl Determiner {
    /// Create a new determiner.
    fn new(ast: AST<MutVar>) -> Self {
        let bag = ast.diagnostics_bag.clone();
        Determiner {
            mut_ast: ast,
            det_ast: AST::new(bag),
            vars: FnvHashMap::default(),
        }
    }

    /// Determine the variable types in the given [AST].
    fn determine(mut_ast: AST<MutVar>) -> AST<DetVar> {
        let mut determiner = Determiner::new(mut_ast);

        for mut_dir in determiner.mut_ast.directives.clone() {
            let dir = determiner.det_directive(mut_dir);
            determiner.det_ast.directives.push(dir);
        }

        determiner.det_ast
    }

    fn det_directive(&mut self, mut_dir: Directive<MutVar>) -> Directive<DetVar> {
        match mut_dir {
            Directive::Block(mut_block) => Directive::Block(self.det_block(mut_block)),
            Directive::DynBlock(mut_dyn_block) => Directive::DynBlock(DynBlock {
                name: mut_dyn_block.name,
                inputs: mut_dyn_block
                    .inputs
                    .iter()
                    .map(|v| match v {
                        super::DynBlockArg::Var(mut_var) => {
                            super::DynBlockArg::Var(self.det_variable(mut_var.clone()))
                        }
                        super::DynBlockArg::Literal(lit) => {
                            super::DynBlockArg::Literal(lit.clone())
                        }
                    })
                    .collect(),
                script_path: mut_dyn_block.script_path,
                _span: mut_dyn_block._span,
                _opts: mut_dyn_block._opts,
                versions: mut_dyn_block
                    .versions
                    .into_iter()
                    .map(|v| self.det_block(v))
                    .collect(),
            }),
            Directive::Import(mut_import) => self.det_import(mut_import),
            Directive::Constant => todo!("Determine constants"),
            Directive::Unknown => todo!(),
        }
    }

    fn det_import(&mut self, mut_import: Import<MutVar>) -> Directive<DetVar> {
        Directive::Import(Import {
            namespace: mut_import.namespace,
            file_path: mut_import.file_path,
            ast: Self::determine(mut_import.ast),
        })
    }

    /// Determine the variable types in the given [Block].
    fn det_block(&mut self, block: Block<MutVar>) -> Block<DetVar> {
        let inputs = block
            .inputs
            .iter()
            .map(|i| self.det_variable(i.clone()))
            .collect();

        let outputs = block
            .outputs
            .iter()
            .map(|i| self.det_variable(i.clone()))
            .collect();

        let statements = block
            .statements
            .iter()
            .map(|s| self.det_statement(s.clone()))
            .collect();

        Block::new(
            block.id,
            block._dyn_block_id,
            block.name.clone(),
            inputs,
            outputs,
            statements,
            block.span.clone(),
        )
    }

    /// Determine the variable types in the given [Variable].
    fn det_variable(&mut self, mut_var: Rc<MutVar>) -> Rc<DetVar> {
        let det_var = Rc::new((*mut_var).clone().into_inner());
        self.vars.insert(det_var.id, det_var.clone());
        det_var
    }

    /// Determine the variable types in the given [VariableRef].
    fn det_var_ref(&mut self, var_ref: VariableRef<MutVar>) -> VariableRef<DetVar> {
        let var_id = var_ref.var.id();
        let var = self
            .vars
            .get(&var_id)
            .expect("This referenced variable should always exist after type checking.");
        VariableRef {
            var: var.clone(),
            span: var_ref.span,
        }
    }

    /// Determine the variable types in the given [Statement].
    fn det_statement(&mut self, statement: Statement<MutVar>) -> Statement<DetVar> {
        Statement {
            kind: self.det_statement_kind(statement.kind),
            span: statement.span,
        }
    }

    /// Determine the variable types in the given [StatementKind].
    fn det_statement_kind(
        &mut self,
        statement_kind: StatementKind<MutVar>,
    ) -> StatementKind<DetVar> {
        match statement_kind {
            StatementKind::When(when) => StatementKind::When(self.det_when(when)),
            StatementKind::Declaration(declaration) => {
                StatementKind::Declaration(self.det_declaration(declaration))
            }
            StatementKind::DeclarationDefinition(declaration_definition) => {
                StatementKind::DeclarationDefinition(
                    self.det_declaration_definition(declaration_definition),
                )
            }
            StatementKind::Definition(definition) => {
                StatementKind::Definition(self.det_definition(definition))
            }
            StatementKind::TupleDeclarationDefinition(tuple_declaration_definition) => {
                StatementKind::TupleDeclarationDefinition(
                    self.det_tuple_declaration_definition(tuple_declaration_definition),
                )
            }
        }
    }

    /// Determine the variable types in the given [Declaration].
    fn det_declaration(&mut self, declaration: Declaration<MutVar>) -> Declaration<DetVar> {
        Declaration {
            variable: self.det_variable(declaration.variable),
        }
    }

    /// Determine the variable types in the given [DeclarationDefinition].
    fn det_declaration_definition(
        &mut self,
        declaration_definition: DeclarationDefinition<MutVar>,
    ) -> DeclarationDefinition<DetVar> {
        DeclarationDefinition {
            variable: self.det_variable(declaration_definition.variable),
            expression: self.det_expression(declaration_definition.expression),
            kind: declaration_definition.kind,
        }
    }

    /// Determine the variable types in the given [Definition].
    fn det_definition(&mut self, definition: Definition<MutVar>) -> Definition<DetVar> {
        Definition {
            variable: self.det_variable(definition.variable),
            expression: self.det_expression(definition.expression),
            kind: definition.kind,
        }
    }

    /// Determine the variable types in the given [TupleDeclarationDefinition].
    fn det_tuple_declaration_definition(
        &mut self,
        tuple_declaration_definition: TupleDeclarationDefinition<MutVar>,
    ) -> TupleDeclarationDefinition<DetVar> {
        TupleDeclarationDefinition {
            defs: tuple_declaration_definition
                .defs
                .into_iter()
                .map(|def| OutputAssignment {
                    variable: self.det_variable(def.variable),
                    block_variable: self.det_variable(def.block_variable),
                    assignment_type: def.assignment_type,
                })
                .collect(),
            block_link: self.det_block_link(tuple_declaration_definition.block_link),
            def_kind: tuple_declaration_definition.def_kind,
        }
    }

    /// Determine the variable types in the given [WhenStatement].
    fn det_when(&mut self, when: WhenStatement<MutVar>) -> WhenStatement<DetVar> {
        WhenStatement {
            condition: self.det_expression(when.condition),
            statements: when
                .statements
                .into_iter()
                .map(|s| self.det_statement(s))
                .collect(),
        }
    }

    /// Determine the variable types in the given [BlockLinkExpression].
    fn det_expression(&mut self, expression: Expression<MutVar>) -> Expression<DetVar> {
        Expression {
            kind: self.det_expression_kind(expression.kind),
            span: expression.span,
        }
    }

    /// Determine the variable types in the given [ExpressionKind].
    fn det_expression_kind(
        &mut self,
        expression_kind: ExpressionKind<MutVar>,
    ) -> ExpressionKind<DetVar> {
        match expression_kind {
            ExpressionKind::Bool(b) => ExpressionKind::Bool(b),
            ExpressionKind::Int(n) => ExpressionKind::Int(n),
            ExpressionKind::Binary(e) => ExpressionKind::Binary(self.det_binary_expr(e)),
            ExpressionKind::Parenthesized(e) => ExpressionKind::Parenthesized(self.det_par_expr(e)),
            ExpressionKind::Negative(e) => ExpressionKind::Negative(self.det_neg_expr(e)),
            ExpressionKind::Pick(e) => ExpressionKind::Pick(self.det_pick_expr(e)),
            ExpressionKind::Index(e) => ExpressionKind::Index(self.det_index_expr(e)),
            ExpressionKind::VariableRef(e) => ExpressionKind::VariableRef(self.det_var_ref(e)),
            ExpressionKind::BlockLink(e) => ExpressionKind::BlockLink(self.det_block_link(e)),
            ExpressionKind::Delay(e) => ExpressionKind::Delay(self.det_delay_expr(e)),
            ExpressionKind::SizeOf(e) => ExpressionKind::SizeOf(self.det_size_of_expr(e)),
            ExpressionKind::Gate(e) => ExpressionKind::Gate(self.det_gate_expr(e)),
        }
    }

    /// Determine the variable types in the given [NegativeExpression].
    fn det_neg_expr(&mut self, neg_expr: NegativeExpression<MutVar>) -> NegativeExpression<DetVar> {
        NegativeExpression {
            expression: Box::new(self.det_expression(*neg_expr.expression)),
        }
    }

    /// Determine the variable types in the given [ParenthesizedExpression].
    fn det_par_expr(
        &mut self,
        par_expr: ParenthesizedExpression<MutVar>,
    ) -> ParenthesizedExpression<DetVar> {
        ParenthesizedExpression {
            expression: Box::new(self.det_expression(*par_expr.expression)),
        }
    }

    /// Determine the variable types in the given [PickExpression].
    fn det_pick_expr(&mut self, pick_expr: PickExpression<MutVar>) -> PickExpression<DetVar> {
        PickExpression {
            pick_signal: pick_expr.pick_signal,
            from: self.det_var_ref(pick_expr.from),
            span: pick_expr.span,
        }
    }

    /// Determine the variable types in the given [IndexExpression].
    fn det_index_expr(&mut self, index_expr: IndexExpression<MutVar>) -> IndexExpression<DetVar> {
        IndexExpression {
            var_ref: self.det_var_ref(index_expr.var_ref),
            index: index_expr.index,
        }
    }

    /// Determine the variable types in the given [BlockLinkExpression].
    fn det_block_link(
        &mut self,
        block_link: BlockLinkExpression<MutVar>,
    ) -> BlockLinkExpression<DetVar> {
        BlockLinkExpression {
            name: block_link.name,
            return_type: block_link.return_type,
            inputs: block_link
                .inputs
                .into_iter()
                .map(|e| self.det_expression(e))
                .collect(),
            dyn_block_version: block_link.dyn_block_version,
        }
    }

    /// Determine the variable types in the given [DelayExpression].
    fn det_delay_expr(&mut self, delay_expr: DelayExpression<MutVar>) -> DelayExpression<DetVar> {
        DelayExpression {
            expression: Box::new(self.det_expression(*delay_expr.expression)),
            delay: delay_expr.delay,
        }
    }

    /// Determine the variable types in the given [SizeOfExpression].
    fn det_size_of_expr(
        &mut self,
        size_of_expr: SizeOfExpression<MutVar>,
    ) -> SizeOfExpression<DetVar> {
        SizeOfExpression {
            expression: Box::new(self.det_expression(*size_of_expr.expression)),
        }
    }

    /// Determine the variable types in the given [GateExpression].
    fn det_gate_expr(&mut self, size_of_expr: GateExpression<MutVar>) -> GateExpression<DetVar> {
        GateExpression::new(
            self.det_expression(*size_of_expr.left),
            self.det_expression(*size_of_expr.right),
            size_of_expr.operator,
            self.det_expression(*size_of_expr.gated_expr),
        )
    }

    /// Determine the variable types in the given [BinaryExpression].
    fn det_binary_expr(
        &mut self,
        binary_expr: BinaryExpression<MutVar>,
    ) -> BinaryExpression<DetVar> {
        BinaryExpression {
            left: Box::new(self.det_expression(*binary_expr.left)),
            right: Box::new(self.det_expression(*binary_expr.right)),
            operator: binary_expr.operator,
            return_type: binary_expr.return_type,
            span: binary_expr.span,
        }
    }
}
