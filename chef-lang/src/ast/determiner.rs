use std::rc::Rc;

use fnv::FnvHashMap;

use crate::ast::{
    BinaryExpression, Block, BlockLinkExpression, Declaration, DeclarationDefinition, Definition,
    DelayExpression, DetVar, Expression, ExpressionKind, IndexExpression, MutVar,
    NegativeExpression, OutputAssignment, ParenthesizedExpression, PickExpression,
    SizeOfExpression, Statement, StatementKind, TupleDeclarationDefinition, Variable, VariableId,
    VariableRef, WhenStatement, AST,
};

use super::GateExpression;

pub fn determine(mut_ast: AST<MutVar>) -> AST<DetVar> {
    Determiner::determine(mut_ast)
}

struct Determiner {
    mut_ast: AST<MutVar>,
    det_ast: AST<DetVar>,
    vars: FnvHashMap<VariableId, Rc<DetVar>>,
}

impl Determiner {
    fn new(ast: AST<MutVar>) -> Self {
        let bag = ast.diagnostics_bag.clone();
        Determiner {
            mut_ast: ast,
            det_ast: AST::new(bag),
            vars: FnvHashMap::default(),
        }
    }

    fn determine(mut_ast: AST<MutVar>) -> AST<DetVar> {
        let mut determiner = Determiner::new(mut_ast);

        for mut_block in determiner.mut_ast.blocks.clone() {
            let block = determiner.det_block(mut_block);
            determiner.det_ast.blocks.push(block);
        }

        determiner.det_ast
    }

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
            block.dyn_block_id,
            block.name.clone(),
            inputs,
            outputs,
            statements,
            block.span.clone(),
        )
    }

    fn det_variable(&mut self, mut_var: Rc<MutVar>) -> Rc<DetVar> {
        let det_var = Rc::new((*mut_var).clone().into_inner());
        self.vars.insert(det_var.id, det_var.clone());
        det_var
    }

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

    fn det_statement(&mut self, statement: Statement<MutVar>) -> Statement<DetVar> {
        Statement {
            kind: self.det_statement_kind(statement.kind),
            span: statement.span,
        }
    }

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

    fn det_declaration(&mut self, declaration: Declaration<MutVar>) -> Declaration<DetVar> {
        Declaration {
            variable: self.det_variable(declaration.variable),
        }
    }

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

    fn det_definition(&mut self, definition: Definition<MutVar>) -> Definition<DetVar> {
        Definition {
            variable: self.det_variable(definition.variable),
            expression: self.det_expression(definition.expression),
            kind: definition.kind,
        }
    }

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

    fn det_expression(&mut self, expression: Expression<MutVar>) -> Expression<DetVar> {
        Expression {
            kind: self.det_expression_kind(expression.kind),
            span: expression.span,
        }
    }

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

    fn det_neg_expr(&mut self, neg_expr: NegativeExpression<MutVar>) -> NegativeExpression<DetVar> {
        NegativeExpression {
            expression: Box::new(self.det_expression(*neg_expr.expression)),
        }
    }

    fn det_par_expr(
        &mut self,
        par_expr: ParenthesizedExpression<MutVar>,
    ) -> ParenthesizedExpression<DetVar> {
        ParenthesizedExpression {
            expression: Box::new(self.det_expression(*par_expr.expression)),
        }
    }

    fn det_pick_expr(&mut self, pick_expr: PickExpression<MutVar>) -> PickExpression<DetVar> {
        PickExpression {
            pick_signal: pick_expr.pick_signal,
            from: self.det_var_ref(pick_expr.from),
            span: pick_expr.span,
        }
    }

    fn det_index_expr(&mut self, index_expr: IndexExpression<MutVar>) -> IndexExpression<DetVar> {
        IndexExpression {
            var_ref: self.det_var_ref(index_expr.var_ref),
            index: index_expr.index,
        }
    }

    fn det_block_link(
        &mut self,
        block_link: BlockLinkExpression<MutVar>,
    ) -> BlockLinkExpression<DetVar> {
        BlockLinkExpression {
            block_id: block_link.block_id,
            dyn_block_id: block_link.dyn_block_id,
            return_type: block_link.return_type,
            inputs: block_link
                .inputs
                .into_iter()
                .map(|e| self.det_expression(e))
                .collect(),
        }
    }

    fn det_delay_expr(&mut self, delay_expr: DelayExpression<MutVar>) -> DelayExpression<DetVar> {
        DelayExpression {
            expression: Box::new(self.det_expression(*delay_expr.expression)),
            delay: delay_expr.delay,
        }
    }

    fn det_size_of_expr(
        &mut self,
        size_of_expr: SizeOfExpression<MutVar>,
    ) -> SizeOfExpression<DetVar> {
        SizeOfExpression {
            expression: Box::new(self.det_expression(*size_of_expr.expression)),
        }
    }

    fn det_gate_expr(&mut self, size_of_expr: GateExpression<MutVar>) -> GateExpression<DetVar> {
        GateExpression::new(
            self.det_expression(*size_of_expr.gate_expr),
            self.det_expression(*size_of_expr.gated_expr),
        )
    }

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
