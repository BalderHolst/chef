use std::{collections::HashMap, rc::Rc};

use crate::ast::{ExpressionReturnType, VariableType};

use super::{visitors::MutVisitor, Directive, DynBlockVersion, MutVar, AST};

pub fn infer(ast: &mut AST<MutVar>) {
    TypeInferer::infer(ast)
}

struct TypeInferer {
    did_work: bool,
    block_outputs: HashMap<(String, Option<DynBlockVersion>), Vec<Rc<MutVar>>>,
}

impl TypeInferer {
    fn infer(ast: &mut AST<MutVar>) {
        let mut inferer = Self::new();

        for block in ast.iter_blocks() {
            match block {
                super::DefinedBlock::Block(b) => {
                    inferer
                        .block_outputs
                        .insert((b.name.clone(), None), b.outputs.clone());
                }
                super::DefinedBlock::DynBlock(b) => {
                    for (n, b) in b.versions.iter().enumerate() {
                        inferer
                            .block_outputs
                            .insert((b.name.clone(), Some(n)), b.outputs.clone());
                    }
                }
            }
        }

        for dir in &mut ast.directives {
            if let Directive::Block(block) = dir {
                inferer.infer_block(block);
            }
        }
    }

    fn new() -> Self {
        Self {
            did_work: true,
            block_outputs: HashMap::new(),
        }
    }

    fn infer_block(&mut self, block: &mut super::Block<MutVar>) {
        loop {
            if !self.did_work {
                self.did_work = true;
                break;
            }

            self.did_work = false;

            self.visit_block(block);
        }
    }
}

impl MutVisitor<MutVar> for TypeInferer {
    fn visit_declaration_definition(&mut self, dec_def: &mut super::DeclarationDefinition<MutVar>) {
        {
            let var = &dec_def.variable;
            let var_type = var.borrow().type_.clone();
            let expr_type = dec_def.expression.return_type();
            if var_type == VariableType::Inferred && expr_type != ExpressionReturnType::Infered {
                var.borrow_mut().type_ = expr_type.try_into().unwrap();
                self.did_work = true;
                return;
            }
        }
        self.do_visit_declaration_definition(dec_def);
    }

    fn visit_definition(&mut self, def: &mut super::Definition<MutVar>) {
        {
            let var = &def.variable;
            let var_type = var.borrow().type_.clone();
            let expr_type = def.expression.return_type();

            // Infer variable type
            if var_type == VariableType::Inferred && expr_type != ExpressionReturnType::Infered {
                var.borrow_mut().type_ = expr_type.try_into().unwrap();
                self.did_work = true;
                return;
            }
        }
        self.do_visit_definition(def);
    }

    fn visit_block_link_expression(&mut self, link: &mut super::BlockLinkExpression<MutVar>) {
        if link.return_type == ExpressionReturnType::Infered {
            let outputs = self
                .block_outputs
                .get(&(link.name.clone(), link.dyn_block_version))
                .unwrap();

            let output_return_types = outputs
                .iter()
                .map(|var| var.borrow().type_.return_type())
                .collect::<Vec<_>>();

            if output_return_types.len() == 1 {
                link.return_type = output_return_types[0].clone();
            } else {
                link.return_type = ExpressionReturnType::Tuple(output_return_types);
            }

            self.did_work = true;
        }
        self.do_visit_block_link_expression(link);
    }

    fn visit_binary_expression(&mut self, bin_expr: &mut super::BinaryExpression<MutVar>) {
        if bin_expr.return_type == ExpressionReturnType::Infered {
            let left_type = bin_expr.left.return_type();
            let right_type = bin_expr.right.return_type();
            if left_type != ExpressionReturnType::Infered
                && right_type != ExpressionReturnType::Infered
            {
                if let Ok(return_type) = bin_expr.operator.return_type(left_type, right_type) {
                    bin_expr.return_type = return_type;
                    self.did_work = true;
                }
            }
        }
        self.do_visit_binary_expression(bin_expr);
    }
}
