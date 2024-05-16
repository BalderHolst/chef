use crate::ast::{ExpressionReturnType, VariableType};

use super::{visitors::MutVisitor, MutVar, AST};

pub fn infer(ast: &mut AST<MutVar>) {
    for block in &mut ast.blocks {
        TypeInferer::new().infer(block);
    }
}

struct TypeInferer {
    did_work: bool,
}

impl TypeInferer {
    fn new() -> Self {
        Self { did_work: true }
    }

    fn infer(&mut self, block: &mut super::Block<MutVar>) {
        loop {
            if !self.did_work {
                break;
            }

            self.did_work = false;

            self.visit_block(block);
        }
    }
}

impl MutVisitor<MutVar> for TypeInferer {
    fn visit_declaration_definition(&mut self, dec_def: &mut super::DeclarationDefinition<MutVar>) {
        let mut var = (*dec_def.variable).borrow_mut();
        let var_type = &var.type_;

        // Skip if nothing need to be inferred
        if *var_type != VariableType::Inferred {
            // self.do_visit_declaration_definition(dec_def);
            return;
        }

        let expr_type = dec_def.expression.return_type();

        if expr_type == ExpressionReturnType::Infered {
            return;
        }

        var.type_ = expr_type.into();

        self.did_work = true;
    }

    fn visit_definition(&mut self, def: &mut super::Definition<MutVar>) {
        let mut var = (*def.variable).borrow_mut();
        let var_type = &var.type_;

        // Skip if nothing need to be inferred
        if *var_type != VariableType::Inferred {
            return;
        }

        let expr_type = def.expression.return_type();

        println!(
            "{}: {} - expr: {:?} -> {}",
            var.name, var_type, &def.expression, expr_type
        );

        if expr_type == ExpressionReturnType::Infered {
            return;
        }

        var.type_ = expr_type.into();

        self.did_work = true;
    }
}
