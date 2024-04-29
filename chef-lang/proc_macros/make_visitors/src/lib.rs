use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn make_visitors(_item: TokenStream) -> TokenStream {
    let mut s = TokenStream::new();
    s.extend(generate_visitor(false));
    s.extend(generate_visitor(true));
    s
}

fn generate_visitor(mutable: bool) -> TokenStream {

    let (doc, name, r) = match mutable {
        true => (
            quote! { "Allows stuct to *mutably* visit the chef [AST]." },
            quote! { MutVisitor },
            quote! { &mut },
        ),
        false => (
            quote! { "Allows stuct to *immutably* visit the chef [AST]." },
            quote! { Visitor },
            quote! { & },
        ),
    };

    quote! {
        #[doc = #doc]
        pub trait #name {
            fn do_visit_statement(&mut self, statement: #r Statement) {
                match #r statement.kind {
                    StatementKind::Declaration(declaration) => {
                        self.visit_declaration(declaration);
                    }
                    StatementKind::DeclarationDefinition(dec_def) => {
                        self.visit_declaration_definition(dec_def);
                    }
                    StatementKind::Definition(def) => {
                        self.visit_definition(def);
                    }
                    StatementKind::When(when) => {
                        self.visit_when_statement(when);
                    }
                    StatementKind::TupleDeclarationDefinition(tuple_dec_def) => {
                        self.visit_tuple_definition_declaration_statement(tuple_dec_def);
                    }
                }
            }

            fn do_visit_expression(&mut self, expression: #r Expression) {
                match #r expression.kind {
                    ExpressionKind::Int(number) => {
                        self.visit_number(number);
                    }
                    ExpressionKind::Bool(bool) => {
                        self.visit_bool(bool);
                    }
                    ExpressionKind::VariableRef(variable) => {
                        self.visit_variable_ref(variable);
                    }
                    ExpressionKind::Binary(expr) => {
                        self.visit_binary_expression(expr);
                    }
                    ExpressionKind::Parenthesized(expr) => {
                        self.visit_parenthesized_expression(expr);
                    }
                    ExpressionKind::Negative(expr) => {
                        self.visit_negative_expression(expr);
                    }
                    ExpressionKind::Pick(expr) => {
                        self.visit_pick_expression(expr);
                    }
                    ExpressionKind::Index(expr) => {
                        self.visit_index_expression(expr);
                    }
                    ExpressionKind::BlockLink(block) => {
                        self.visit_block_link(block);
                    }
                    ExpressionKind::Delay(delay) => {
                        self.visit_delay(delay);
                    }
                    ExpressionKind::SizeOf(expr) => {
                        self.visit_size_of(expr);
                    }
                }
            }

            fn do_visit_block(&mut self, block: #r Block) {
                for statement in #r block.statements {
                    self.visit_statement(statement);
                }
            }

            fn do_visit_declaration_definition(&mut self, assignment: #r DeclarationDefinition) {
                let expr = #r assignment.expression;
                self.visit_expression(expr)
            }

            fn do_visit_definition(&mut self, assignment: #r Definition) {
                self.visit_expression(#r assignment.expression)
            }

            fn do_visit_size_of(&mut self, expr: #r SizeOfExpression) {
                self.visit_expression(#r expr.expression);
            }

            fn visit_statement(&mut self, statement: #r Statement) {
                self.do_visit_statement(statement);
            }

            fn visit_expression(&mut self, expression: #r Expression) {
                self.do_visit_expression(expression);
            }

            fn visit_binary_expression(&mut self, binary_expression: #r BinaryExpression) {
                self.visit_expression(#r binary_expression.left);
                self.visit_expression(#r binary_expression.right);
            }

            fn visit_parenthesized_expression(&mut self, expr: #r ParenthesizedExpression) {
                self.visit_expression(#r expr.expression);
            }

            fn visit_negative_expression(&mut self, expr: #r Expression) {
                self.visit_expression(expr);
            }

            fn visit_expression_statement(&mut self, expr: #r Expression) {
                self.visit_expression(expr);
            }

            fn visit_block(&mut self, block: #r Block) {
                self.do_visit_block(block);
            }

            fn visit_out(&mut self, expr: #r Expression) {
                self.visit_expression(expr);
            }

            fn visit_declaration(&mut self, _declaration: #r Declaration) {}

            fn visit_declaration_definition(&mut self, dec_def: #r DeclarationDefinition) {
                self.do_visit_declaration_definition(dec_def);
            }

            fn visit_definition(&mut self, def: #r Definition) {
                self.do_visit_definition(def);
            }

            fn visit_block_link(&mut self, block: #r BlockLinkExpression) {
                for expr in #r block.inputs {
                    self.visit_expression(expr);
                }
            }

            fn visit_delay(&mut self, delay: #r DelayExpression) {
                self.visit_expression(#r delay.expression);
            }

            fn visit_size_of(&mut self, expr: #r SizeOfExpression) {
                self.do_visit_size_of(expr);
            }

            fn visit_when_statement(&mut self, when: #r WhenStatement) {
                self.visit_expression(#r when.condition);
                for statement in #r when.statements {
                    self.visit_statement(statement);
                }
            }

            fn visit_tuple_definition_declaration_statement(
                &mut self,
                tuple_dec_def: #r TupleDeclarationDefinition,
            ) {
                self.visit_block_link(#r tuple_dec_def.block_link)
            }

            fn visit_statement_list(&mut self, sl: #r StatementList) {
                for s in #r sl.statements {
                    self.visit_statement(s);
                }
                if let Some(out_expr) = #r sl.out {
                    self.visit_expression(out_expr);
                }
            }

            fn visit_pick_expression(&mut self, expr: #r PickExpression);
            fn visit_index_expression(&mut self, expr: #r IndexExpression);
            fn visit_number(&mut self, number: #r i32);
            fn visit_bool(&mut self, value: #r bool);
            fn visit_variable_ref(&mut self, var: #r VariableRef);
        }
    }.into()
}
