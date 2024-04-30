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

            fn visit_block(&mut self, block: #r Block) {
                self.do_visit_block(block);
            }
            fn do_visit_block(&mut self, block: #r Block) {
                for input in #r block.inputs {
                    self.visit_variable(input);
                }
                for output in #r block.outputs {
                    self.visit_variable(output);
                }
                for statement in #r block.statements {
                    self.visit_statement(statement);
                }
            }

            fn visit_statement(&mut self, statement: #r Statement) {
                self.do_visit_statement(statement);
            }
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

            fn visit_expression(&mut self, expression: #r Expression) {
                self.do_visit_expression(expression);
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

            fn visit_declaration_definition(&mut self, dec_def: #r DeclarationDefinition) {
                self.do_visit_declaration_definition(dec_def);
            }
            fn do_visit_declaration_definition(&mut self, dec_def: #r DeclarationDefinition) {
                self.visit_variable(#r dec_def.variable);
                self.visit_expression(#r dec_def.expression);
            }

            fn visit_declaration(&mut self, dec: #r Declaration) {
                self.do_visit_declaration(dec);
            }
            fn do_visit_declaration(&mut self, dec: #r Declaration) {
                self.visit_variable(#r dec.variable);
            }

            fn visit_definition(&mut self, def: #r Definition) {
                self.do_visit_definition(def);
            }
            fn do_visit_definition(&mut self, def: #r Definition) {
                self.visit_expression(#r def.expression);
            }

            fn visit_size_of(&mut self, size: #r SizeOfExpression) {
                self.do_visit_size_of(size);
            }
            fn do_visit_size_of(&mut self, size: #r SizeOfExpression) {
                self.visit_expression(#r size.expression);
            }

            fn visit_when_statement(&mut self, when_statement: #r WhenStatement) {
                self.do_visit_when_statement(when_statement);
            }
            fn do_visit_when_statement(&mut self, when: #r WhenStatement) {
                self.visit_expression(#r when.condition);
                for statement in #r when.statements {
                    self.visit_statement(statement);
                }
            }

            fn visit_tuple_definition_declaration_statement(&mut self, tuple_dec_def: #r TupleDeclarationDefinition) {
                self.do_visit_tuple_declaration_definition_statement(tuple_dec_def);
            }
            fn do_visit_tuple_declaration_definition_statement(&mut self, tuple_dec_def: #r TupleDeclarationDefinition) {
                for def in #r tuple_dec_def.defs {
                    self.visit_variable(#r def.variable);
                    self.visit_variable(#r def.block_variable);
                }
                self.visit_block_link(#r tuple_dec_def.block_link);
            }

            fn visit_number(&mut self, n: #r i32) {
                self.do_visit_number(n);
            }
            fn do_visit_number(&mut self, n: #r i32) {}

            fn visit_bool(&mut self, b: #r bool) {
                self.do_visit_bool(b);
            }
            fn do_visit_bool(&mut self, b: #r bool) {}

            fn visit_variable(&mut self, var: #r std::rc::Rc<Variable>) {
                self.do_visit_variable(var);
            }
            fn do_visit_variable(&mut self, var: #r std::rc::Rc<Variable>) {}

            fn visit_variable_ref(&mut self, var: #r VariableRef) {
                self.do_visit_variable_ref(var);
            }
            fn do_visit_variable_ref(&mut self, var: #r VariableRef) {
                self.visit_variable(#r var.var);
            }

            fn visit_binary_expression(&mut self, bin_expr: #r BinaryExpression) {
                self.do_visit_binary_expression(bin_expr);
            }
            fn do_visit_binary_expression(&mut self, bin_expr: #r BinaryExpression) {
                self.visit_expression(#r bin_expr.left);
                self.visit_expression(#r bin_expr.right);
            }

            fn visit_parenthesized_expression(&mut self, paren_expr: #r ParenthesizedExpression) {
                self.do_visit_parenthesized_expression(paren_expr);
            }
            fn do_visit_parenthesized_expression(&mut self, paren_expr: #r ParenthesizedExpression) {
                self.visit_expression(#r paren_expr.expression);
            }

            fn visit_negative_expression(&mut self, neg_expr: #r NegativeExpression) {
                self.do_visit_negative_expression(neg_expr);
            }
            fn do_visit_negative_expression(&mut self, neg_expr: #r NegativeExpression) {
                self.visit_expression(#r neg_expr.expression);
            }

            fn visit_pick_expression(&mut self, expr: #r PickExpression) {
                self.do_visit_pick_expression(expr);
            }
            fn do_visit_pick_expression(&mut self, pick_expr: #r PickExpression) {
                self.visit_variable_ref(#r pick_expr.from);
            }
            fn visit_index_expression(&mut self, expr: #r IndexExpression) {
                self.do_visit_index_expression(expr);
            }
            fn do_visit_index_expression(&mut self, index_expr: #r IndexExpression) {
                self.visit_variable_ref(#r index_expr.var_ref);
            }

            fn visit_block_link(&mut self, link: #r BlockLinkExpression) {
                self.do_visit_block_link(link);
            }
            fn do_visit_block_link(&mut self, link_expr: #r BlockLinkExpression) {
                for input in #r link_expr.inputs {
                    self.visit_expression(input);
                }
            }

            fn visit_delay(&mut self, delay: #r DelayExpression) {
                self.do_visit_delay(delay);
            }
            fn do_visit_delay(&mut self, delay: #r DelayExpression) {
                self.visit_expression(#r delay.expression);
            }

        }
    }.into()
}
