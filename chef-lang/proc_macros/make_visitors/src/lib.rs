use convert_case::{Case, Casing};
use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use proc_macro2;

/// Methods of the visitors
const METHODS: &[VisitorMethod] = &[

    VisitorMethod {
        type_: "Block",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                for input in #r block.inputs {
                    self.visit_variable(input);
                }
                for output in #r block.outputs {
                    self.visit_variable(output);
                }
                for statement in #r block.statements {
                    self.visit_statement(statement);
                }
            })
        }),
    },

    VisitorMethod {
        type_: "Statement",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
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
                        self.visit_tuple_declaration_definition(tuple_dec_def);
                    }
                }
            })
        }),
    },

    VisitorMethod {
        type_: "Expression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
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
                        self.visit_block_link_expression(block);
                    }
                    ExpressionKind::Delay(delay) => {
                        self.visit_delay_expression(delay);
                    }
                    ExpressionKind::SizeOf(expr) => {
                        self.visit_size_of_expression(expr);
                    }
                }
            })
        }),
    },

    VisitorMethod {
        type_: "DeclarationDefinition",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_variable(#r declaration_definition.variable);
                self.visit_expression(#r declaration_definition.expression);
            })
        }),
    },

    VisitorMethod {
        type_: "Declaration",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_variable(#r declaration.variable);
            })
        }),
    },

    VisitorMethod {
        type_: "Definition",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_expression(#r definition.expression);
            })
        }),
    },

    VisitorMethod {
        type_: "SizeOfExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_expression(#r size_of_expression.expression);
            })
        }),
    },

    VisitorMethod {
        type_: "WhenStatement",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_expression(#r when_statement.condition);
                for statement in #r when_statement.statements {
                    self.visit_statement(statement);
                }
            })
        }),
    },

    VisitorMethod {
        type_: "TupleDeclarationDefinition",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                for def in #r tuple_declaration_definition.defs {
                    self.visit_variable(#r def.variable);
                    self.visit_variable(#r def.block_variable);
                }
                self.visit_block_link_expression(#r tuple_declaration_definition.block_link);
            })
        }),
    },

    VisitorMethod {
        type_: "Number",
        no_mut: false,
        do_method: None,
    },

    VisitorMethod {
        type_: "bool",
        no_mut: false,
        do_method: None,
    },

    VisitorMethod {
        type_: "Variable",
        no_mut: true,
        do_method: None,
    },

    VisitorMethod {
        type_: "VariableRef",
        no_mut: true,
        do_method: None,
    },

    VisitorMethod {
        type_: "BinaryExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_expression(#r binary_expression.left);
                self.visit_expression(#r binary_expression.right);
            })
        }),
    },

    VisitorMethod {
        type_: "ParenthesizedExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_expression(#r parenthesized_expression.expression);
            })
        }),
    },

    VisitorMethod {
        type_: "NegativeExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_expression(#r negative_expression.expression);
            })
        }),
    },

    VisitorMethod {
        type_: "PickExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_variable_ref(#r pick_expression.from);
            })
        }),
    },

    VisitorMethod {
        type_: "IndexExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_variable_ref(#r index_expression.var_ref);
            })
        }),
    },

    VisitorMethod {
        type_: "BlockLinkExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                for input in #r block_link_expression.inputs {
                    self.visit_expression(input);
                }
            })
        }),
    },

    VisitorMethod {
        type_: "DelayExpression",
        no_mut: false,
        do_method: Some(|r: Box<dyn ToTokens>| {
            Box::new(quote! {
                self.visit_expression(#r delay_expression.expression);
            })
        }),
    },

];

#[proc_macro]
pub fn make_visitors(_item: TokenStream) -> TokenStream {
    let mut s = TokenStream::new();
    s.extend(generate_visitor(false));
    s.extend(generate_visitor(true));
    s
}

struct VisitorMethod {
    type_: &'static str,
    no_mut: bool,

    // Takes (r: &str)
    do_method: Option<fn(Box<dyn ToTokens>) -> Box<dyn ToTokens>>,
}

impl VisitorMethod {
    fn tokens(&self, r: proc_macro2::TokenStream) -> Box<dyn ToTokens> {
        let name  = format_ident!("{}", self.type_.to_case(Case::Snake));
        let type_ = format_ident!("{}", self.type_);

        let r = match self.no_mut {
            false => Box::new(quote! { #r }),
            true => Box::new(quote! { & }),
        };

        let method_name = format_ident!("visit_{name}");
        let do_method_name = format_ident!("do_visit_{name}");

        let do_body = match self.do_method {
            Some(do_method) => (do_method)(r.clone()),
            None => Box::new(quote! {}),
        };

        Box::new(quote! {

            fn #method_name(&mut self, #name: #r #type_) {
                self.#do_method_name(#name);
            }

            fn #do_method_name(&mut self, #name: #r #type_) {
                #do_body
            }

        })
    }
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

    let methods: Vec<Box<dyn ToTokens>> = METHODS.iter().map(|method| method.tokens(r.clone())).collect();

    quote! { 
        #[doc = #doc]
        pub trait #name {
            #(#methods)*
        }
    }.to_token_stream().into()
}
