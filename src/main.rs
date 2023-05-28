use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::AST;

mod ast;

fn main() {
    let lexer = Lexer::new("45*115*5");
    let tokens: Vec<Token> = lexer.collect();
    println!("{:?}", tokens);
    let parser = Parser::new(tokens);
    let mut ast = AST::new();

    println!("\nBuilding AST...");
    for statement in parser {
        ast.add_statement(statement);
    }

    println!("\nPrinting AST:");
    ast.print();
}
