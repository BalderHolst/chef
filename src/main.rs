use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::AST;

mod ast;

fn main() {
    let lexer = Lexer::from_file("./examples/simple_block.rcp").unwrap();
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
