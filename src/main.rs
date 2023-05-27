use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::AST;

mod ast;

fn main() {
    let lexer = Lexer::new("151");
    let tokens: Vec<Token> = lexer.collect();
    println!("{:?}", tokens);
    let parser = Parser::new(tokens);
    let mut ast = AST::new();

    println!("Building AST...");
    for statement in parser {
        dbg!(&statement);
        ast.add_statement(statement);
    }

    println!("Printing!");
    ast.print();
}
