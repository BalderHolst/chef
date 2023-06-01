use std::vec;

use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::AST;
use crate::compiler::graph::IOType;
use crate::compiler::graph::*;
use crate::compiler::Compiler;


mod ast;
mod compiler;

fn main() {
    let lexer = Lexer::from_file("./examples/simple_block.rcp").unwrap();
    // let lexer = Lexer::new("a = 5 + 8; 5 * (9 + 2);");
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

    println!("\n-------------------------------------------------------------------------------\n");

    let mut graph = Graph::new();
    let input = graph.push_input_node("a".to_string(), vec![ 
                                      IOType::Signal("RED".to_string()), 
                                      IOType::Signal("GREEN".to_string()), 
                                      IOType::Signal("BLUE".to_string()),
    ]); 

    let v2 = graph.push_inner_node();
    let v3 = graph.push_inner_node();
    let output = graph.push_inner_node();

    graph.push_connection(input, v2, Connection::Arithmetic(
                ArithmeticConnection::new(
                    IOType::Signal("BLUE".to_string()),
                    IOType::Signal("GREEN".to_string()),
                    ArithmeticOperation::MULTIPLY,
                    IOType::AnySignal(0),
                    )
                ));

    graph.push_connection(input, v2, Connection::Arithmetic(
                ArithmeticConnection::new_pick("RED")
                ));

    graph.push_connection(v2, v3, Connection::Arithmetic(
                ArithmeticConnection::new(
                    IOType::Signal("RED".to_string()), 
                    IOType::AnySignal(0), 
                    ArithmeticOperation::ADD,
                    IOType::AnySignal(1),
                    )
                ));

    graph.push_connection(v3, output, Connection::Arithmetic(
                ArithmeticConnection::new(
                    IOType::AnySignal(1),
                    IOType::Constant(500),
                    ArithmeticOperation::SUBTRACT,
                    IOType::Signal("WHITE".to_string()),
                    )
                ));

    graph.print();

    println!("\n-------------------------------------------------------------------------------\n");

    let mut compiler = Compiler::new(ast);
    compiler.compile();

    println!("DONE.");
}
