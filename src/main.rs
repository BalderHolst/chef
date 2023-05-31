use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::AST;
use crate::compiler::graph::IOType;

use compiler::graph::*;

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

    let mut graph = Graph::new();
    let input = graph.push_vertex(Node::new());
    let v2 = graph.push_vertex(Node::new());
    let v3 = graph.push_vertex(Node::new());
    let output = graph.push_vertex(Node::new());

    graph.push_edge(input, v2, Edge::new(
            Connection::Arithmetic(
                ArithmeticConnection::new(
                    IOType::Signal("BLUE".to_string()),
                    IOType::Signal("GREEN".to_string()),
                    ArithmeticOperation::MULTIPLY,
                    IOType::AnySignal(0),
                    )
                )));

    graph.push_edge(input, v2, Edge::new(
            Connection::Arithmetic(
                ArithmeticConnection::new_pick(IOType::Signal("RED".to_string()))
                )));

    graph.push_edge(v2, v3, Edge::new(
            Connection::Arithmetic(
                ArithmeticConnection::new(
                    IOType::Signal("RED".to_string()), 
                    IOType::AnySignal(0), 
                    ArithmeticOperation::ADD,
                    IOType::AnySignal(1),
                    )
                )));

    graph.push_edge(v3, output, Edge::new(
            Connection::Arithmetic(
                ArithmeticConnection::new(
                    IOType::AnySignal(1),
                    IOType::Constant(500),
                    ArithmeticOperation::SUBTRACT,
                    IOType::Signal("WHITE".to_string()),
                    )
                )));

    graph.print();

    println!("DONE.");
}
