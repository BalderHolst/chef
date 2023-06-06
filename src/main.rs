use std::cell::RefCell;
use std::process::exit;
use std::rc::Rc;
use std::{vec, io, env};

use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::{AST, Statement};
use crate::compiler::graph::IOType;
use crate::compiler::{graph::*, compile};
use crate::diagnostics::{DiagnosticsBag, DiagnosticsBagRef};
use crate::text::SourceText;


mod ast;
mod compiler;
mod diagnostics;
mod text;

fn main() -> Result<(), io::Error> {
    env::set_var("RUST_BACKTRACE", "1");

    let text = SourceText::from_file("./examples/test.rcp").unwrap();
    let diagnostics_bag: DiagnosticsBagRef = Rc::new(RefCell::new(DiagnosticsBag::new()));

    let lexer = Lexer::from_source(diagnostics_bag.clone(), &text);
    let tokens: Vec<Token> = lexer.collect();

    let parser = Parser::new(diagnostics_bag.clone(), tokens);
    let mut ast = AST::new();

    println!("\nBuilding AST...");
    for statement in parser {
        ast.add_statement(statement);
    }

    println!("\nPrinting AST:");
    ast.print();

    if diagnostics_bag.borrow().has_errored() {
        println!("\n");
        diagnostics_bag.borrow().print(&text);
        exit(1);
    }


    println!("\n-------------------------------------------------------------------------------\n");

    let graph = compile(ast, diagnostics_bag.clone());

    if diagnostics_bag.borrow().has_errored() {
        println!("\n");
        diagnostics_bag.borrow().print(&text);
        exit(1);
    }

    graph.visualize("graph.svg").unwrap();

    println!("Enjoy!");
    Ok(())
}
