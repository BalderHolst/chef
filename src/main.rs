use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::AST;
use crate::compiler::graph::IOType;
use crate::compiler::graph::*;
use crate::compiler::Compiler;
use crate::diagnostics::{DiagnosticsBag, DiagnosticsBagRef};
use crate::text::SourceText;


mod ast;
mod compiler;
mod diagnostics;
mod text;

fn main() {

    let text = SourceText::from_file("./examples/simple_block.rcp").unwrap();
    let diagnostics_bag: DiagnosticsBagRef = Rc::new(RefCell::new(DiagnosticsBag::new()));

    let lexer = Lexer::from_source(diagnostics_bag.clone(), &text);
    // let lexer = Lexer::new("a = 5 + 8; 5 * (9 + 2);");
    let tokens: Vec<Token> = lexer.collect();

    diagnostics_bag.borrow().print(&text);

    let parser = Parser::new(tokens);
    let mut ast = AST::new();

    println!("\nBuilding AST...");
    for statement in parser {
        ast.add_statement(statement);
    }

    println!("\nPrinting AST:");
    ast.print();

    println!("\n-------------------------------------------------------------------------------\n");

    let mut compiler = Compiler::new(ast);
    compiler.compile();

    println!("DONE.");
}
