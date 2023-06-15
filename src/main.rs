use gumdrop::Options;

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
mod the_chef;

#[derive(Debug, Options)]
struct Opts {
    #[options(help = "print help message")]
    help: bool,

    #[options(help = "do not give cooking advice")]
    no_advice: bool,

    #[options(command)]
    command: Option<Command>,
}

#[derive(Debug, Options)]
enum Command {
    #[options(help = "compile source code")]
    Cook(CookOpts),
}

#[derive(Debug, Options)]
struct CookOpts {
    #[options(help = "print help message")]
    help: bool,

    #[options(free)]
    files: Vec<String>
}

fn main() -> Result<(), io::Error> {
    let opts = Opts::parse_args_default_or_exit();

    match opts.command {
        Some(Command::Cook(cook_opts)) => {
            if cook_opts.files.len() == 0 {
                eprintln!("{}", cook_opts.self_usage());
                exit(1); // TODO: use results
            }
            else if cook_opts.files.len() != 1 {
                eprintln!("`chef cook` only accepts one file as an entry point. Found {}.", cook_opts.files.len());
                exit(1); // TODO: use results
            }
            let path = cook_opts.files.get(0).unwrap();
            let text = SourceText::from_file(path).unwrap();
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
                println!();
                the_chef::give_advice();
                exit(1);
            }


            println!("\n-------------------------------------------------------------------------------\n");

            let graph = compile(ast, diagnostics_bag.clone());

            if diagnostics_bag.borrow().has_errored() {
                println!("\n");
                diagnostics_bag.borrow().print(&text);
                println!();
                the_chef::give_advice();
                exit(1);
            }

            graph.print();
            graph.visualize("graph.svg").unwrap();

            println!("Enjoy!");
            Ok(())
        },
        None => {
            eprintln!("{}", opts.self_usage());
            if let Some(command_list) = opts.self_command_list() {
                eprintln!("\nSubcommands:\n{}", command_list);
            }
            exit(1); // TODO use results
        },
    }
}
