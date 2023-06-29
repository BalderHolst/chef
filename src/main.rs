use gumdrop::Options;
use terminal_size::Width;

use std::cell::RefCell;
use std::process::exit;
use std::rc::Rc;
use std::io;

use crate::ast::lexer::{Lexer, Token};
use crate::ast::parser::Parser;
use crate::ast::AST;
use crate::compiler::compile;
use crate::diagnostics::{DiagnosticsBag, DiagnosticsBagRef};
use crate::text::SourceText;


mod ast;
mod compiler;
mod diagnostics;
mod text;
mod the_chef;

#[derive(Debug, Options)]
pub struct Opts {
    #[options(help = "print help message")]
    help: bool,

    #[options(short = "q", help = "do not give cooking advice")]
    no_advice: bool,

    #[options(short = "v", help = "be verbose")]
    verbose: bool,

    #[options(command)]
    command: Option<Command>,
}

#[derive(Debug, Options)]
pub enum Command {
    #[options(help = "compile source code")]
    Cook(CookOpts),
}

#[derive(Debug, Options)]
pub struct CookOpts {
    #[options(help = "print help message")]
    help: bool,

    #[options(free)]
    files: Vec<String>
}

fn get_term_width() -> Option<usize> {
    if let Some((Width(w), _)) = terminal_size::terminal_size() {
        Some(w as usize)
    }
    else {
        None
    }
}

fn print_label(label: &'static str) {
    match get_term_width() {
        Some(width) => {
            let mut padding = width / 2 - 1 - label.len()/2;
            let mut odd = (width % 2) == 1;
            if (label.len() % 2) == 1 {
                padding -= 1;
                odd = !odd;
            }
            println!("\n{} {} {}", 
                     "=".repeat(padding),
                     label,
                     "=".repeat(padding + odd as usize),
                     )
        },
        None => {
            println!("\n{}:", label)
        },
    }
}

fn main() -> Result<(), io::Error> {
    let opts = Rc::new(Opts::parse_args_default_or_exit());

    match &opts.command {
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
            let diagnostics_bag: DiagnosticsBagRef = Rc::new(RefCell::new(DiagnosticsBag::new(opts.clone())));

            let lexer = Lexer::from_source(diagnostics_bag.clone(), &text);
            let tokens: Vec<Token> = lexer.collect();

            let parser = Parser::new(tokens, diagnostics_bag.clone(), opts.clone());
            let mut ast = AST::new();

            if opts.verbose {
                print_label("Building AST");
            } 
            for statement in parser {
                ast.add_statement(statement);
            }

            ast.evaluate_constants();

            if opts.verbose {
                print_label("AST");
                ast.print();
            }

            if diagnostics_bag.borrow().has_errored() {
                diagnostics_bag.borrow().exit_with_errors(&text);
            }

            let graph = compile(ast, diagnostics_bag.clone());

            if diagnostics_bag.borrow().has_errored() {
                diagnostics_bag.borrow().exit_with_errors(&text);
            }

            if opts.verbose {
                graph.print();
            }
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
