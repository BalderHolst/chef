use std::cell::RefCell;
use std::process::exit;
use std::rc::Rc;
use std::{io, env};

use cli::{Opts, Command, AddCommand};
use ast::AST;
use compiler::compile;
use diagnostics::{DiagnosticsBag, DiagnosticsBagRef};
use gumdrop::Options;
use text::SourceText;

mod ast;
mod compiler;
mod diagnostics;
mod text;
mod the_chef;
mod blueprint_converter;
mod utils;
mod cli;


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
            let text = Rc::new(SourceText::from_file(path).unwrap());
            let diagnostics_bag: DiagnosticsBagRef = Rc::new(RefCell::new(DiagnosticsBag::new(opts.clone(), text.clone())));
            let ast = AST::from_source(text, diagnostics_bag.clone(), opts.clone());
            
            diagnostics_bag.borrow().exit_if_errored();

            if opts.verbose {
                cli::print_label("AST");
                ast.print();
            }

            diagnostics_bag.borrow().exit_if_errored();

            let graph = compile(ast, diagnostics_bag.clone());

            diagnostics_bag.borrow().exit_if_errored();

            if opts.verbose {
                graph.print();
            }

            graph.visualize("graph.svg").unwrap();

            // let blueprint = BlueprintConverter::new(graph).convert_to_blueprint();
            // dbg!(blueprint);

            println!("Enjoy!");
            Ok(())
        },
        Some(Command::Add(c)) =>  {
            match c.command {
                Some(AddCommand::Signals(_)) => {
                    let cwd = env::current_dir().expect("Could not find current dir");
                    utils::import_signals::import_signal_file(cwd);
                    Ok(())
                },
                None => {
                    eprintln!("Only `signals` subcommand works for now"); // TODO
                    exit(1);
                },
            }
        }
        None => {
            eprintln!("{}", opts.self_usage());
            if let Some(command_list) = opts.self_command_list() {
                eprintln!("\nSubcommands:\n{}", command_list);
            }
            exit(1); // TODO use results
        },
    }
}
