use std::process::exit;
use std::rc::Rc;
use std::{env, io};

use ast::AST;
use clap::Parser;
use cli::{AddCommand, Command, CookOpts, Opts, SimulateOpts};
use diagnostics::{DiagnosticsBag, DiagnosticsBagRef};
use simulator::Simulator;
use text::SourceText;
use utils::VisualizerError;

mod ast;
mod blueprint_converter;
mod cli;
mod compiler;
mod diagnostics;
mod simulator;
mod text;
mod the_chef;
mod utils;

#[cfg(test)]
mod tests;

pub fn compile(opts: Rc<Opts>, cook_opts: &CookOpts) {
    let path = &cook_opts.file;
    let text = Rc::new(SourceText::from_file(path).unwrap());
    let diagnostics_bag: DiagnosticsBagRef = DiagnosticsBag::new_ref(opts.clone(), text.clone());
    let ast = AST::from_source(text, diagnostics_bag.clone(), opts.clone());

    diagnostics_bag.borrow().exit_if_errored();

    if opts.verbose {
        cli::print_label("AST");
        ast.print();
    }

    diagnostics_bag.borrow().exit_if_errored();

    let graph = compiler::compile(ast, diagnostics_bag.clone());

    diagnostics_bag.borrow().exit_if_errored();

    if opts.verbose {
        graph.print();
    }

    if let Some(graph_path) = cook_opts.graph.as_ref() {
        if let Err(e) = graph.visualize(graph_path) {
            match e {
                VisualizerError::IoErr(e) => {
                    eprintln!("Error writing graph output file: `{}`", e)
                }
                VisualizerError::GraphvizIoError(_e) => {
                    eprintln!("Error calling graphviz. Do you have it installed?");
                    exit(1);
                }
                VisualizerError::GraphvizError(e) => eprintln!("Error creating graph: `{}`", e),
            };
        };
    }

    if cook_opts.dot {
        let dot = graph.dot_repr();
        println!("{dot}");
        exit(0);
    }

    let blueprint_str = blueprint_converter::convert_to_graph_to_blueprint_string(
        graph,
        opts.verbose || cook_opts.verbose,
    )
    .unwrap();
    println!("{blueprint_str}");
    eprintln!();
}

fn simulate(opts: Rc<Opts>, sim_opts: &SimulateOpts) {
    let path = &sim_opts.file;
    let text = Rc::new(SourceText::from_file(&path).unwrap());
    let diagnostics_bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
    let ast = AST::from_source(text, diagnostics_bag.clone(), opts.clone());

    diagnostics_bag.borrow().exit_if_errored();

    let graph = compiler::compile(ast, diagnostics_bag.clone());

    diagnostics_bag.borrow().exit_if_errored();

    let mut sim = Simulator::new(graph, vec![]);

    sim.dump_simulation(10, "./test")
}

fn main() -> Result<(), io::Error> {
    let opts = Rc::new(Opts::parse());

    match &opts.command {
        Command::Cook(cook_opts) => {
            compile(opts.clone(), cook_opts);

            eprintln!("Enjoy!");
            Ok(())
        }
        Command::Simulate(sim_opts) => {
            simulate(opts.clone(), sim_opts);
            Ok(())
        }
        Command::Add(c) => match c.command {
            AddCommand::Signals(_) => {
                let cwd = env::current_dir().expect("Could not find current dir");
                utils::import_signals::import_signal_file(cwd);
                Ok(())
            }
        },
    }
}
