use std::fs;
use std::process::exit;
use std::rc::Rc;
use std::{env, process::ExitCode};

use ast::AST;
use clap::Parser;
use cli::{AddCommand, Command, CookOpts, Opts, SimulateOpts};
use diagnostics::{CompilationError, CompilationResult, DiagnosticsBag, DiagnosticsBagRef};
use simulator::Simulator;
use text::SourceText;
use utils::VisualizerError;

mod ast;
mod blueprint;
mod cli;
mod compiler;
mod diagnostics;
mod simulator;
mod text;
mod the_chef;
mod utils;

#[cfg(test)]
mod tests;

/// Default name for simulation output.
const DEFAULT_SIMULATION_DIR: &str = "sim";

/// Compile a program as specified by the given options.
pub fn compile(opts: Rc<Opts>, cook_opts: &CookOpts) -> CompilationResult<()> {
    let path = &cook_opts.file;

    let text = SourceText::from_file(path, opts.clone())?;
    let text = Rc::new(text);

    let diagnostics_bag: DiagnosticsBagRef = DiagnosticsBag::new_ref(opts.clone());
    let ast = AST::from_source(text, diagnostics_bag.clone(), opts.clone());

    diagnostics_bag.borrow().exit_if_errored();

    if opts.verbose {
        cli::print_label("AST");
        ast.print();
    }

    diagnostics_bag.borrow().exit_if_errored();

    let graph = compiler::compile(ast, opts.clone())?;

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

    let entitites = blueprint::graph_to_entities(&cook_opts.placer, graph);

    let blueprint_str = blueprint::entities_to_blueprint_string(entitites).unwrap();

    if cook_opts.gui {
        std::process::Command::new("chef-inspector")
            .arg(&blueprint_str)
            .spawn()
            .expect("Failed to start chef-gui");
    }

    println!("{blueprint_str}");
    eprintln!();

    Ok(())
}

/// Simulate a program as specified by the given options.
fn simulate(opts: Rc<Opts>, sim_opts: &SimulateOpts) -> CompilationResult<()> {
    let path = &sim_opts.file;
    let text = Rc::new(SourceText::from_file(path, opts.clone()).unwrap());
    let diagnostics_bag = DiagnosticsBag::new_ref(opts.clone());
    let ast = AST::from_source(text, diagnostics_bag.clone(), opts.clone());

    diagnostics_bag.borrow().exit_if_errored();

    let graph = compiler::compile(ast, opts.clone())?;

    diagnostics_bag.borrow().exit_if_errored();

    let mut sim = Simulator::new(graph, vec![]);

    let out_dir = sim_opts
        .output
        .clone()
        .unwrap_or_else(|| DEFAULT_SIMULATION_DIR.to_string());

    fs::create_dir_all(&out_dir).map_err(|e| {
        CompilationError::new_generic(format!("Could not create output directory: {}", e))
    })?;
    sim.dump_simulation(sim_opts.iterations, &out_dir);

    Ok(())
}

fn run() -> CompilationResult<()> {
    let opts = Rc::new(Opts::parse());

    match &opts.command {
        Command::Cook(cook_opts) => {
            compile(opts.clone(), cook_opts)?;

            eprintln!("Enjoy!");
            Ok(())
        }
        Command::Simulate(sim_opts) => {
            simulate(opts.clone(), sim_opts)?;
            Ok(())
        }
        Command::Add(c) => match c.command {
            AddCommand::Signals(_) => {
                let cwd = env::current_dir().expect("Could not find current dir");
                utils::import_signals::import_signal_file(cwd);
                Ok(())
            }
        },
        Command::Inspect(inspect_opts) => {
            match blueprint::utils::blueprint_to_json(&inspect_opts.blueprint) {
                Ok(json) => println!("{json}"),
                Err(e) => eprintln!("{e}"),
            }
            Ok(())
        }
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{}", e.stringify());
            ExitCode::FAILURE
        }
    }
}
