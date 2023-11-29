use std::process::exit;
use std::rc::Rc;
use std::{env, io};

use ast::AST;
use clap::Parser;
use cli::{AddCommand, Command, CookOpts, Opts};
use diagnostics::{DiagnosticsBag, DiagnosticsBagRef};
use text::SourceText;
use utils::VisualizerError;

mod ast;
mod blueprint_converter;
mod cli;
mod compiler;
mod diagnostics;
mod text;
mod the_chef;
mod utils;

pub fn compile(opts: Rc<Opts>, cook_opts: &CookOpts) {
    let path = cook_opts.files.get(0).unwrap();
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

    if let Err(e) = graph.clone() {
        diagnostics_bag.borrow_mut().report_compilation_error(e);
    }
    diagnostics_bag.borrow().exit_if_errored();

    let graph = graph.expect("Error case handled above");

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

fn main() -> Result<(), io::Error> {
    let opts = Rc::new(Opts::parse());

    match &opts.command {
        Command::Cook(cook_opts) => {
            compile(opts.clone(), cook_opts);

            eprintln!("Enjoy!");
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn compile_examples() {
        let example_dir = "examples";
        for file in fs::read_dir(example_dir).unwrap() {
            let file = file.unwrap().path().to_str().unwrap().to_owned();
            println!("Compiling: \'{}\'... ", file);
            compile(Rc::new(Opts::new_test()), &CookOpts::from_files(vec![file]));
        }
    }

    #[test]
    fn check_dot_files() {
        let example_dir = "examples";
        let out_dir = "example_outputs".to_string();
        let graph_out = "test.svg";
        for file in fs::read_dir(example_dir).unwrap() {
            let file = file.unwrap().path();
            println!("\nCompiling: {}... ", file.display());
            let output_file =
                out_dir.clone() + "/" + file.file_stem().unwrap().to_str().unwrap() + ".dot";

            println!("\treading expected dot: \'{}\'", output_file);
            let expected_dot = fs::read_to_string(output_file).unwrap();

            let text = Rc::new(SourceText::from_file(file.to_str().unwrap()).unwrap());
            let opts = Rc::new(Opts::new_test());
            let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
            let ast = AST::from_source(text.clone(), bag.clone(), opts);
            bag.borrow_mut().exit_if_errored();
            let graph = compiler::compile(ast, bag.clone()).unwrap();
            bag.borrow_mut().exit_if_errored();
            let compiled_dot = graph.dot_repr() + "\n";

            // Fail test with fancy diff output
            if expected_dot != compiled_dot {
                let diff = prettydiff::diff_chars(&expected_dot, &compiled_dot)
                    .set_highlight_whitespace(true);
                let _ = graph.visualize(graph_out);
                cli::print_label("CODE");
                println!("{}\n", text.text());
                println!("Graph saved to {}\n", graph_out);
                cli::print_label("DIFF");
                println!("{diff}\n");
                panic!(
                    "Compiled dot is different from expected in \"{}\".",
                    file.display()
                )
            }
        }
    }
}
