use std::process::exit;
use std::rc::Rc;
use std::{env, io};

use ast::AST;
use blueprint_converter::BlueprintConverter;
use cli::{AddCommand, Command, CookOpts, Opts};
use diagnostics::{DiagnosticsBag, DiagnosticsBagRef};
use gumdrop::Options;
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
                VisualizerError::GraphvizError(e) => eprintln!("Error creating graph: `{}`", e),
            };
        };
    }

    if cook_opts.dot {
        let dot = graph.dot_repr();
        println!("{dot}");
        exit(0);
    }

    match BlueprintConverter::new(graph).convert_to_blueprint_string() {
        Ok(blueprint) => {
            cli_clipboard::set_contents(blueprint.clone()).unwrap();
            println!("\n\n{blueprint}\n");
        }
        Err(e) => println!("Could not create blueprint string: `{}`", e),
    }
}

fn main() -> Result<(), io::Error> {
    let opts = Rc::new(Opts::parse_args_default_or_exit());

    match &opts.command {
        Some(Command::Cook(cook_opts)) => {
            if cook_opts.files.is_empty() {
                eprintln!("{}", cook_opts.self_usage());
                exit(1);
            } else if cook_opts.files.len() != 1 {
                eprintln!(
                    "`chef cook` only accepts one file as an entry point. Found {}.",
                    cook_opts.files.len()
                );
                exit(1);
            }

            compile(opts.clone(), cook_opts);

            println!("Enjoy!");
            Ok(())
        }
        Some(Command::Add(c)) => {
            match c.command {
                Some(AddCommand::Signals(_)) => {
                    let cwd = env::current_dir().expect("Could not find current dir");
                    utils::import_signals::import_signal_file(cwd);
                    Ok(())
                }
                None => {
                    eprintln!("Only `signals` subcommand works for now"); // TODO
                    exit(1);
                }
            }
        }
        None => {
            eprintln!("{}", opts.self_usage());
            if let Some(command_list) = opts.self_command_list() {
                eprintln!("\nSubcommands:\n{}", command_list);
            }
            exit(1);
        }
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
            compile(Rc::new(Opts::default()), &CookOpts::from_files(vec![file]));
        }
    }

    #[test]
    fn check_dot_files() {
        let example_dir = "examples";
        let out_dir = "example_outputs".to_string();
        for file in fs::read_dir(example_dir).unwrap() {
            let file = file.unwrap().path();
            println!("\nCompiling: {}... ", file.display());
            let output_file =
                out_dir.clone() + "/" + file.file_stem().unwrap().to_str().unwrap() + ".dot";

            println!("\treading expected dot: \'{}\'", output_file);
            let expected_dot = fs::read_to_string(output_file).unwrap();

            let text = Rc::new(SourceText::from_file(file.to_str().unwrap()).unwrap());
            let opts = Rc::new(Opts::default());
            let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
            let ast = AST::from_source(text, bag.clone(), opts);
            bag.borrow_mut().exit_if_errored();
            let graph = compiler::compile(ast, bag.clone()).unwrap();
            bag.borrow_mut().exit_if_errored();
            let compiled_dot = graph.dot_repr() + "\n";

            // Fail test with fancy diff output
            if expected_dot != compiled_dot {
                let diff = prettydiff::diff_chars(&expected_dot, &compiled_dot)
                    .set_highlight_whitespace(true);
                println!("EXPECTED:\n{expected_dot}\n");
                println!("COMPILED:{compiled_dot}\n");
                println!("DIFF:\n{diff}\n");
                panic!("Compiled dot is different from expected.")
            }
        }
    }
}
