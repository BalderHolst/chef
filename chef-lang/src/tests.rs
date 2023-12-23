extern crate make_tests;

use super::*;
use std::fs;

make_tests::make_example_tests!();

// #[test]
// fn check_dot_files() {
//     let example_dir = "examples";
//     let out_dir = "example_outputs".to_string();
//     let graph_out = "test.svg";
//     for file in fs::read_dir(example_dir).unwrap() {
//         let file = file.unwrap().path();
//         println!("\nCompiling: {}... ", file.display());
//         let output_file =
//             out_dir.clone() + "/" + file.file_stem().unwrap().to_str().unwrap() + ".dot";

//         println!("\treading expected dot: \'{}\'", output_file);
//         let expected_dot = fs::read_to_string(output_file).unwrap();

//         let text = Rc::new(SourceText::from_file(file.to_str().unwrap()).unwrap());
//         let opts = Rc::new(Opts::new_test());
//         let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
//         let ast = AST::from_source(text.clone(), bag.clone(), opts);
//         bag.borrow_mut().exit_if_errored();
//         let graph = compiler::compile(ast, bag.clone()).unwrap();
//         bag.borrow_mut().exit_if_errored();
//         let compiled_dot = graph.dot_repr() + "\n";

//         // Fail test with fancy diff output
//         if expected_dot != compiled_dot {
//             let diff =
//                 prettydiff::diff_chars(&expected_dot, &compiled_dot).set_highlight_whitespace(true);
//             let _ = graph.visualize(graph_out);
//             cli::print_label("CODE");
//             println!("{}\n", text.text());
//             println!("Graph saved to {}\n", graph_out);
//             cli::print_label("DIFF");
//             println!("{diff}\n");
//             panic!(
//                 "Compiled dot is different from expected in \"{}\".",
//                 file.display()
//             )
//         }
//     }
// }
