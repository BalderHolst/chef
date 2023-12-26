extern crate proc_macro;
use std::{ffi::OsStr, fs};

use proc_macro::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};

const EXAMPLE_DIR: &str = "examples";

/// Turn a name into a valid function name by replacing bad characters.
fn functionify(name: String) -> String {
    name.chars()
        .map(|c| match c {
            '-' => '_',
            other => other,
        })
        .collect()
}

/// Create test cases for each example in the `examples` directory.
#[proc_macro]
pub fn make_example_tests(_item: TokenStream) -> TokenStream {
    let mut stream = TokenStream::new();

    let generic_test_function: TokenStream = r#"
        fn compile_example(file: &str, output_file: &str) {
            println!("Expected output file: {}", output_file);
            let file = std::path::PathBuf::from(file);
            let expected_dot = fs::read_to_string(output_file).unwrap();
            let text = Rc::new(SourceText::from_file(file.to_str().unwrap()).unwrap());
            let opts = Rc::new(Opts::new_test());
            let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
            let ast = AST::from_source(text.clone(), bag.clone(), opts);
            bag.borrow_mut().exit_if_errored();
            let graph = compiler::compile(ast, bag.clone());
            bag.borrow_mut().exit_if_errored();
            let compiled_dot = graph.dot_repr() + "\n";

            // Fail test with fancy diff output
            if expected_dot != compiled_dot {
                let diff = prettydiff::diff_chars(&expected_dot, &compiled_dot).set_highlight_whitespace(true);
                cli::print_label("CODE");
                println!("{}\n", text.text());
                cli::print_label("DIFF");
                println!("{diff}\n");
                panic!(
                      "Compiled dot is different from expected in \"{}\".",
                      file.display()
                )
            }
        }
    "#.parse().unwrap();

    stream.extend(generic_test_function);

    for file in fs::read_dir(EXAMPLE_DIR).unwrap() {
        let file = file.unwrap().path();

        if file.extension() != Some(OsStr::new("rcp")) {
            continue;
        }

        let file_name = file.file_stem().unwrap().to_str().unwrap().to_owned();

        let output_file = file.clone().with_extension("output.dot");

        let test_name = format!("compile_example_{}", &file_name);
        let test_name = functionify(test_name);

        let test_body = format!(
            "compile_example(\"{file}\", \"{output_file}\");",
            file = file.as_os_str().to_str().unwrap(),
            output_file = output_file.as_os_str().to_str().unwrap()
        );

        let test = [
            TokenTree::Punct(Punct::new('#', Spacing::Alone)),
            TokenTree::Group(Group::new(Delimiter::Bracket, "test".parse().unwrap())),
            TokenTree::Ident(Ident::new("fn", Span::call_site())),
            TokenTree::Ident(Ident::new(test_name.as_str(), Span::call_site())),
            TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
            TokenTree::Group(Group::new(Delimiter::Brace, test_body.parse().unwrap())),
        ];

        stream.extend(test);
    }

    stream
}

// #[test]
// fn compile_examples() {
//     let example_dir = "examples";
//     for file in fs::read_dir(example_dir).unwrap() {
//         let file = file.unwrap().path().to_str().unwrap().to_owned();
//         println!("Compiling: \'{}\'... ", file);
//         compile(Rc::new(Opts::new_test()), &CookOpts::from_files(vec![file]));
//     }
// }
