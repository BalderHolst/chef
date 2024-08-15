extern crate proc_macro;
use std::{ffi::OsStr, fs};

use proc_macro::TokenStream;
use quote::{format_ident, quote};

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

    fs::read_dir(EXAMPLE_DIR).unwrap().into_iter().filter_map(|file| {
        let file = file.unwrap().path();

        if file.extension() != Some(OsStr::new("rcp")) {
            return None;
        }

        let file_name = file.file_stem().unwrap().to_str().unwrap().to_owned();
        let test_name = format_ident!("compile_example_{}", functionify(file_name));
        let file = file.clone().to_str().unwrap().to_owned();

        Some(quote! {
            #[test]
            fn #test_name() {
                let file = std::path::PathBuf::from(#file);
                let opts = std::rc::Rc::new(crate::cli::Opts::new_test());
                let text = std::rc::Rc::new(crate::text::SourceText::from_file(file.to_str().unwrap(), opts).expect("Could not read example file"));
                let opts = std::rc::Rc::new(crate::cli::Opts::new_test());
                let bag = crate::diagnostics::DiagnosticsBag::new_ref(opts.clone());
                let ast = crate::ast::AST::from_source(text.clone(), bag.clone(), opts.clone());
                bag.borrow_mut().exit_if_errored();
                let graph = crate::compiler::compile(ast, opts);
                bag.borrow_mut().exit_if_errored();
            }
        })
    }).collect::<proc_macro2::TokenStream>().into()
}
