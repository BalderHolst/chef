use std::fs;

use proc_macro::{TokenStream, TokenTree};
use quote::quote;

fn to_static_file(path_str: &String) -> proc_macro2::TokenStream {
    let root = std::env::current_dir().expect("Could not determine current directory");
    let path = root.join(path_str);
    if path.is_file() {
        let file_contents = fs::read_to_string(path).unwrap();
        quote! {
            static_files::StaticFile::File {
                name: #path_str,
                contents: #file_contents,
            }
        }
    }
    else if path.is_dir() {
        let files = fs::read_dir(&path).unwrap().into_iter().map(|file| {
            let file = file.unwrap();
            let file_path = file.path();
            let file_path = file_path.to_str().unwrap().to_string();
            to_static_file(&file_path)
        }).collect::<Vec<_>>();

        quote! {
            static_files::StaticFile::Directory {
                name: #path_str,
                files: &[#(#files),*],
            }
        }

    }
    else {
        panic!("Could not read file '{}'. It is neither file or directory.", path.display());
    }
    .into()
}

#[proc_macro]
pub fn static_file(tokens: TokenStream) -> TokenStream {
    let mut tokens = tokens.into_iter();

    match tokens.next() {
        Some(TokenTree::Literal(lit)) => {
            let file_path = (&lit.to_string()[1..lit.to_string().len() - 1]).to_string();
            to_static_file(&file_path).into()
        }
        other => {
            panic!("Expected a string literal. Got: {:?}", other);
        }
    }
}
