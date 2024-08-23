use std::{fs, path::PathBuf};

use proc_macro::{TokenStream, TokenTree};
use quote::quote;

/// TODO: Construct the expected tree. No paths with "/" in them!

#[proc_macro]
pub fn static_files(tokens: TokenStream) -> TokenStream {
    let mut tokens = tokens.into_iter().fuse();
    let root = std::env::current_dir().expect("Could not determine current directory");

    let mut paths = Vec::new();

    while let Some(token) = tokens.next() {
        match token {
            TokenTree::Literal(literal) => {
                let path = literal.to_string();
                let path = path.trim_matches('"');
                paths.push(path.to_string());

                // Consume ','
                if let Some(comma_token) = tokens.next() {
                    if let TokenTree::Punct(p) = comma_token {
                        if p.as_char() == ',' {
                            continue;
                        }
                    }
                    panic!("Expected a comma after path.")
                }
            }
            _ => panic!("Expected a string literal"),
        }
    }

    let files = paths
        .iter()
        .map(|vault_path| {
            let mut path = PathBuf::from(vault_path);
            if path.is_relative() {
                path = root.join(path);
            }
            if !path.is_file() {
                panic!("Path is not a file: {:?}", path);
            }
            let content = fs::read_to_string(&path).expect("Could not read file");
            quote! {
                static_files::File {
                    path: #vault_path,
                    contents: #content,
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        static_files::FileStash {
            files: &[#(#files),*],
        }
    }.into()
}
