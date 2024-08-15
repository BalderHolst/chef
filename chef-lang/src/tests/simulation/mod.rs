mod complex;
mod trivial;

use std::rc::Rc;

use crate::{
    ast::AST,
    cli::Opts,
    compiler::{
        self,
        graph::{DetSig, Graph},
    },
    diagnostics::{CompilationResult, DiagnosticsBag},
    text::SourceText,
};

fn compile_source(source_text: SourceText) -> CompilationResult<Graph<DetSig>> {
    let text = Rc::new(source_text);
    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone());
    let ast = AST::from_source(text.clone(), bag.clone(), opts.clone());
    bag.borrow_mut().exit_if_errored();
    compiler::compile(ast, opts)
}

fn compile_code<S>(code: S) -> Graph<DetSig>
where
    S: ToString,
{
    let code = code.to_string();
    let text = SourceText::from_str(code.as_str());
    compile_source(text).unwrap()
}
