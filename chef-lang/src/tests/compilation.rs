use pretty_assertions::assert_eq;

use crate::{ast::AST, cli::Opts, diagnostics::DiagnosticsBag, text::SourceText};
use std::rc::Rc;

#[test]
fn report_incorrect_block_arguments() {
    let code = Rc::new(SourceText::from_str(
        "
    block other(a: int, b: int) => (out: int) {
        out <- a + b;
    }

    block main() => (out: int(tank)) {
        let var: int = 5;
        out <- other(var);
    }
",
    ));

    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone());
    AST::from_source(code, bag.clone(), opts);
    bag.borrow().print();
    assert_eq!(bag.borrow().error_count(), 1);
}

#[test]
fn type_inference() {
    let code = Rc::new(SourceText::from_str(
        "
        block main() => (out: int) {
            let a = 10;
            let b = true;
            out <- a + b;
        }
",
    ));

    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone());
    AST::from_source(code, bag.clone(), opts);
    bag.borrow().print();
    assert_eq!(bag.borrow().error_count(), 2);
}
