use pretty_assertions::assert_eq;

use std::rc::Rc;
use crate::{text::SourceText, cli::Opts, diagnostics::DiagnosticsBag, ast::AST};

#[test]
fn report_incorrect_block_arguments() {
    let code = Rc::new(SourceText::from_str(
        "
    block other(a: int, b: int) -> int {
        a + b
    }

    block main() -> int(tank) {
        var: int = 5;
        other(var)
    }
",
    ));

    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone(), code.clone());
    AST::from_source(code, bag.clone(), opts);
    bag.borrow().print();
    assert_eq!(bag.borrow().error_count(), 1);
}
