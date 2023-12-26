use crate::{
    compiler::graph::{Graph, IOType},
    simulator::{Item, Simulator},
};

use super::*;
use std::fs;

fn compile_source(source_text: SourceText) -> Graph {
    let text = Rc::new(source_text);
    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
    let ast = AST::from_source(text.clone(), bag.clone(), opts);
    bag.borrow_mut().exit_if_errored();
    compiler::compile(ast, bag.clone())
}

fn compile_code<S>(code: S) -> Graph
where
    S: ToString,
{
    let code = code.to_string();
    let text = SourceText::from_str(code.as_str());
    compile_source(text)
}

proc_macros::make_example_tests!();

#[test]
fn simulate_arithmetic() {
    let text = Rc::new(SourceText::from_file("./examples/arithmetic.rcp").unwrap());
    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
    let ast = AST::from_source(text.clone(), bag.clone(), opts);
    bag.borrow_mut().exit_if_errored();
    let graph = compiler::compile(ast, bag.clone());

    let mut sim = Simulator::new(graph, vec![vec![Item::new_signal("signal-B", 100)]]);

    sim.simulate(10);

    let outputs = sim.get_output();

    assert_eq!(
        outputs,
        vec![vec![Item::new(IOType::Signal("inserter".to_string()), 116)]]
    )
}
