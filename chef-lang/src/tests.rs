extern crate make_tests;

use crate::simulator::{Item, Simulator};

use super::*;
use std::fs;

make_tests::make_example_tests!();

#[test]
fn simulate_arithmetic() {
    let text = Rc::new(SourceText::from_file("./examples/arithmetic.rcp").unwrap());
    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
    let ast = AST::from_source(text.clone(), bag.clone(), opts);
    bag.borrow_mut().exit_if_errored();
    let graph = compiler::compile(ast, bag.clone()).unwrap();

    let mut sim = Simulator::new(graph, vec![vec![Item::new("signal-B", 0)]]);

    sim.simulate(10);

    todo!()
}
