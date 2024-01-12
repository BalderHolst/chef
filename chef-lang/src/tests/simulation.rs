use pretty_assertions::assert_eq;

use std::rc::Rc;

use crate::{
    ast::AST,
    cli::Opts,
    compiler::{
        self,
        graph::{Graph, IOType},
    },
    diagnostics::DiagnosticsBag,
    items,
    simulator::{Item, Simulator},
    text::SourceText,
};

fn compile_source(source_text: SourceText) -> Graph {
    let text = Rc::new(source_text);
    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone(), text.clone());
    let ast = AST::from_source(text.clone(), bag.clone(), opts);
    bag.borrow_mut().exit_if_errored();
    compiler::compile(ast)
}

fn compile_code<S>(code: S) -> Graph
where
    S: ToString,
{
    let code = code.to_string();
    let text = SourceText::from_str(code.as_str());
    compile_source(text)
}

#[test]
fn simulate_arithmetic() {
    let graph = compile_code(
        "

block main(input: all) -> int(inserter) {
    out input[signal-B] * 3 + 4;
}

",
    );

    let mut sim = Simulator::new(graph, vec![items!["signal-B": 100]]);

    sim.simulate(10);

    let outputs = sim.get_output();

    assert_eq!(outputs, vec![items!["inserter":304]])
}

#[test]
fn simulate_counter_with_when() {
    let graph = compile_code(
        "
block main() -> int(tank) {
    c: counter(signal-T : 5);
    v: var(signal-V);
    when (c == 5) {
        v += 1;
    };
    out v;
}

",
    );

    graph.print();

    let mut sim = Simulator::new(graph, vec![]);

    sim.simulate(14);

    let outputs = sim.get_output();

    assert_eq!(vec![outputs[0].clone()], vec![items!["tank":2]])
}

#[test]
fn simulate_when_as_expression() {
    let graph = compile_code(
        "
        block main(a: int(signal-A), b: int(signal-B)) -> int(signal-0) {
            out when (a == b) {
                out 100;
            };
        }
",
    );

    // a = 0 and b = 0
    let mut sim = Simulator::new(graph.clone(), vec![]);
    sim.simulate(10);
    let expected = vec![vec![Item::new_signal("signal-0", 100)]];
    assert_eq!(sim.get_output(), expected);

    // a = 0 and b = 10
    let mut sim = Simulator::new(
        graph.clone(),
        vec![items!["signal-B": 10], items!["signal-A": 0]],
    );
    sim.simulate(10);
    let expected = vec![vec![Item::new_signal("signal-0", 0)]];
    assert_eq!(sim.get_output(), expected);

    // a = 10 and b = 10
    let mut sim = Simulator::new(
        graph.clone(),
        vec![items!["signal-B": 10], items!["signal-A": 10]],
    );
    sim.simulate(10);
    let expected = vec![vec![Item::new_signal("signal-0", 100)]];
    assert_eq!(sim.get_output(), expected);
}
