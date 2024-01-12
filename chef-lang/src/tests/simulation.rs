use pretty_assertions::assert_eq;

use std::rc::Rc;

use crate::{
    ast::AST,
    cli::Opts,
    compiler::{self, graph::Graph},
    diagnostics::DiagnosticsBag,
    inputs, outputs,
    simulator::Simulator,
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

    let mut sim = Simulator::new(graph, inputs!["signal-B": 100]);

    sim.simulate(10);

    let outputs = sim.get_output();

    assert_eq!(outputs, outputs!["inserter": 304])
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

    assert_eq!(vec![outputs[0].clone()], outputs!["tank": 2],)
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
    let expected = outputs!["signal-0": 100];
    // vec![vec![Item::new_signal("signal-0", 100)]];
    assert_eq!(sim.get_output(), expected);

    // a = 0 and b = 10
    let mut sim = Simulator::new(
        graph.clone(),
        inputs! {
            ["signal-B": 10];
            ["signal-A":  0];
        },
    );
    sim.simulate(10);
    let expected = outputs!["signal-0": 0];
    assert_eq!(sim.get_output(), expected);

    // a = 10 and b = 10
    let mut sim = Simulator::new(
        graph.clone(),
        inputs! {
            ["signal-B": 10];
            ["signal-A": 10];
        },
    );
    sim.simulate(10);
    let expected = outputs!["signal-0": 100];

    assert_eq!(sim.get_output(), expected);
}

#[test]
fn constant_int_blocks() {
    let out = compile_code(
        "
    
    block main() -> int(rail) {
        out 100;
    }

",
    );

    let no_out = compile_code(
        "
    
    block main() -> int(rail) {
        100
    }

",
    );

    let mut sim_out = Simulator::new(out, inputs![]);
    let mut sim_no_out = Simulator::new(no_out, inputs![]);

    for _ in 0..20 {
        assert_eq!(sim_out.get_output(), sim_no_out.get_output());
        sim_out.simulate(1);
        sim_no_out.simulate(1);
    }

    assert_eq!(sim_out.get_output(), outputs!["rail": 100])
}

#[test]
fn constant_bool_blocks() {
    let out = compile_code(
        "
    
    block main() -> bool(rail) {
        out true;
    }

",
    );

    let no_out = compile_code(
        "
    
    block main() -> bool(rail) {
        true
    }

",
    );

    let mut sim_out = Simulator::new(out, inputs![]);
    let mut sim_no_out = Simulator::new(no_out, inputs![]);

    for _ in 0..20 {
        assert_eq!(sim_out.get_output(), sim_no_out.get_output());
        sim_out.simulate(1);
        sim_no_out.simulate(1);
    }

    assert_eq!(sim_out.get_output(), outputs!["rail": 1])
}

#[test]
fn multiple_blocks() {
    let g = compile_code(
        "

    block over1000(a: int) -> bool {
        a > 1000
    }

    block add(a: int, b: int) -> int {
        a + b
    }
    
    block main(input: all) -> bool(rail) {
        over1000(add(input[signal-A], input[signal-B]))
    }

",
    );

    let mut sim = Simulator::new(
        g.clone(),
        inputs![
                                 "signal-A": 10,
                                 "signal-B": 100,
                                 "signal-C": 1000 // Should be ignored
        ],
    );
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["rail": 0]);

    let mut sim = Simulator::new(
        g.clone(),
        inputs![
                                 "signal-A": 900,
                                 "signal-B": 100,
                                 "signal-C": -1000
        ],
    );
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["rail": 0]);

    let mut sim = Simulator::new(
        g.clone(),
        inputs![
                                 "signal-A": 900,
                                 "signal-B": 101,
                                 "signal-C": -10000
        ],
    );
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["rail": 1]);

    let mut sim = Simulator::new(
        g,
        inputs![
                                 "signal-A": 501,
                                 "signal-B": 500,
                                 "signal-C": 10000
        ],
    );
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["rail": 1]);
}
