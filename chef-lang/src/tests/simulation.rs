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
fn counter_with_when() {
    let graph = compile_code(
        "
block main() -> int(tank) {
    c: counter(signal-T : 6);
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

    sim.simulate(15);

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

    let steps = 20;

    let mut sim = Simulator::new(
        g.clone(),
        inputs![
                                 "signal-A": 10,
                                 "signal-B": 100,
                                 "signal-C": 1000 // Should be ignored
        ],
    );
    sim.simulate(steps);

    assert_eq!(sim.get_output(), outputs!["rail": 0]);

    let mut sim = Simulator::new(
        g.clone(),
        inputs![
                                 "signal-A": 900,
                                 "signal-B": 100,
                                 "signal-C": -1000
        ],
    );
    sim.simulate(steps);

    assert_eq!(sim.get_output(), outputs!["rail": 0]);

    let mut sim = Simulator::new(
        g.clone(),
        inputs![
                                 "signal-A": 900,
                                 "signal-B": 101,
                                 "signal-C": -10000
        ],
    );
    sim.simulate(steps);

    assert_eq!(sim.get_output(), outputs!["rail": 1]);

    let mut sim = Simulator::new(
        g,
        inputs![
                                 "signal-A": 501,
                                 "signal-B": 500,
                                 "signal-C": 10000
        ],
    );
    sim.simulate(steps);

    assert_eq!(sim.get_output(), outputs!["rail": 1]);
}

#[test]
fn order_of_operations_compile_time() {
    let g = compile_code(
        "
    block main(input: all) -> int(rail) {
        1+2*3+4
    }

",
    );
    g.print();
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn order_of_operations_constants() {
    let g = compile_code(
        "
    const A = 1
    const B = 2
    const C = 3
    const D = 4

    block main() -> int(rail) {
        A+B*C+D
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn order_of_operations_constant_evaluation() {
    let g = compile_code(
        "
    const A = 1
    const B = 2
    const C = 3
    const D = 4
    const RES = A+B*C+D

    block main() -> int(rail) {
        RES
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn order_of_operations_factorio_time() {
    let g = compile_code(
        "
    block main(input: all) -> int(rail) {
        a: int = 1;
        b: int = 2;
        c: int = 3;
        d: int = 4;
        a+b*c+d
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn return_when() {
    let g = compile_code(
        "
    block main(input: all) -> int(rail) {
        when input[signal-I] == 100 {
            -200
        }
    }

",
    );
    let mut sim = Simulator::new(g.clone(), inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 0]);
    let mut sim = Simulator::new(g, inputs!["signal-I": 100]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": -200]);
}

#[test]
fn var_mutiation_with_clock() {
    let g = compile_code(
        "
    block main() -> int(signal-O) {
        total: var(signal-T);
        c: counter(signal-C : 10);
        when c == 1 {
            total += 2;
        };
        when c == 5 {
            total -= 1;
        };
        total
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(50);
    assert_eq!(sim.get_output(), outputs!["signal-O": 5]);
}

#[test]
fn negative_numbers() {
    let g = compile_code(
        "
    const A = -5+8-5        // = -2
    
    block main() -> int(pump) {
        b: int = -4-5+1;    // = -8
        A + b
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(5);
    assert_eq!(sim.get_output(), outputs!["pump": -10]);
}
