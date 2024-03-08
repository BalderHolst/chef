use pretty_assertions::assert_eq;

use std::rc::Rc;

use crate::{
    ast::AST,
    cli::Opts,
    compiler::{self, graph::Graph},
    diagnostics::{CompilationResult, DiagnosticsBag},
    inputs, outputs,
    simulator::Simulator,
    text::SourceText,
};

fn compile_source(source_text: SourceText) -> CompilationResult<Graph> {
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
    compile_source(text).unwrap()
}

#[test]
fn simulate_arithmetic() {
    let graph = compile_code(
        "

block main(input: many) => (out: int(inserter)) {
    out <~ input[signal-B] * 3 + 4;
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
block main() => (out: int(tank)) {
    c: counter(signal-T : 6);
    v: var(signal-V);
    when (c == 5) {
        v <- 1;
    }
    out <- v;
}
",
    );

    graph.print();

    let mut sim = Simulator::new(graph, vec![]);

    sim.simulate(17);

    let outputs = sim.get_output();

    assert_eq!(vec![outputs[0].clone()], outputs!["tank": 2],)
}

#[test]
fn simulate_constant_int_blocks() {
    let out = compile_code(
        "
    
    block main() => (out: int(rail)) {
        out <- 100;
    }

",
    );

    let mut sim_out = Simulator::new(out, inputs![]);

    sim_out.simulate(1);

    assert_eq!(sim_out.get_output(), outputs!["rail": 100])
}

#[test]
fn simulate_constant_bool_blocks() {
    let out = compile_code(
        "
    
    block main() => (out: bool(rail)) {
        out <- true;
    }

",
    );

    let mut sim_out = Simulator::new(out, inputs![]);

    sim_out.simulate(1);

    assert_eq!(sim_out.get_output(), outputs!["rail": 1])
}

#[ignore = "Not implemented"]
#[test]
fn simulate_multiple_blocks() {
    let g = compile_code(
        "

    block over1000(a: int) => (out: bool) {
        out <- a > 1000;
    }

    block add(a: int, b: int) => (out: int) {
        out <- a + b;
    }
    
    block main(input: many) => (out: bool(rail)) {
        out <- over1000(add(input[signal-A], input[signal-B]));
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
fn simulate_order_of_operations_compile_time() {
    let g = compile_code(
        "
    block main(input: many) => (out: int(rail)) {
        out <- 1+2*3+4;
    }

",
    );
    g.print();
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn simulate_order_of_operations_constants() {
    let g = compile_code(
        "
    const A = 1
    const B = 2
    const C = 3
    const D = 4

    block main() => (out: int(rail)) {
        out <- A+B*C+D;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(1);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn simulate_order_of_operations_constant_evaluation() {
    let g = compile_code(
        "
    const A = 1
    const B = 2
    const C = 3
    const D = 4
    const RES = A+B*C+D

    block main() => (out: int(rail)) {
        out <- RES;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn simulate_order_of_operations_factorio_time() {
    let g = compile_code(
        "
    block main(input: many) => (out: int(rail)) {
        a: int(signal-A) <- 1;
        b: int(signal-B) <- 2;
        c: int(signal-C) <- 3;
        d: int(signal-D) <- 4;
        out <- a+b*c+d;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn simulate_order_of_operations_any_signals_factorio_time() {
    let g = compile_code(
        "
    block main(input: many) => (out: int(rail)) {
        a: int <- 1;
        b: int <- 2;
        c: int <- 3;
        d: int <- 4;
        out <- a+b*c+d;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.dump_simulation(10, "out");
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn simulate_var_mutation_with_clock() {
    let g = compile_code(
        "
    block main() => (out: int(signal-O)) {
        total: var(signal-T);
        c: counter(signal-C : 10);
        when c == 1 {
            total <- 2;
        }
        when c == 5 {
            total <- -1;
        }
        out <- total;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(50);
    assert_eq!(sim.get_output(), outputs!["signal-O": 5]);
}

#[test]
fn simulate_negative_numbers() {
    let g = compile_code(
        "
    const A = -5+8-5        // = -2
    
    block main() => (out: int(pump)) {
        b: int <- -4-5+1;    // = -8
        out <- A + b;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.dump_simulation(7, "out");

    assert_eq!(sim.get_output(), outputs!["pump": -10]);
}
