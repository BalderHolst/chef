use pretty_assertions::assert_eq;

use std::rc::Rc;

use crate::{
    ast::AST,
    cli::Opts,
    compiler::{
        self,
        graph::{DetSig, Graph},
    },
    diagnostics::{CompilationResult, DiagnosticsBag},
    inputs, outputs,
    simulator::Simulator,
    text::SourceText,
};

fn compile_source(source_text: SourceText) -> CompilationResult<Graph<DetSig>> {
    let text = Rc::new(source_text);
    let opts = Rc::new(Opts::new_test());
    let bag = DiagnosticsBag::new_ref(opts.clone());
    let ast = AST::from_source(text.clone(), bag.clone(), opts);
    bag.borrow_mut().exit_if_errored();
    compiler::compile(ast)
}

fn compile_code<S>(code: S) -> Graph<DetSig>
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
    let c: counter(signal-T : 6);
    let v: var(signal-V);
    when (c == 5) {
        v <- 1;
    }
    out <- v;
}
",
    );

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
        let a: int(signal-A) <- 1;
        let b: int(signal-B) <- 2;
        let c: int(signal-C) <- 3;
        let d: int(signal-D) <- 4;
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
        let a: int <- 1;
        let b: int <- 2;
        let c: int <- 3;
        let d: int <- 4;
        out <- a+b*c+d;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn simulate_var_mutation_with_clock() {
    let g = compile_code(
        "
    block main() => (out: int(signal-O)) {
        let total: var(signal-T);
        let c: counter(signal-C : 10);
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
        let b: int <- -4-5+1;    // = -8
        out <- A + b;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(7);

    assert_eq!(sim.get_output(), outputs!["pump": -10]);
}

#[test]
fn test_declaration_tuple_unpacking() {
    let g = compile_code(
        "
    block values() => (a: int, b: int) {
        a <- 1;
        b <- 2;
    }

    block main() => (out: int(signal-E)) {
        (x: int, y: int) <- values();
        out <- x - y;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(7);

    assert_eq!(sim.get_output(), outputs!["signal-E": -1]);
}

#[test]
fn test_definition_tuple_unpacking() {
    let g = compile_code(
        "
    block values() => (a: int, b: int, c: int) {
        a <- 1;
        b <- 2;
        c <- 3;
    }

    block main() => (out: int(signal-E)) {
        let x: int;
        let y: int;
        let z: int;
        (x, y, z) <- values();
        out <- -x - y + z;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["signal-E": 0]);
}

#[test]
fn test_mixed_tuple_unpacking() {
    let g = compile_code(
        "
    block values() => (a: int, b: int, c: bool) {
        a <- 100;
        b <- -11;
        c <- true;
    }

    block main() => (out: int(signal-E)) {
        let x: int;
        let z: bool;
        (x, y: int, z) <- values();
        when z {
            out <- x - y;
        }
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["signal-E": 111]);
}
