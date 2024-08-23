use super::*;
use crate::{inputs, outputs, simulator::Simulator};
use pretty_assertions::assert_eq;

#[test]
fn arithmetic() {
    let graph = compile_code(
        "

block main(input: many) => (out: int(inserter)) {
    out <<~ input[signal-B] * 3 + 4;
}

",
    );

    let mut sim = Simulator::new(graph, inputs!["signal-B": 100]);

    sim.simulate(10);

    let outputs = sim.get_output();

    assert_eq!(outputs, outputs!["inserter": 304])
}

#[test]
fn counter() {
    let graph = compile_code(
        "
import std

block main() => (out: int(tank)) {
    out <<- std::counter(100);
}
",
    );

    let mut sim = Simulator::new(graph, vec![]);

    sim.simulate(10);

    let outputs = sim.get_output();

    assert_eq!(vec![outputs[0].clone()], outputs!["tank": 7],)
}

#[test]
fn constant_int_blocks() {
    let out = compile_code(
        "

    block main() => (out: int(rail)) {
        out <<- 100;
    }

",
    );

    let mut sim_out = Simulator::new(out, inputs![]);

    sim_out.simulate(2);

    assert_eq!(sim_out.get_output(), outputs!["rail": 100])
}

#[test]
fn constant_bool_blocks() {
    let out = compile_code(
        "
    
    block main() => (out: bool(rail)) {
        out <<- true;
    }

",
    );

    let mut sim_out = Simulator::new(out, inputs![]);

    sim_out.simulate(2);

    assert_eq!(sim_out.get_output(), outputs!["rail": 1])
}

#[test]
fn multiple_blocks() {
    let g = compile_code(
        "

    block over1000(a: int) => (out: bool) {
        out <- a > 1000;
    }

    block add(a: int, b: int) => (out: int) {
        out <- a + b;
    }

    block main(input: many) => (out: bool(rail)) {
        out <<- over1000(add(input[signal-A], input[signal-B]));
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
    block main(input: many) => (out: int(rail)) {
        out <<- 1+2*3+4;
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

    block main() => (out: int(rail)) {
        out <<- A+B*C+D;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(2);
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

    block main() => (out: int(rail)) {
        out <<- RES;
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
    block main(input: many) => (out: int(rail)) {
        let a: int(signal-A) <<- 1;
        let b: int(signal-B) <<- 2;
        let c: int(signal-C) <<- 3;
        let d: int(signal-D) <<- 4;
        out <<- a+b*c+d;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn order_of_operations_any_signals_factorio_time() {
    let g = compile_code(
        "
    block main(input: many) => (out: int(rail)) {
        let a: int <- 1;
        let b: int <- 2;
        let c: int <- 3;
        let d: int <- 4;
        out <<- a+b*c+d;
    }

",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);
    assert_eq!(sim.get_output(), outputs!["rail": 11]);
}

#[test]
fn negative_numbers() {
    let g = compile_code(
        "
    const A = -5+8-5 // = -2

    block main() => (out: int(pump)) {
        let b: int <- -4-5+1; // = -8
        out <<- A + b;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(7);

    assert_eq!(sim.get_output(), outputs!["pump": -10]);
}

#[test]
fn declaration_tuple_unpacking() {
    let g = compile_code(
        "
    block values() => (a: int, b: int) {
        a <- 1;
        b <- 2;
    }

    block main() => (out: int(signal-E)) {
        (x: int, y: int) <- values();
        out <<- x - y;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(7);

    assert_eq!(sim.get_output(), outputs!["signal-E": -1]);
}

#[test]
fn definition_tuple_unpacking() {
    let g = compile_code(
        "
    block values() => (a: int, b: int, c: int) {
        a <- 1;
        b <- 2;
        c <- 13;
    }

    block main() => (out: int(signal-E)) {
        let x: int(signal-X);
        let y: int;
        let z: int(signal-Z);
        (x, y, z) <- values();
        out <<- -x - y + z;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["signal-E": 10]);
}

#[test]
fn mixed_tuple_unpacking() {
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
        out <<- x - y;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["signal-E": 111]);
}

#[test]
fn when_statement() {
    let g = compile_code(
        "
    block main(input: many) => (out: int(signal-B)) {
        when input[signal-A] > 10 {
            out <<- 20;
        }
    }
",
    );
    let n = 6;

    let mut sim = Simulator::new(g.clone(), inputs!["signal-A": 0]);
    sim.simulate(n);
    assert_eq!(sim.get_output(), outputs!["signal-B": 0]);

    let mut sim = Simulator::new(g.clone(), inputs!["signal-A": 10]);
    sim.simulate(n);
    assert_eq!(sim.get_output(), outputs!["signal-B": 0]);

    let mut sim = Simulator::new(g, inputs!["signal-A": 11]);
    sim.simulate(n);
    assert_eq!(sim.get_output(), outputs!["signal-B": 20]);
}

#[test]
fn gate_expression() {
    let g = compile_code(
        "
    block main(input: many) => (out: many) {

        let a: int(signal-A) <<- 10;

        out <- ? input[pump] < 2 <- a;
    }
",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(10);

    assert_eq!(sim.get_output(), outputs!["signal-A": 10]);
}
