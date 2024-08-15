use super::*;
use crate::{inputs, outputs, simulator::Simulator};
use pretty_assertions::assert_eq;

#[test]
fn type_inference() {
    let g = compile_code(
        "
            block main() => (out: int(car)) {
                let a: int;
                a <- 10;
                out <<- a;
            }
        ",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(5);
    let outputs = sim.get_output();
    assert_eq!(outputs, outputs!["car": 10]);
}

#[test]
fn larger_than() {
    let g = compile_code(
        "
            block main() => (out: bool(car)) {
                let a: int <- 10;
                out <<- a > 5;
            }
        ",
    );
    let mut sim = Simulator::new(g, inputs![]);
    sim.simulate(5);
    let outputs = sim.get_output();
    assert_eq!(outputs, outputs!["car": 1]);
}
