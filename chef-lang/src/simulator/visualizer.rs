use std::{
    collections::HashSet,
    fs,
    io::{self, Write},
};

use crate::{
    compiler::graph::{Combinator, Connection, Node, WireKind},
    utils,
};

use super::Simulator;

/// Visualize the simulator state with graphvis and save the svg to a file path.
pub(crate) fn visualize_simulator(sim: &Simulator, path: &str) -> io::Result<()> {
    let dot = simulator_to_dot(sim);
    let svg = utils::dot_to_svg(dot).unwrap();

    fs::OpenOptions::new()
        .truncate(true)
        .write(true)
        .create(true)
        .open(path)?
        .write_all(svg.as_bytes())?;

    io::Result::Ok(())
}

/// Create a dot graph representing the simulator state
pub(crate) fn simulator_to_dot(sim: &Simulator) -> String {
    let mut dot = "strict digraph {\n\tnodesep=1\n".to_string();

    for (nid, node) in &sim.graph.vertices {
        // Node color
        let color = match node {
            Node::Inner => "white",
            Node::InputVariable(_) => "lightgreen",
            Node::Variable(_) => "lightblue",
            Node::Output(_) => "orange",
            Node::Constant(_) => "lightgray",
        };

        // Node text
        let node_contents: String = match sim.get_node_contents(nid) {
            items if !items.is_empty() => items.iter().map(|item| item.to_string()).collect(),
            _ => "EMPTY".to_string(),
        };

        dot += &format!(
            "\t{nid}\t[style=filled fillcolor={color} label=\"{label}\"]\n",
            nid = nid,
            color = color,
            label = node_contents
        );
    }

    let mut visualized_wires = HashSet::new();

    for (from_nid, to_nid, conn) in sim.graph.iter_conns() {
        match &conn {
            Connection::Wire(wire_color) => {
                let is_new_wire = match from_nid < to_nid {
                    true => visualized_wires.insert((from_nid, to_nid)),
                    false => visualized_wires.insert((to_nid, from_nid)),
                };

                if !is_new_wire {
                    continue;
                }

                let color = match wire_color {
                    WireKind::Green => "green",
                    WireKind::Red => "red",
                };
                dot += &format!(
                    "\t{} -> {}\t[label=\"{}\" color={} fontcolor={} dir=\"both\"]\n",
                    from_nid, to_nid, conn, color, color
                );
            }
            Connection::Combinator(com) => {
                let color = match com {
                    com if com.is_pick() => "black",
                    com if com.is_convert() => "blue",
                    Combinator::Arithmetic(_) => "orange",
                    Combinator::Decider(_) => "purple",
                    Combinator::Gate(_) => "teal",
                    Combinator::Constant(_) => "brown",
                };
                dot += &format!(
                    "\t{} -> {}\t[label=\"{}\" color={} fontcolor={}]\n",
                    from_nid, to_nid, conn, color, color
                );
            }
        };
    }
    dot += "}\n";
    dot
}
