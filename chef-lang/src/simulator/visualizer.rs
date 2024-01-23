use std::{
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

    for (vid, node) in &sim.graph.vertices {
        // Node color
        let color = match node {
            Node::Inner => "white",
            Node::InputVariable(_) => "lightgreen",
            Node::Variable(_) => "lightblue",
            Node::Output(_) => "orange",
            Node::Constant(_) => "lightgray",
        };

        // Node text
        let node_contents: String = match sim.contents.get(vid) {
            Some(items) if !items.is_empty() => items.iter().map(|item| item.to_string()).collect(),
            _ => "EMPTY".to_string(),
        };

        dot += &format!(
            "\t{vid}\t[style=filled fillcolor={color} label=\"{label}\"]\n",
            vid = vid,
            color = color,
            label = node_contents
        );
    }

    for (from_vid, to_vec) in &sim.graph.adjacency {
        for (to_vid, conn) in to_vec {
            let color = match conn {
                Connection::Wire(WireKind::Green) => "green",
                Connection::Wire(WireKind::Red) => "red",
                Connection::Combinator(com) if com.is_pick() => "black",
                Connection::Combinator(com) if com.is_convert() => "blue",
                Connection::Combinator(Combinator::Arithmetic(_)) => "orange",
                Connection::Combinator(Combinator::Decider(_)) => "purple",
                Connection::Combinator(Combinator::Gate(_)) => "teal",
                Connection::Combinator(Combinator::Constant(_)) => "brown",
            };

            dot += &format!(
                "\t{} -> {}\t[label=\"{}\" color={} fontcolor={}]\n",
                from_vid, to_vid, conn, color, color
            );
        }
    }
    dot += "}\n";
    dot
}
