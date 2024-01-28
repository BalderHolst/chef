use std::{
    collections::{HashMap, HashSet},
    fs,
    io::{self, Write},
};

use crate::{
    compiler::graph::{Combinator, Connection, Node, WireConnection, WireKind},
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
        let mut node_contents: String = match sim.get_node_contents(nid) {
            items if !items.is_empty() => items.iter().map(|item| item.to_string()).collect(),
            _ => "EMPTY".to_string(),
        };

        if sim.graph.is_wire_only_node(*nid) {
            if let Some(Node::Inner) = sim.graph.get_node(nid) {
                node_contents = "".to_string();
            }
        }

        dot += &format!(
            "\t{nid}\t[style=filled fillcolor={color} label=\"{label}\"]\n",
            nid = nid,
            color = color,
            label = node_contents
        );
    }

    let mut wires = HashMap::new();
    let mut combinators = Vec::new();

    for (from_nid, to_nid, conn) in sim.graph.iter_conns() {
        match &conn {
            Connection::Wire(wk) => {
                // Avoid wire duplication
                if from_nid > to_nid {
                    continue;
                }
                wires
                    .entry((from_nid, to_nid))
                    .and_modify(|wk| {
                        *wk = WireConnection::Both;
                    })
                    .or_insert(WireConnection::from_wire_kind(wk));
            }
            Connection::Combinator(com) => combinators.push((from_nid, to_nid, com.clone())),
        }
    }

    for ((from_nid, to_nid), wire) in wires {
        let color = match wire {
            WireConnection::Green => "green",
            WireConnection::Red => "red",
            WireConnection::Both => "lightblue",
        };

        dot += &format!(
            "\t{} -> {}\t[label=\"\" color={} fontcolor={} dir=\"both\"]\n",
            from_nid, to_nid, color, color
        );
    }

    for (from_nid, to_nid, com) in combinators {
        let color = match &com {
            com if com.is_pick() => "black",
            com if com.is_convert() => "blue",
            Combinator::Arithmetic(_) => "orange",
            Combinator::Decider(_) => "purple",
            Combinator::Gate(_) => "teal",
            Combinator::Constant(_) => "brown",
        };
        dot += &format!(
            "\t{} -> {}\t[label=\"{}\" color={} fontcolor={}]\n",
            from_nid, to_nid, com, color, color
        );
    }

    // for (from_nid, to_nid, conn) in sim.graph.iter_conns() {
    // match &conn {
    //     Connection::Wire(wire_color) => {

    //         let color = match wire_color {
    //             WireKind::Green => "green",
    //             WireKind::Red => "red",
    //         };
    //         dot += &format!(
    //             "\t{} -> {}\t[label=\"{}\" color={} fontcolor={} dir=\"both\"]\n",
    //             from_nid, to_nid, conn, color, color
    //         );
    //     }
    //     Connection::Combinator(com) => {
    //         let color = match com {
    //             com if com.is_pick() => "black",
    //             com if com.is_convert() => "blue",
    //             Combinator::Arithmetic(_) => "orange",
    //             Combinator::Decider(_) => "purple",
    //             Combinator::Gate(_) => "teal",
    //             Combinator::Constant(_) => "brown",
    //         };
    //         dot += &format!(
    //             "\t{} -> {}\t[label=\"{}\" color={} fontcolor={}]\n",
    //             from_nid, to_nid, conn, color, color
    //         );
    //     }
    // };
    // }
    dot += "}\n";
    dot
}
