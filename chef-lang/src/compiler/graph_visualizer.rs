use std::{collections::HashMap, fmt::Debug, fmt::Display, fs::OpenOptions, io::Write};

use crate::utils::{self, VisualizerError};

use super::graph::{Connection, Graph, Node, Operation, Signal, WireConnection};

pub fn create_dot<S>(graph: &Graph<S>) -> String
where
    S: Clone + Display + Signal<S> + PartialEq + Debug,
{
    let mut dot = "strict digraph {\n\tnodesep=1\n".to_string();

    for (nid, node) in &graph.vertices {
        let color = match node {
            super::graph::Node::Inner => "white",
            super::graph::Node::InputVariable { .. } => "lightgreen",
            super::graph::Node::Variable { kind: _, name: _ } => "lightblue",
            super::graph::Node::Output { .. } => "orange",
            super::graph::Node::Constant(_) => "lightgray",
        };

        let mut label = {
            let inputs = &graph.get_input_iotypes(nid);
            if inputs.is_empty() {
                "CONST".to_string()
            } else {
                Vec::from_iter(inputs.iter().map(|(iotype, wc)| {
                    let repr = iotype.to_string();
                    match wc {
                        super::graph::WireKind::Green => format!("G[{repr}]"),
                        super::graph::WireKind::Red => format!("R[{repr}]"),
                    }
                }))
                .join(" | ")
            }
        };

        if graph.is_wire_only_node(*nid) {
            if let Some(Node::Inner) = graph.get_node(nid) {
                label = "".to_string();
            }
        }

        dot += &format!(
            "\t{}\t[style=filled fillcolor={} label=\"{}\"]\n",
            nid, color, label
        );
    }

    let mut wires = HashMap::new();
    let mut combinators = Vec::new();

    for (from_nid, to_nid, conn) in graph.iter_conns() {
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
            Connection::Operation(com) => combinators.push((from_nid, to_nid, com.clone())),
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
            Operation::Arithmetic(_) => "orange",
            Operation::Decider(_) => "purple",
            Operation::Pick(_) => "black",
            Operation::Convert(_) => "blue",
            Operation::Gate(_) => "teal",
            Operation::Delay(_) => "brown",
            Operation::Sum(_) => "lightgray",
        };
        dot += &format!(
            "\t{} -> {}\t[label=\"{}\" color={} fontcolor={}]\n",
            from_nid, to_nid, com, color, color
        );
    }

    dot += "}\n";
    dot
}

pub fn visualize<S>(graph: &Graph<S>, output_path: &str) -> Result<(), VisualizerError>
where
    S: Clone + Display + Signal<S> + PartialEq + Debug,
{
    let dot = create_dot(graph);
    let svg = utils::dot_to_svg(dot)?;

    OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(output_path)
        .map_err(VisualizerError::IoErr)?
        .write_all(svg.as_bytes())
        .map_err(VisualizerError::IoErr)
}
