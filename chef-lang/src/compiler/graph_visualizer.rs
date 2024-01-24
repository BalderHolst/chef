use std::{collections::HashSet, fs::OpenOptions, io::Write};

use crate::utils::{self, VisualizerError};

use super::graph::{Combinator, Connection, Graph, WireKind};

pub fn create_dot(graph: &Graph) -> String {
    let mut dot = "strict digraph {\n\tnodesep=1\n".to_string();

    for (vid, node) in &graph.vertices {
        let color = match node {
            super::graph::Node::Inner => "white",
            super::graph::Node::InputVariable(_) => "lightgreen",
            super::graph::Node::Variable(_) => "lightblue",
            super::graph::Node::Output(_) => "orange",
            super::graph::Node::Constant(_) => "lightgray",
        };
        let inputs = &graph.get_input_iotypes(vid);
        let input = if inputs.is_empty() {
            "CONST".to_string()
        } else {
            Vec::from_iter(inputs.iter().map(|i| i.to_string())).join(" | ")
        };
        dot += &format!(
            "\t{}\t[style=filled fillcolor={} label=\"{}\"]\n",
            vid, color, input
        );
    }

    let mut visualized_wires = HashSet::new();

    for (from_nid, to_nid, conn) in graph.iter_conns() {
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

pub fn visualize(graph: &Graph, output_path: &str) -> Result<(), VisualizerError> {
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
