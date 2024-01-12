use std::{fs::OpenOptions, io::Write};

use crate::utils::{self, VisualizerError};

use super::graph::Graph;

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

    for (from_vid, to_vec) in &graph.adjacency {
        for (to_vid, conn) in to_vec {
            let color = if conn.is_pick() {
                "red"
            } else if conn.is_convert() {
                "blue"
            } else {
                match conn {
                    super::graph::Connection::Arithmetic(_) => "black",
                    super::graph::Connection::Decider(_) => "purple",
                    super::graph::Connection::Gate(_) => "teal",
                    super::graph::Connection::Constant(_) => "green",
                }
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
