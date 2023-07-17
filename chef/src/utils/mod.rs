//! Utilities for chef.

use std::io;

use graphviz_rust::{self, cmd::Format, printer::PrinterContext};

pub mod import_signals;

pub const BASE_SIGNALS: &str = include_str!("base.signals");

pub type VisualizerResult = Result<String, VisualizerError>;

#[derive(Debug)]
pub enum VisualizerError {
    IoErr(io::Error),
    GraphvizError(String),
}

pub fn dot_to_svg(dot: String) -> VisualizerResult {
    let dot_graph = match graphviz_rust::parse(dot.as_str()) {
        Ok(g) => g,
        Err(e) => return Err(VisualizerError::GraphvizError(e)),
    };

    match graphviz_rust::exec(
        dot_graph,
        &mut PrinterContext::default(),
        vec![Format::Svg.into()],
    ) {
        Ok(s) => Ok(s),
        Err(e) => Err(VisualizerError::IoErr(e)),
    }
}
