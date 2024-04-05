//! Module for compiling abstract syntax trees to a graph. The graph contains information about
//! how factorio combinators should be connected, and what operations they should do.

use crate::{ast::AST, diagnostics::CompilationResult};

use self::{
    graph::{Graph, LooseSig},
    graph_compiler::GraphCompiler,
};

pub mod graph;
mod graph_compiler;
pub mod graph_visualizer;

/// A signal used for gates that allow all signals through. All except this reserved signal of
/// course.
pub const RESERVED_SIGNAL: &str = "signal-dot";

/// Compile and abstract syntax tree in to a graph and report errors.
pub fn compile(ast: AST) -> CompilationResult<Graph<LooseSig>> {
    let mut graph_compiler = GraphCompiler::new(ast);
    let mut graph = graph_compiler.compile()?;
    graph.assign_anysignals();
    Ok(graph)
}
