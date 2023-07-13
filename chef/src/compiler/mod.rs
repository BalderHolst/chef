//! Module for compiling abstract syntax trees to a graph. The graph contains information about
//! how factorio combinators should be connected, and what operations they should do.

use crate::{
    ast::AST,
    diagnostics::{CompilationError, DiagnosticsBagRef},
};

use self::{graph::Graph, graph_compiler::GraphCompiler, graph_optimizer::GraphOptimizer};

pub mod graph;
mod graph_compiler;
mod graph_optimizer;
pub mod graph_visualizer;

/// Compile and abstract syntax tree in to a graph and report errors.
pub fn compile(ast: AST, diagnostics_bag: DiagnosticsBagRef) -> Result<Graph, CompilationError> {
    let mut graph_compiler = GraphCompiler::new(ast, diagnostics_bag);
    let mut graph = graph_compiler.compile()?;
    let mut graph_optimizer = GraphOptimizer::new(&mut graph);
    graph_optimizer.optimize();
    Ok(graph)
}
