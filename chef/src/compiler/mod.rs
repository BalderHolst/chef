//! Module for compiling abstract syntax trees to a graph. The graph contains information about
//! how factorio combinators should be connected, and what operations they should do.

use crate::{ast::AST, diagnostics::DiagnosticsBagRef};

use self::{graph_compiler::GraphCompiler, graph::Graph, graph_optimizer::GraphOptimizer};

pub mod graph;
mod graph_visualizer;
mod graph_compiler;
mod graph_optimizer;

/// Compile and abstract syntax tree in to a graph and report errors.
pub fn compile(ast: AST, diagnostics_bag: DiagnosticsBagRef) -> Graph {
    let mut graph_compiler = GraphCompiler::new(ast, diagnostics_bag);
    graph_compiler.compile();
    let mut graph = graph_compiler.get_graph();
    let mut graph_optimizer = GraphOptimizer::new(&mut graph);
    graph_optimizer.optimize();
    graph.clone()
}
