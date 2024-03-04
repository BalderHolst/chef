//! Module for compiling abstract syntax trees to a graph. The graph contains information about
//! how factorio combinators should be connected, and what operations they should do.

use crate::ast::AST;

use self::{graph::Graph, graph_compiler::GraphCompiler, graph_optimizer::GraphOptimizer};

pub mod graph;
mod graph_compiler;
mod graph_optimizer;
pub mod graph_visualizer;

/// A signal used for gates that allow all signals through. All except this reserved signal of
/// course.
pub const RESERVED_SIGNAL: &str = "signal-dot";

/// Compile and abstract syntax tree in to a graph and report errors.
pub fn compile(ast: AST) -> Graph {
    let mut graph_compiler = GraphCompiler::new(ast);
    let mut graph = match graph_compiler.compile() {
        Ok(g) => g,
        Err(e) => panic!("{e:?}"),
    };
    let mut graph_optimizer = GraphOptimizer::new(&mut graph);
    graph_optimizer.optimize();
    graph.assign_anysignals();
    graph
}
