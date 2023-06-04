use crate::ast::AST;

use self::{graph_compiler::GraphCompiler, graph::Graph, graph_optimizer::GraphOptimizer};

pub mod graph;
mod graph_visualizer;
mod graph_compiler;
mod graph_optimizer;

pub fn compile(ast: AST) -> Graph {
    let mut graph_compiler = GraphCompiler::new(ast);
    graph_compiler.compile();
    let mut graph = graph_compiler.graph;
    let mut graph_optimizer = GraphOptimizer::new(&mut graph);
    graph_optimizer.optimize();
    graph
}
