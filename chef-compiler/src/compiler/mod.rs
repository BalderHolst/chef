//! Module for compiling abstract syntax trees to a graph. The graph contains information about
//! how factorio combinators should be connected, and what operations they should do.

use std::rc::Rc;

use crate::{ast::AST, cli::Opts, diagnostics::CompilationResult};

use self::{
    graph::{DetSig, Graph},
    graph_compiler::GraphCompiler,
};

pub mod graph;
mod graph_compiler;
pub mod graph_visualizer;
mod signal_assigner;

/// A signal used for gates that allow all signals through. All except this reserved signal of
/// course.
pub const RESERVED_SIGNAL: &str = "signal-dot";

/// Compile and abstract syntax tree in to a graph and report errors.
pub fn compile(ast: AST, opts: Rc<Opts>) -> CompilationResult<Graph<DetSig>> {
    let mut graph_compiler = GraphCompiler::new(ast);
    graph_compiler.compile()?;
    let loose_graph = graph_compiler.get_block_graph("main", None, None)?;
    let graph = signal_assigner::assign_signals(loose_graph, opts.verbose);
    Ok(graph)
}
