use super::graph::{Graph, LooseSig};

pub struct GraphOptimizer<'a> {
    _graph: &'a mut Graph<LooseSig>,
}

impl<'a> GraphOptimizer<'a> {
    pub fn new(graph: &'a mut Graph<LooseSig>) -> Self {
        Self { _graph: graph }
    }

    pub fn optimize(&mut self) {
        // TODO: This breaks wires
        // self.remove_redundant_picks();
    }
}
