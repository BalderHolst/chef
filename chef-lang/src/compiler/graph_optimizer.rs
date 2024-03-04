use super::graph::Graph;

pub struct GraphOptimizer<'a> {
    _graph: &'a mut Graph,
}

impl<'a> GraphOptimizer<'a> {
    pub fn new(graph: &'a mut Graph) -> Self {
        Self { _graph: graph }
    }

    pub fn optimize(&mut self) {
        // TODO: This breaks wires
        // self.remove_redundant_picks();
    }
}
