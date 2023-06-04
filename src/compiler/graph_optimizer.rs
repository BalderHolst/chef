use super::graph::{Graph, VId, Node, IOType, NodeInputs};

pub struct GraphOptimizer<'a> {
    graph: &'a mut Graph,
}

impl<'a> GraphOptimizer<'a> {
    pub fn new(graph: &'a mut Graph) -> Self { Self { graph } }

    pub fn optimize(&mut self) {
        for vid in self.graph.get_input_nodes() {
            self.integrate_constant_input(vid);
        }
    }

    fn integrate_constant_input(&mut self, vid: VId) {
        // TODO: This shit is ugly...
        let input_node = self.graph.get_vertex(&vid).unwrap();
        let inputs = input_node.get_inputs(self.graph);

        if inputs.len() == 1 {
            if let IOType::Constant(_) = inputs.first().unwrap() { }
            else { return; }
        }
        else { return; };

        for (to_vid, connection) in self.graph.adjacency.get(&vid).unwrap().clone() {
            if connection.is_pick() {
                self.graph.remove_node(&vid);
                // self.graph.remove_connection(vid, to_vid.clone(), &connection);
            }
        }
    }
}
