use super::graph::{Graph, VId, IOType, NodeInputs};

pub struct GraphOptimizer<'a> {
    graph: &'a mut Graph,
}

impl<'a> GraphOptimizer<'a> {
    pub fn new(graph: &'a mut Graph) -> Self { Self { graph } }

    pub fn optimize(&mut self) {
        for vid in self.graph.get_input_nodes() {
            self.integrate_constant_input(vid);
        }
        self.remove_redundant_picks();
    }

    fn remove_redundant_picks(&mut self) {
        while let Some((from_vid, middle_vid, to_vid, index)) = self.get_redundant_pick() {
            if let Some(to_vec) = self.graph.adjacency.get_mut(&from_vid) {
                to_vec[index].0 = to_vid;
                self.graph.vertices.remove(&middle_vid);
            }
        }
    }

    fn get_redundant_pick(&mut self) -> Option<(VId, VId, VId, usize)> {
        for (from_vid, middle_vec) in self.graph.adjacency.iter() {
            for (i, (middle_vid, _conn1)) in middle_vec.clone().iter().enumerate() {
                if let Some(to_vec) = self.graph.adjacency.get(middle_vid) {
                    if to_vec.len() != 1 { continue; }
                    let (to_vid, conn2) = to_vec.first().unwrap();
                    if !conn2.is_pick() { continue; }
                    println!("({}, {}, {}) -> ({}, {})", from_vid, middle_vid, to_vid, from_vid, to_vid);
                    return Some((from_vid.clone(), middle_vid.clone(), to_vid.clone(), i))
                }
            }
        }
        None
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
                self.graph.remove_node_with_connections(&vid);
                // self.graph.remove_connection(vid, to_vid.clone(), &connection);
            }
            self.integrate_constant_input(to_vid);
        }
    }
}
