use super::graph::{Graph, VId, Node, IOType, NodeInputs, Connection, ArithmeticConnection};

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

        dbg!(self.graph.get_vertex(&10));
    }

    fn remove_redundant_picks(&mut self) {
        for (from_vid, middle_vec) in self.graph.adjacency.clone().iter() {
            if middle_vec.len() != 1 { continue; }
            let (middle_vid, conn1) = middle_vec.first().unwrap();
            if !conn1.is_pick() { continue; }

            let to_vec = self.graph.adjacency.get(middle_vid).unwrap().clone();
            if to_vec.len() != 1 { continue; }
            let (to_vid, conn2) = to_vec.first().unwrap();
            if !conn2.is_pick() { continue; }

            if let Connection::Arithmetic(c) = conn2.as_ref().clone() {
                self.graph.remove_node(middle_vid);
                self.graph.push_connection(*from_vid, *to_vid, Connection::Arithmetic(ArithmeticConnection::new_pick(c.output)))
            }

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
