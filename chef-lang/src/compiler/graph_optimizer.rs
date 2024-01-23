use super::graph::{Connection, Graph, NId};

pub struct GraphOptimizer<'a> {
    graph: &'a mut Graph,
}

impl<'a> GraphOptimizer<'a> {
    pub fn new(graph: &'a mut Graph) -> Self {
        Self { graph }
    }

    pub fn optimize(&mut self) {
        self.remove_redundant_picks();
    }

    fn remove_redundant_picks(&mut self) {
        while let Some((from_vid, middle_vid, to_vid, index)) = self.get_redundant_pick() {
            if let Some(to_vec) = self.graph.adjacency.get_mut(&from_vid) {
                to_vec[index].0 = to_vid;
                self.graph.remove_node_with_connections(&middle_vid);
            }
        }
    }

    fn get_redundant_pick(&mut self) -> Option<(NId, NId, NId, usize)> {
        for (from_vid, middle_vec) in self.graph.adjacency.iter() {
            for (i, (middle_vid, _conn1)) in middle_vec.clone().iter().enumerate() {
                // Perserve `var` variables
                if from_vid == middle_vid {
                    continue;
                }

                if let Some(to_vec) = self.graph.adjacency.get(middle_vid) {
                    if to_vec.len() != 1 {
                        continue;
                    }
                    let (to_vid, conn2) = to_vec.first().unwrap();
                    if let Connection::Combinator(com2) = conn2 {
                        if !com2.is_pick() {
                            continue;
                        }
                    }
                    return Some((*from_vid, *middle_vid, *to_vid, i));
                }
            }
        }
        None
    }
}
