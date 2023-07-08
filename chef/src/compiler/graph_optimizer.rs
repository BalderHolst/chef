use super::graph::{Graph, NId, IOType, Connection, ArithmeticConnection, ArithmeticOperation};

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
                self.graph.remove_node_with_connections(&middle_vid);
            }
        }
    }

    fn get_redundant_pick(&mut self) -> Option<(NId, NId, NId, usize)> {
        for (from_vid, middle_vec) in self.graph.adjacency.iter() {
            for (i, (middle_vid, _conn1)) in middle_vec.clone().iter().enumerate() {
                if let Some(to_vec) = self.graph.adjacency.get(middle_vid) {
                    if to_vec.len() != 1 { continue; }
                    let (to_vid, conn2) = to_vec.first().unwrap();
                    if !conn2.is_pick() { continue; }
                    return Some((from_vid.clone(), middle_vid.clone(), to_vid.clone(), i))
                }
            }
        }
        None
    }

    fn integrate_constant_input(&mut self, vid: NId) {
        // TODO: This shit is ugly...
        let inputs = self.graph.get_inputs(&vid);

        if inputs.len() == 1 {
            if let IOType::Constant(_) = inputs.first().unwrap() { }
            else { return; }
        }
        else if inputs.len() == 0 { // TODO: Remove this, as it is already taken care of in the AST
                                    // evaluation.
            for (to_vid, conn) in self.graph.adjacency[&vid].clone() {
                match conn {
                    Connection::Arithmetic(ac) => {
                        if let IOType::Constant(left_value) = ac.left {
                            if let IOType::Constant(right_value) = ac.right {
                                let result = match ac.operation {
                                    // TODO: Left and right placements are random, and left and right
                                    // values can therefore not be trusted.
                                    ArithmeticOperation::ADD => left_value + right_value,
                                    ArithmeticOperation::SUBTRACT => left_value - right_value, 
                                    ArithmeticOperation::MULTIPLY => left_value * right_value,
                                    ArithmeticOperation::DIVIDE => left_value / right_value,
                                };
                                let old_out_type = ac.output;
                                let new_out_type = IOType::Constant(result);
                                for (after_vid, _) in self.graph.adjacency.get(&to_vid).unwrap().clone() {
                                    for (_, out_conn) in self.graph.adjacency.get_mut(&after_vid).unwrap() {
                                        match out_conn {
                                            Connection::Arithmetic(out_ac) => {
                                                if out_ac.right == old_out_type {
                                                    out_ac.right = new_out_type.clone()
                                                }
                                                else {
                                                    out_ac.left = out_ac.right.clone();
                                                    out_ac.right = new_out_type.clone();
                                                }
                                            },
                                        }
                                    }
                                }
                                let new_input_vid = self.graph.push_input_node("internal to be removed".to_string(), new_out_type.clone());
                                self.graph.remove_node_with_connections(&vid);
                                self.graph.push_connection(new_input_vid, to_vid, Connection::Arithmetic(ArithmeticConnection::new_pick(new_out_type)));
                                self.integrate_constant_input(new_input_vid);
                            }
                            return;
                        }
                    }
                    _ => (),
                }
            }
        }
        else { return; };

        for (to_vid, connection) in self.graph.adjacency.get(&vid).unwrap().clone() {
            if connection.is_pick() {
                self.graph.remove_node_with_connections(&vid);
            }
            self.integrate_constant_input(to_vid);
        }
    }
}
