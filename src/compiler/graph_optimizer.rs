use super::graph::{Graph, VId, IOType};

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
        let inputs = self.graph.get_inputs(&vid);

        if inputs.len() == 1 {
            if let IOType::Constant(_) = inputs.first().unwrap() { }
            else { return; }
        }
        else if inputs.len() == 0 {
            for (to_vid, conn) in self.graph.adjacency[&vid].clone() {
                if let Connection::Arithmetic(ac) = conn.as_ref() {
                    if let IOType::Constant(left_value) = ac.left {
                        if let IOType::Constant(right_value) = ac.right {
                            let result = match ac.operation {
                                super::graph::ArithmeticOperation::ADD => left_value + right_value,
                                super::graph::ArithmeticOperation::SUBTRACT => left_value - right_value,
                                super::graph::ArithmeticOperation::MULTIPLY => left_value * right_value,
                                super::graph::ArithmeticOperation::DIVIDE => left_value / right_value,
                            };
                            let out_type = IOType::Constant(result);
                            let new_input_vid = self.graph.push_input_node("internal (todo)".to_string(), vec![out_type.clone()]);
                            self.graph.remove_node_with_connections(&vid);
                            let new_conn = self.graph.push_connection(new_input_vid, to_vid, Connection::Arithmetic(ArithmeticConnection::new_pick(out_type)));

                            // TODO: Virker ikke...
                            // Remove reference to old connection 
                            match self.graph.vertices.get_mut(&to_vid) {
                                Some(Node::Inner(n)) => {
                                    for (i, c) in n.inputs.iter().enumerate() {
                                        if c == &conn {
                                            n.inputs.remove(i);
                                            break;
                                        }
                                    }
                                    n.inputs.push(new_conn);
                                },
                                Some(Node::Output(n)) => {
                                    for (i, c) in n.inputs.iter().enumerate() {
                                        if c == &conn {
                                            n.inputs.remove(i);
                                            break;
                                        }
                                    }
                                    n.inputs.push(new_conn);
                                },
                                Some(Node::Input(_n)) => panic!("This should not be possible."),
                                None => {},
                            }

                        }
                        return;
                    }
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
