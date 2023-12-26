#![allow(dead_code)] // TODO: Remove

use crate::compiler::graph::{
    ArithmeticOperation, Connection, DeciderOperation, Graph, IOType, NId,
};
use fnv::FnvHashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    kind: IOType,
    count: i32,
}

impl Item {
    pub fn new(kind: IOType, count: i32) -> Self {
        Self { kind, count }
    }

    pub fn new_signal<S>(name: S, count: i32) -> Self
    where
        S: ToString,
    {
        Self {
            kind: IOType::Signal(name.to_string()),
            count,
        }
    }
}

pub struct Simulator {
    graph: Graph,
    constant_inputs: FnvHashMap<NId, Vec<Item>>,
    contents: FnvHashMap<NId, Vec<Item>>,
    step: usize,
}

impl Simulator {
    pub fn new(graph: Graph, inputs: Vec<Vec<Item>>) -> Self {
        let input_nodes = graph.get_input_nodes();
        let mut constant_inputs = FnvHashMap::default();
        assert_eq!(inputs.len(), input_nodes.len());
        for i in 0..inputs.len() {
            let vid = input_nodes[i];
            let input = inputs[i].clone();

            constant_inputs
                .entry(vid)
                .and_modify(|v: &mut Vec<Item>| v.extend(input.clone()))
                .or_insert(input);
        }

        Self {
            graph,
            constant_inputs,
            contents: FnvHashMap::default(),
            step: 0,
        }
    }

    fn step(&mut self) {
        let mut new_contents = self.constant_inputs.clone();

        for (from_nid, to_nid, conn) in self.graph.iter_conns() {
            let conn_inputs = self.contents.get(&from_nid).cloned().unwrap_or(vec![]);

            let output = match conn {
                Connection::Arithmetic(c) => {
                    let left = get_count(&conn_inputs, &c.left);
                    let right = get_count(&conn_inputs, &c.right);
                    let result = match c.operation {
                        ArithmeticOperation::Add => left + right,
                        ArithmeticOperation::Subtract => left - right,
                        ArithmeticOperation::Multiply => left * right,
                        ArithmeticOperation::Divide => left / right,
                    };
                    Item::new(c.output, result)
                }
                Connection::Decider(c) => {
                    let left = get_count(&conn_inputs, &c.left);
                    let right = get_count(&conn_inputs, &c.right);
                    let result = match c.operation {
                        DeciderOperation::LargerThan => left > right,
                        DeciderOperation::LargerThanOrEqual => left >= right,
                        DeciderOperation::LessThan => left < right,
                        DeciderOperation::LessThanOrEqual => left <= right,
                        DeciderOperation::Equals => left == right,
                        DeciderOperation::NotEquals => left != right,
                    } as i32;
                    Item::new(c.output, result)
                }
                Connection::Gate(c) => {
                    let left = get_count(&conn_inputs, &c.left);
                    let right = get_count(&conn_inputs, &c.right);
                    let should_pass = match c.operation {
                        DeciderOperation::LargerThan => left > right,
                        DeciderOperation::LargerThanOrEqual => left >= right,
                        DeciderOperation::LessThan => left < right,
                        DeciderOperation::LessThanOrEqual => left <= right,
                        DeciderOperation::Equals => left == right,
                        DeciderOperation::NotEquals => left != right,
                    };

                    let count = match should_pass {
                        true => get_count(&conn_inputs, &c.gate_type),
                        false => 0,
                    };

                    Item::new(c.gate_type, count)
                }
                Connection::Constant(c) => Item::new(c.type_, c.count),
            };

            new_contents
                .entry(to_nid)
                .and_modify(|v: &mut Vec<Item>| v.push(output.clone()))
                .or_insert(vec![output]);
        }

        dbg!(&new_contents);

        self.contents = new_contents;
    }

    fn get_node_contents(&self, nid: &NId) -> Vec<Item> {
        self.contents.get(nid).cloned().unwrap_or(vec![])
    }

    pub fn simulate(&mut self, steps: usize) {
        for _ in 0..steps {
            self.step()
        }
    }

    pub fn get_output(&self) -> Vec<Vec<Item>> {
        let output_nodes = self.graph.get_output_nodes();

        output_nodes
            .iter()
            .map(|nid| self.get_node_contents(nid))
            .collect()
    }
}

fn get_count(items: &Vec<Item>, iotype: &IOType) -> i32 {
    if let IOType::Constant(n) = iotype {
        return *n;
    }

    for item in items {
        if &item.kind == iotype {
            return item.count;
        }
    }
    0
}
