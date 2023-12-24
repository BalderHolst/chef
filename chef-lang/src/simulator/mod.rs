#![allow(dead_code)] // TODO: Remove

use crate::compiler::graph::{Connection, Graph, NId};
use fnv::FnvHashMap;

#[derive(Clone)]
pub struct Item {
    name: String,
    count: usize,
}

impl Item {
    pub fn new<S>(name: S, count: usize) -> Self
    where
        S: ToString,
    {
        Self {
            name: name.to_string(),
            count,
        }
    }
}

pub struct Simulator {
    graph: Graph,
    contents: FnvHashMap<NId, Vec<Item>>,
    step: usize,
}

impl Simulator {
    pub fn new(graph: Graph, inputs: Vec<Vec<Item>>) -> Self {
        let mut s = Self {
            graph,
            contents: FnvHashMap::default(),
            step: 0,
        };
        let input_nodes = s.graph.get_input_nodes();
        assert_eq!(inputs.len(), input_nodes.len());
        for i in 0..inputs.len() {
            let vid = input_nodes[i];
            let input = inputs[i].clone();
            s.contents
                .entry(vid)
                .and_modify(|v| v.extend(input.clone()))
                .or_insert(input);
        }
        s
    }

    fn step(&mut self) {
        let mut new_contents = FnvHashMap::default();

        for (from_nid, to_nid, conn) in self.graph.iter_conns() {
            let conn_inputs = self.contents.get(&from_nid).unwrap_or(&vec![]);

            match conn {
                Connection::Arithmetic(c) => {
                    todo!()
                }
                Connection::Decider(c) => todo!(),
                Connection::Gate(c) => todo!(),
                Connection::Constant(c) => todo!(),
            }
        }

        self.contents = new_contents;

        todo!()
    }

    pub fn simulate(&mut self, steps: usize) {
        for _ in 0..steps {
            self.step()
        }
    }

    pub fn get_output(&self) -> Vec<Vec<Item>> {
        todo!()
    }
}
