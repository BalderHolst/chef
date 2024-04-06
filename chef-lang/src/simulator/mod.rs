#![allow(dead_code)] // TODO: Remove

mod visualizer;

use std::{fmt::Display, io};

use crate::compiler::graph::{
    self, ArithmeticOperation, DeciderOperation, DetSig, Graph, NId, NetworkId, Operation, Signal,
};
use fnv::FnvHashMap;

use self::visualizer::visualize_simulator;

/// Create inputs for a graph ergonomically
#[macro_export]
macro_rules! inputs {
    {$([$($name:literal:$count:expr),+];)+} => {
        vec![
            $($crate::items![$( $name : $count ),+]),+
        ]
    };
    [] => {vec![]};
    [$($all:tt)*] => {
        vec![$crate::items![$($all)*]]
    };
}

/// The exact same as `inputs!` but i like it better for outputs.
#[macro_export]
macro_rules! outputs {
    {$($x:tt)*} => {
        $crate::inputs!{$($x)*}
    }
}

/// Create node items contents ergonomically
#[macro_export]
macro_rules! items {
    [$($name:literal:$count:expr),+] => {
        vec![
            $($crate::simulator::Item::new(
                    $crate::compiler::graph::DetSig::signal($name), $count
                    )),+
        ]
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    kind: DetSig,
    count: i32,
}

impl Item {
    pub fn new(kind: DetSig, count: i32) -> Self {
        Self { kind, count }
    }

    pub fn new_signal<S>(name: S, count: i32) -> Self
    where
        S: ToString,
    {
        Self {
            kind: DetSig::Signal(name.to_string()),
            count,
        }
    }
}

fn combine_items(input: Vec<Item>) -> Vec<Item> {
    let mut evaluated = vec![false; input.len()];
    let mut outputs: Vec<Item> = vec![];

    for (i, item) in input.iter().enumerate() {
        if evaluated[i] {
            continue;
        }
        let mut count = item.count;
        evaluated[i] = true;
        for (j, other) in input.iter().enumerate() {
            if evaluated[j] {
                continue;
            }
            if item.kind == other.kind {
                count += other.count;
                evaluated[j] = true;
            }
        }
        outputs.push(Item::new(item.kind.clone(), count));
    }

    outputs
}

#[test]
fn test_combine_items() {
    let input = vec![
        Item::new(DetSig::signal("test"), 1),
        Item::new(DetSig::signal("test"), 1),
        Item::new(DetSig::signal("test"), 1),
        Item::new(DetSig::signal("other"), 1),
        Item::new(DetSig::signal("test"), 1),
        Item::new(DetSig::signal("test"), 1),
    ];
    let expected = vec![
        Item::new(DetSig::signal("test"), 5),
        Item::new(DetSig::signal("other"), 1),
    ];
    assert_eq!(combine_items(input), expected);
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "<{}:{}>", self.kind, self.count)
    }
}

pub struct Simulator {
    graph: Graph<DetSig>,
    constant_inputs: FnvHashMap<NetworkId, Vec<Item>>,
    step: usize,
    networks: FnvHashMap<NId, Vec<NetworkId>>,
    network_contents: FnvHashMap<NetworkId, Vec<Item>>,
}

impl Simulator {
    pub fn new(graph: Graph<DetSig>, mut inputs: Vec<Vec<Item>>) -> Self {
        let inputs_nodes = graph.get_input_nodes();
        let input_nodes: Vec<_> = inputs_nodes.iter().map(|(_name, nid, _sig)| nid).collect();

        let mut constant_inputs = FnvHashMap::default();

        // fill inputs with nothing, if nothing was specified
        while inputs.len() < input_nodes.len() {
            inputs.push(vec![])
        }

        assert_eq!(
            inputs.len(),
            input_nodes.len(),
            "Incorrect number of inputs were provided."
        );

        let mut nid_to_networks_id: FnvHashMap<_, Vec<_>> = FnvHashMap::default();

        for (network_id, (network, _wk)) in graph.get_networks().iter().enumerate() {
            for nid in network {
                nid_to_networks_id
                    .entry(*nid)
                    .and_modify(|v| v.push(network_id))
                    .or_insert(vec![network_id]);
            }
        }

        for i in 0..inputs.len() {
            let nid = input_nodes[i];
            let input = inputs[i].clone();
            let network_ids = nid_to_networks_id.get(nid).unwrap();

            for network_id in network_ids {
                constant_inputs
                    .entry(*network_id)
                    .and_modify(|v: &mut Vec<Item>| v.extend(input.clone()))
                    .or_insert(input.clone());
            }
        }

        for (nid, node) in &graph.vertices {
            if let graph::Node::Constant(c) = node {
                if let Some((t, n)) = c.get_constant_signal() {
                    let network_ids = nid_to_networks_id.get(nid).unwrap();
                    let item = Item::new(t, n);
                    for network_id in network_ids {
                        constant_inputs
                            .entry(*network_id)
                            .and_modify(|v: &mut Vec<Item>| v.push(item.clone()))
                            .or_insert(vec![item.clone()]);
                    }
                }
            }
        }

        Self {
            graph,
            constant_inputs,
            networks: nid_to_networks_id,
            network_contents: FnvHashMap::default(),
            step: 0,
        }
    }

    pub fn graph(&self) -> &Graph<DetSig> {
        &self.graph
    }

    fn step(&mut self) {
        let mut new_contents = self.constant_inputs.clone();

        for (from_nid, to_nid, conn) in self.graph.iter_combinators() {
            let from_network_ids = self
                .networks
                .get(&from_nid)
                .expect("Constructor should have created a network for each node.");

            let to_network_ids = self
                .networks
                .get(&to_nid)
                .expect("Constructor should have created a network for each node.");

            let conn_inputs: Vec<_> = from_network_ids
                .iter()
                .flat_map(|from_network_id| {
                    self.network_contents
                        .get(from_network_id)
                        .cloned()
                        .unwrap_or(vec![])
                })
                .collect();

            let conn_inputs = combine_items(conn_inputs);

            let mut output = match conn {
                Operation::Arithmetic(c) => {
                    let left = get_count(&conn_inputs, &c.left);
                    let right = get_count(&conn_inputs, &c.right);
                    let result = match c.operation {
                        ArithmeticOperation::Add => left + right,
                        ArithmeticOperation::Subtract => left - right,
                        ArithmeticOperation::Multiply => left * right,
                        ArithmeticOperation::Divide if right == 0 => 0,
                        ArithmeticOperation::Divide => left / right,
                    };
                    Item::new(c.output, result)
                }
                Operation::Decider(c) => {
                    let left = get_count(&conn_inputs, &c.left);
                    let right = get_count(&conn_inputs, &c.right);
                    let result = match c.operation {
                        DeciderOperation::LargerThan => left > right,
                        DeciderOperation::LargerThanOrEqual => left >= right,
                        DeciderOperation::LessThan => left < right,
                        DeciderOperation::LessThanOrEqual => left <= right,
                        DeciderOperation::Equals => left == right,
                        DeciderOperation::NotEquals => left != right,
                        DeciderOperation::EveryEquals => todo!(),
                        DeciderOperation::EveryLargerThan => todo!(),
                        DeciderOperation::EveryLargerThanEquals => todo!(),
                        DeciderOperation::EveryLessThan => todo!(),
                        DeciderOperation::EveryLessThanEquals => todo!(),
                        DeciderOperation::EveryNotEquals => todo!(),
                        DeciderOperation::AnyEquals => todo!(),
                        DeciderOperation::AnyLargerThan => todo!(),
                        DeciderOperation::AnyLargerThanEquals => todo!(),
                        DeciderOperation::AnyLessThan => todo!(),
                        DeciderOperation::AnyLessThanEquals => todo!(),
                        DeciderOperation::AnyNotEquals => todo!(),
                    } as i32;
                    Item::new(c.output, result)
                }
                Operation::Pick(p) => {
                    let sig = get_count(&conn_inputs, &p.pick);
                    Item::new(p.pick, sig)
                }
                Operation::Convert(c) => {
                    let sig = get_count(&conn_inputs, &c.input);
                    Item::new(c.output, sig)
                }
                Operation::Gate(c) => {
                    let left = get_count(&conn_inputs, &c.left);
                    let right = get_count(&conn_inputs, &c.right);
                    let should_pass = match c.operation {
                        DeciderOperation::LargerThan => left > right,
                        DeciderOperation::LargerThanOrEqual => left >= right,
                        DeciderOperation::LessThan => left < right,
                        DeciderOperation::LessThanOrEqual => left <= right,
                        DeciderOperation::Equals => left == right,
                        DeciderOperation::NotEquals => left != right,
                        DeciderOperation::EveryEquals => todo!(),
                        DeciderOperation::EveryLargerThan => todo!(),
                        DeciderOperation::EveryLargerThanEquals => todo!(),
                        DeciderOperation::EveryLessThan => todo!(),
                        DeciderOperation::EveryLessThanEquals => todo!(),
                        DeciderOperation::EveryNotEquals => todo!(),
                        DeciderOperation::AnyEquals => todo!(),
                        DeciderOperation::AnyLargerThan => todo!(),
                        DeciderOperation::AnyLargerThanEquals => todo!(),
                        DeciderOperation::AnyLessThan => todo!(),
                        DeciderOperation::AnyLessThanEquals => todo!(),
                        DeciderOperation::AnyNotEquals => todo!(),
                    };

                    let count = match should_pass {
                        true => get_count(&conn_inputs, &c.gate_type),
                        false => 0,
                    };
                    Item::new(c.gate_type, count)
                }

                Operation::Delay(_dc) => todo!("Simulate delay"),
                Operation::Sum(_sc) => todo!("Simulate sum"),
            };

            if self.step == 0 {
                output.count = 0;
            }

            for to_network_id in to_network_ids {
                new_contents
                    .entry(*to_network_id)
                    .and_modify(|v: &mut Vec<Item>| {
                        // Check if the item exists in node
                        for item in v.iter_mut() {
                            if output.kind == item.kind {
                                item.count += output.count;
                                return;
                            }
                        }
                        v.push(output.clone())
                    })
                    .or_insert(vec![output.clone()]);
            }
        }

        self.network_contents = new_contents;

        self.step += 1;
    }

    fn get_node_contents(&self, nid: &NId) -> Vec<Item> {
        let network_ids = self.networks.get(nid).unwrap();
        network_ids
            .iter()
            .flat_map(|network_id| {
                self.network_contents
                    .get(network_id)
                    .cloned()
                    .unwrap_or(vec![])
            })
            .collect()
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
            .map(|(_name, nid)| self.get_node_contents(nid))
            .collect()
    }

    fn dump_state(&self, out_file: &str) -> io::Result<()> {
        visualize_simulator(self, out_file)?;
        io::Result::Ok(())
    }

    pub fn dump_simulation(&mut self, steps: usize, our_dir: &str) {
        for step in 0..steps {
            let file = our_dir.to_string() + "/" + step.to_string().as_str() + ".svg";
            self.dump_state(file.as_str()).unwrap();
            self.step();
        }
    }
}

fn get_count(items: &Vec<Item>, iotype: &DetSig) -> i32 {
    if let DetSig::Constant(n) = iotype {
        return *n;
    }

    for item in items {
        if &item.kind == iotype {
            return item.count;
        }
    }
    0
}
