use std::{fs::OpenOptions, io::Write};

use factorio_blueprint as fb;
use fb::objects::{EntityNumber, Position};
use fnv::FnvHashMap;
use noisy_float::types::R64;

use crate::{
    compiler::graph::{self, Connection, Graph},
    utils::{self, VisualizerError},
};

pub(crate) type Coord = i64;
pub(crate) type CoordSet = (Coord, Coord);

#[derive(Clone)]
pub enum NodeType {
    Input,
    Output,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CombinatorPosition {
    pub input: CoordSet,
    pub output: CoordSet,
}

impl CombinatorPosition {
    pub fn factorio_pos(&self) -> Position {
        let (x1, y1) = self.input;
        let (x2, y2) = self.output;

        let x = ((x1 + x2) / 2) as f64;
        let y = ((y1 + y2) / 2) as f64;

        let x = R64::new(x);
        let y = R64::new(y);

        Position { x, y }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Combinator {
    pub from: NId,
    pub to: NId,
    pub operation: Connection,
    pub position: Option<CombinatorPosition>,
    pub entity_number: EntityNumber,
}

impl Combinator {
    fn _new(
        from: NId,
        to: NId,
        operation: Connection,
        position: Option<CombinatorPosition>,
        entity_number: EntityNumber,
    ) -> Self {
        Self {
            from,
            to,
            operation,
            position,
            entity_number,
        }
    }
    fn new_no_pos(from: NId, to: NId, operation: Connection, entity_number: EntityNumber) -> Self {
        Self {
            from,
            to,
            operation,
            position: None,
            entity_number,
        }
    }
}

enum _ConnectionPoint {
    Constant(_ConstantCombinator),
    PowerPole,
}

struct _ConstantCombinator {
    position: CoordSet,
    count: i32,
    signal: String,
}

type NId = u64;

// Id of directly connected nodes will be the same
type Node = (graph::NId, NodeType);

#[derive(Clone)]
pub struct BlueprintGraph {
    pub vertices: FnvHashMap<NId, Node>,
    pub combinators: Vec<Combinator>,
    pub wires: FnvHashMap<NId, Vec<NId>>,
    next_nid: NId,
    next_entity_number: EntityNumber,
}

impl BlueprintGraph {
    fn new() -> Self {
        Self {
            vertices: FnvHashMap::default(),
            combinators: vec![],
            wires: FnvHashMap::default(),
            next_nid: 0,
            next_entity_number: EntityNumber::new(1).unwrap(),
        }
    }

    fn get_next_nid(&mut self) -> NId {
        let nid = self.next_nid;
        self.next_nid = self
            .next_nid
            .checked_add(1)
            .expect("This should not overflow... hopefully.");
        nid
    }

    fn get_next_entity_number(&mut self) -> EntityNumber {
        let en = self.next_entity_number;
        self.next_entity_number = self
            .next_entity_number
            .checked_add(1)
            .expect("This should not overflow... hopefully.");
        en
    }

    pub fn push_node(&mut self, node: Node) -> NId {
        let nid = self.get_next_nid();
        self.vertices.insert(nid, node);
        nid
    }

    pub fn push_combinator(&mut self, com: Combinator) {
        self.combinators.push(com);
    }

    pub fn get_corresponding_combinator(&self, nid: NId) -> &Combinator {
        for com in &self.combinators {
            if com.from == nid || com.to == nid {
                return com;
            }
        }
        panic!("All nids should be connected to combinators.")
    }

    pub fn get_other_nodes_in_wire_network(&self, nid: &NId) -> Vec<NId> {
        let (network_id, _node_type) = self
            .vertices
            .get(nid)
            .expect("The nid should always be valid");
        self.get_nodes_in_wire_network(network_id)
            .iter()
            .copied()
            .filter(|other| other != nid)
            .collect()
    }

    pub fn get_nodes_in_wire_network(&self, network_id: &u64) -> Vec<NId> {
        let mut network_nids = Vec::new();
        for (nid, (node_network_id, _node_type)) in &self.vertices {
            if node_network_id == network_id {
                network_nids.push(*nid);
            }
        }
        network_nids
    }

    pub fn _get_placed_combinators(&self) -> Vec<&Combinator> {
        self.combinators
            .iter()
            .filter(|com| com.position.is_some())
            .collect()
    }

    pub fn _get_combinator(&self, entity_number: EntityNumber) -> &Combinator {
        for com in &self.combinators {
            if com.entity_number == entity_number {
                return com;
            }
        }
        panic!("Could not find combinator.")
    }

    pub fn _get_placed_connected_combinators(
        &self,
        this_entity_number: EntityNumber,
    ) -> Vec<&Combinator> {
        let com = self._get_combinator(this_entity_number);
        let placed = self._get_placed_combinators();
        placed
            .iter()
            .filter(|other| other.entity_number != this_entity_number)
            .filter(|other| {
                other.to == com.to
                    || other.from == com.to
                    || other.to == com.from
                    || other.from == com.from
            })
            .copied()
            .collect()
    }

    pub fn push_wire(&mut self, first: NId, second: NId) {
        {
            let first_adj = self.wires.entry(first).or_default();
            first_adj.push(second);
        }
        {
            let second_adj = self.wires.entry(second).or_default();
            second_adj.push(first);
        }
    }

    pub fn from_graph(graph: Graph) -> Self {
        let mut blueprint_graph = Self::new();
        for (orig_from_nid, to_vec) in graph.adjacency {
            for (orig_to_nid, conn) in to_vec {
                let new_from_nid = blueprint_graph.push_node((orig_from_nid, NodeType::Input));
                let new_to_nid = blueprint_graph.push_node((orig_to_nid, NodeType::Output));
                let entity_number = blueprint_graph.get_next_entity_number();
                blueprint_graph.push_combinator(Combinator::new_no_pos(
                    new_from_nid,
                    new_to_nid,
                    conn.clone(),
                    entity_number,
                ));
            }
        }
        blueprint_graph
    }

    fn create_dot(&self) -> String {
        let mut dot = "strict digraph {\n\tnodesep=1\n".to_string();

        for (nid, (node_netword_id, node_type)) in &self.vertices {
            let color = match node_type {
                NodeType::Input => "lightgreen",
                NodeType::Output => "orange",
            };
            dot += &format!(
                "\t{} [style=filled fillcolor=\"{}\" label=\"{}\" ]\n",
                nid, color, node_netword_id
            );
        }

        for com in &self.combinators {
            let color = match com.position {
                Some(_) => "black",
                None => "red",
            };
            dot += &format!(
                "\t{} -> {} [label=\"{} ({})\\n{:?}\" color={} fontcolor={}]\n",
                com.from, com.to, com.operation, com.entity_number, com.position, color, color
            );
        }

        for (from, to_vec) in &self.wires {
            for to in to_vec {
                dot += &format!("\t{} -> {} [color=blue]\n", from, to);
            }
        }

        dot += "}\n";
        dot
    }

    pub fn visualize(&self, output_path: &str) -> Result<(), VisualizerError> {
        let dot = self.create_dot();
        let svg = utils::dot_to_svg(dot)?;

        OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(output_path)
            .map_err(VisualizerError::IoErr)?
            .write_all(svg.as_bytes())
            .map_err(VisualizerError::IoErr)
    }
}
