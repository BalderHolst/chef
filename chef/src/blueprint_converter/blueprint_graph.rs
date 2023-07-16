use std::{fs::OpenOptions, io::Write};

use factorio_blueprint as fb;
use fb::objects::{EntityNumber, OneBasedIndex};
use fnv::FnvHashMap;

use crate::{
    compiler::graph::{self, Connection, Graph},
    utils::{self, VisualizerError},
};

pub enum NodeType {
    Input,
    Output,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Combinator {
    operation: Connection,
    pub position: Option<(f64, f64)>,
}

impl Combinator {
    fn new(operation: Connection, position: Option<(f64, f64)>) -> Self {
        Self {
            operation,
            position,
        }
    }
    fn new_no_pos(operation: Connection) -> Self {
        Self {
            operation,
            position: None,
        }
    }
}

type NId = usize;

// Id of directly connected nodes will be the same
type Node = (graph::NId, NodeType);

pub struct BlueprintGraph {
    pub vertices: FnvHashMap<NId, Node>,
    pub combinators: FnvHashMap<NId, Vec<(NId, EntityNumber, Combinator)>>,
    pub wires: FnvHashMap<NId, Vec<NId>>,
    next_nid: NId,
    next_entity_number: EntityNumber,
}

impl BlueprintGraph {
    fn new() -> Self {
        Self {
            vertices: FnvHashMap::default(),
            combinators: FnvHashMap::default(),
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

    pub fn push_combinator(
        &mut self,
        from: NId,
        to: NId,
        entity_id: EntityNumber,
        conn_type: Combinator,
    ) {
        let adj = self.combinators.entry(from).or_default();
        adj.push((to, entity_id, conn_type));
    }

    pub fn get_nodes_in_wire_network(&self, network_id: &u64) -> Vec<NId> {
        let mut network_nids = Vec::new();
        for (nid, (node_network_id, _node_type)) in &self.vertices {
            if node_network_id == network_id {
                network_nids.push(nid.clone());
            }
        }
        network_nids
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
                blueprint_graph.push_combinator(
                    new_from_nid,
                    new_to_nid,
                    entity_number,
                    Combinator::new_no_pos(conn.clone()),
                );
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

        for (from_nid, to_vec) in &self.combinators {
            for (to_nid, entity_num, com) in to_vec {
                let conn_repr = match &com.operation {
                    Connection::Arithmetic(ac) => ac.operation.to_string(),
                    Connection::Decider(_) => "Decider".to_string(),
                    Connection::Gate(_) => "Gate".to_string(),
                };
                dot += &format!(
                    "\t{} -> {} [label=\"{} ({})\"]\n",
                    from_nid, to_nid, conn_repr, entity_num
                );
            }
        }

        for (from, to_vec) in &self.wires {
            for to in to_vec {
                dot += &format!("\t{} -> {} [color=red]\n", from, to);
            }
        }

        dot += "}\n";
        println!("{dot}");
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
            .map_err(|e| VisualizerError::IoErr(e))?
            .write_all(svg.as_bytes())
            .map_err(|e| VisualizerError::IoErr(e))
    }
}
