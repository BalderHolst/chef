use std::{collections::HashMap, fs::OpenOptions, io::Write, rc::Rc};

use factorio_blueprint as fb;
use fb::objects::{EntityNumber, OneBasedIndex};
use fnv::FnvHashMap;

use crate::{
    compiler::graph::{self, Connection, Graph},
    utils::{self, VisualizerError},
};

type NId = OneBasedIndex;

pub enum NodeType {
    Input,
    Output,
}

// Id of directly connected nodes will be the same
type Node = (graph::NId, NodeType);

pub struct BlueprintGraph {
    pub vertices: FnvHashMap<NId, Node>,
    operations: FnvHashMap<NId, Vec<(NId, EntityNumber, Connection)>>,
    _wires: FnvHashMap<NId, Vec<NId>>,
    next_nid: NId,
    next_entity_number: EntityNumber,
}

impl BlueprintGraph {
    fn new() -> Self {
        Self {
            vertices: FnvHashMap::default(),
            operations: FnvHashMap::default(),
            _wires: FnvHashMap::default(),
            next_nid: EntityNumber::new(1).unwrap(),
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

    fn get_next_entity_number(&mut self) -> NId {
        let en = self.next_entity_number;
        self.next_entity_number = self
            .next_entity_number
            .checked_add(1)
            .expect("This should not overflow... hopefully.");
        en
    }

    fn push_node(&mut self, node: Node) -> NId {
        let nid = self.get_next_nid();
        self.vertices.insert(nid, node);
        nid
    }

    fn push_operation(&mut self, from: NId, to: NId, entity_id: EntityNumber, conn: Connection) {
        let adj = self.operations.entry(from).or_default();
        adj.push((to, entity_id, conn));
    }

    fn push_wire(&mut self, first: NId, second: NId) {
        {
            let first_adj = self._wires.entry(first).or_default();
            first_adj.push(second);
        }
        {
            let second_adj = self._wires.entry(second).or_default();
            second_adj.push(first);
        }
    }

    pub fn from_graph(graph: Graph) -> Self {
        let mut blueprint_graph = Self::new();
        for (orig_from_nid, to_vec) in graph.adjacency {
            for (orig_to_nid, conn) in to_vec {
                let new_from_nid = blueprint_graph.push_node((orig_from_nid, NodeType::Output));
                let new_to_nid = blueprint_graph.push_node((orig_to_nid, NodeType::Input));
                let entity_number = blueprint_graph.get_next_entity_number();
                blueprint_graph.push_operation(
                    new_from_nid,
                    new_to_nid,
                    entity_number,
                    conn.clone(),
                );
            }
        }
        blueprint_graph
    }

    fn create_dot(&self) -> String {
        let mut dot = "strict digraph {\n\tnodesep=1\n".to_string();

        for (nid, node) in &self.vertices {
            dot += &format!("\t{} [style=filled label=\"{}\" ]\n", nid, node.0);
        }

        for (from_nid, to_vec) in &self.operations {
            for (to_nid, entity_num, conn) in to_vec {
                let conn_repr = match conn {
                    Connection::Arithmetic(_) => "Arithmetic",
                    Connection::Decider(_) => "Decider",
                    Connection::Gate(_) => "Gate",
                };
                dot += &format!(
                    "{} -> {} [label=\"{} ({})\"]",
                    from_nid, to_nid, conn_repr, entity_num
                );
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
            .map_err(|e| VisualizerError::IoErr(e))?
            .write_all(svg.as_bytes())
            .map_err(|e| VisualizerError::IoErr(e))
    }
}
