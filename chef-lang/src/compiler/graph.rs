//! Implementation of [Graph]. A graph for representing a network of factorio combinators.

#![allow(dead_code)] // TODO: remove

use fnv::FnvHashMap;
use std::{collections::HashSet, fmt::Display, usize};

use crate::utils::BASE_SIGNALS;

use super::graph_visualizer;

#[derive(Debug, Clone, PartialEq)]
pub struct Register {
    pub shift: NId,
    pub input: NId,
    pub output: Vec<NId>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DeciderOperation {
    LargerThan,
    LargerThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equals,
    NotEquals,
}

impl Display for DeciderOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            DeciderOperation::LargerThan => "LARGER_THAN",
            DeciderOperation::LargerThanOrEqual => "LARGER_THAN_OR_EQUAL",
            DeciderOperation::LessThan => "LESS_THAN",
            DeciderOperation::LessThanOrEqual => "LESS_THAN_OR_EQUAL",
            DeciderOperation::Equals => "EQUALS",
            DeciderOperation::NotEquals => "NOT_EQUALS",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeciderCombinator {
    pub left: IOType,
    pub right: IOType,
    pub operation: DeciderOperation,
    pub output: IOType,
}

impl DeciderCombinator {
    pub fn new(left: IOType, right: IOType, operation: DeciderOperation, output: IOType) -> Self {
        Self {
            left,
            right,
            operation,
            output,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Display for ArithmeticOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ArithmeticOperation::Add => "ADD",
            ArithmeticOperation::Subtract => "SUBTRACT",
            ArithmeticOperation::Multiply => "MULTIPLY",
            ArithmeticOperation::Divide => "DIVIDE",
        };
        write!(f, "{s}")
    }
}

/// Type of inputs/outputs of combinators
#[derive(Clone, Debug, PartialEq)]
pub enum IOType {
    Signal(String),
    AnySignal(u64),
    Constant(i32),
    ConstantSignal((String, i32)),
    ConstantAny((u64, i32)),
    Everything,
    Anything,
    Each,
}

impl IOType {
    pub fn signal<S>(signal: S) -> Self
    where
        S: ToString,
    {
        Self::Signal(signal.to_string())
    }
}

impl Display for IOType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IOType::Signal(s) => format!("Sig({})", s),
            IOType::AnySignal(n) => format!("Any({})", n),
            IOType::Constant(n) => format!("({})", n),
            IOType::ConstantSignal((sig, n)) => format!("Const({}, {})", sig, n),
            IOType::ConstantAny((sig, n)) => format!("ConstAny({}, {})", sig, n),
            IOType::Everything => "EVERYTHING".to_string(),
            IOType::Anything => "ANYTHING".to_string(),
            IOType::Each => "EACH".to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArithmeticCombinator {
    pub left: IOType,
    pub right: IOType,
    pub operation: ArithmeticOperation,
    pub output: IOType,
}

impl ArithmeticCombinator {
    pub fn new(
        left: IOType,
        right: IOType,
        operation: ArithmeticOperation,
        output: IOType,
    ) -> Self {
        Self {
            left,
            right,
            operation,
            output,
        }
    }

    pub fn new_pick(signal: IOType) -> Self {
        Self::new(
            signal.clone(),
            IOType::Constant(0),
            ArithmeticOperation::Add,
            signal,
        )
    }

    pub fn new_convert(in_signal: IOType, out_signal: IOType) -> Self {
        Self::new(
            in_signal,
            IOType::Constant(0),
            ArithmeticOperation::Add,
            out_signal,
        )
    }

    fn is_pick(&self) -> bool {
        self.right == IOType::Constant(0)
            && self.operation == ArithmeticOperation::Add
            && self.output == self.left
    }

    fn is_convert(&self) -> bool {
        self.right == IOType::Constant(0)
            && self.operation == ArithmeticOperation::Add
            && self.output != self.left
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GateCombinator {
    pub left: IOType,
    pub right: IOType,
    pub operation: DeciderOperation,
    pub gate_type: IOType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantCombinator {
    pub type_: IOType,
    pub count: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub enum WireKind {
    Green,
    Red,
}

impl Display for WireKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            WireKind::Green => "GREEN WIRE".to_string(),
            WireKind::Red => "RED WIRE".to_string(),
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Combinator {
    Arithmetic(ArithmeticCombinator),
    Decider(DeciderCombinator),
    Gate(GateCombinator),
    Constant(ConstantCombinator),
}

impl Combinator {
    fn new_pick(signal: IOType) -> Self {
        Self::Arithmetic(ArithmeticCombinator::new_pick(signal))
    }

    fn new_convert(in_signal: IOType, out_signal: IOType) -> Self {
        Self::Arithmetic(ArithmeticCombinator::new_convert(in_signal, out_signal))
    }

    pub fn get_output_iotype(&self) -> IOType {
        match self {
            Self::Arithmetic(ac) => ac.output.clone(),
            Self::Decider(dc) => dc.output.clone(),
            Self::Gate(gc) => gc.gate_type.clone(),
            Self::Constant(cc) => cc.type_.clone(),
        }
    }

    #[allow(irrefutable_let_patterns)]
    pub fn is_pick(&self) -> bool {
        if let Self::Arithmetic(ac) = self {
            return ac.is_pick();
        }
        false
    }

    #[allow(irrefutable_let_patterns)]
    pub fn is_convert(&self) -> bool {
        if let Self::Arithmetic(ac) = self {
            return ac.is_convert();
        }
        false
    }
}

impl Display for Combinator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Combinator::Arithmetic(ac) if ac.is_pick() => {
                format!("PICK: {}", ac.output)
            }
            Combinator::Arithmetic(ac) if ac.is_convert() => {
                format!("CONVERT: {} -> {}", ac.left, ac.output)
            }
            Combinator::Arithmetic(ac) => {
                format!("{}: {}, {}", ac.operation, ac.left, ac.right)
            }
            Combinator::Decider(dc) => {
                format!("{}: {}, {}", dc.operation, dc.left, dc.right)
            }
            Combinator::Gate(gate) => format!(
                "GATE: {}\n{}: {}, {}",
                gate.gate_type, gate.operation, gate.left, gate.right
            ),
            // TODO: This should probably be a node instead of a connection
            Combinator::Constant(cc) => {
                format!("CONSTANT : {} = {}", cc.type_, cc.count)
            }
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Connection {
    Wire(WireKind),
    Combinator(Combinator),
}

impl Connection {
    fn new_combinator(com: Combinator) -> Self {
        Self::Combinator(com)
    }

    pub fn new_arithmetic(ac: ArithmeticCombinator) -> Self {
        Self::new_combinator(Combinator::Arithmetic(ac))
    }

    pub fn new_decider(dc: DeciderCombinator) -> Self {
        Self::new_combinator(Combinator::Decider(dc))
    }

    pub fn new_gate(gate: GateCombinator) -> Self {
        Self::new_combinator(Combinator::Gate(gate))
    }

    pub fn new_pick(signal: IOType) -> Self {
        Self::new_combinator(Combinator::new_pick(signal))
    }

    pub fn new_convert(input_signal: IOType, output_signal: IOType) -> Self {
        Self::new_combinator(Combinator::new_convert(input_signal, output_signal))
    }
}

impl Display for Connection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Combinator(com) => com.to_string(),
            Self::Wire(wire) => wire.to_string(),
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Inner,

    /// Variable node that is an input to the graph
    InputVariable(IOType),

    /// Variable node that is integrated in the graph
    Variable(IOType),

    /// An output of the entire graph
    // TODO: This can probably be removed, as output nodes and their output types can be derived
    // from the graph structure itself.
    Output(IOType),

    // TODO: Maybe this should just contain the constant and not a generic iotype?
    Constant(IOType),
}

/// Index of a node in a [Graph].
pub type NId = u64;

/// Identifier for a network of nodes directly connected by wires.
pub type NetworkId = usize;

// TODO: Make `print` the debug or display function
/// A graph for storing connection and nodes representing factorio combinators.
#[derive(Clone)]
pub struct Graph {
    pub vertices: FnvHashMap<NId, Node>,
    pub adjacency: FnvHashMap<NId, Vec<(NId, Connection)>>,
    pub next_nid: NId,
}

impl Graph {
    /// Instantiate a new [Graph].
    pub fn new() -> Graph {
        Graph {
            vertices: FnvHashMap::default(),
            adjacency: FnvHashMap::default(),
            next_nid: 0,
        }
    }

    // TODO: Check that this is correct
    /// Recursive function that walks nodes connected to the input nid. The `network` vector
    /// contains all the that have been found in the network. This is used to handle circular
    /// connections.
    fn get_wire_connected_nodes(
        &self,
        nid: &NId,
        mut network: Vec<NId>,
        wire_kind: WireKind,
    ) -> Vec<NId> {
        network.push(*nid);
        if let Some(conns) = self.adjacency.get(nid) {
            for (to_nid, conn) in conns {
                if let Connection::Wire(wk) = conn {
                    if wk == &wire_kind {
                        // Avoid infinite loop
                        if network.contains(to_nid) {
                            continue;
                        }
                        network = self.get_wire_connected_nodes(to_nid, network, wire_kind.clone());
                    }
                }
            }
        }
        network
    }

    /// Recursively get all nodes connected to a node via wires.
    pub fn get_node_network(&self, nid: &NId, wire_kind: WireKind) -> Vec<NId> {
        self.get_wire_connected_nodes(nid, vec![], wire_kind)
    }

    pub fn get_networks(&self) -> Vec<(Vec<NId>, WireKind)> {
        let mut inserted: HashSet<NId> = HashSet::new();
        let mut networks: Vec<(Vec<NId>, WireKind)> = vec![];

        // Green wires
        for nid in self.vertices.keys() {
            let has_new_network = inserted.insert(*nid);
            if !has_new_network {
                continue;
            }
            let green_network = self.get_node_network(nid, WireKind::Green);
            for network_nid in &green_network {
                inserted.insert(*network_nid);
            }
            if !green_network.is_empty() {
                networks.push((green_network, WireKind::Green));
            }
        }

        // Red wires
        for nid in self.vertices.keys() {
            let has_new_network = inserted.insert(*nid);
            if !has_new_network {
                continue;
            }
            let red_network = self.get_node_network(nid, WireKind::Red);
            for network_nid in &red_network {
                inserted.insert(*network_nid);
            }
            if !red_network.is_empty() {
                networks.push((red_network, WireKind::Red));
            }
        }

        networks
    }

    pub fn get_output_iotype(&self, nid: &NId) -> IOType {
        let green_network_nids = self.get_node_network(nid, WireKind::Green);
        let red_network_nids = self.get_node_network(nid, WireKind::Red);

        let mut types = vec![];

        for to_vec in self.adjacency.values() {
            for (to_nid, conn) in to_vec {
                if green_network_nids.contains(to_nid) || red_network_nids.contains(to_nid) {
                    if let Connection::Combinator(com) = conn {
                        types.push(com.get_output_iotype())
                    }
                }
            }
        }

        if types.len() == 1 {
            types[0].clone()
        } else {
            IOType::Everything
        }
    }

    pub fn get_input_iotypes(&self, nid: &NId) -> Vec<(IOType, WireConnection)> {
        let green_network_nids = self.get_node_network(nid, WireKind::Green);
        let red_network_nids = self.get_node_network(nid, WireKind::Red);

        let mut input_types = vec![];

        // Check for CONSTANT nodes in network
        for other_nid in &green_network_nids {
            if let Some(Node::Constant(t)) = self.get_node(other_nid) {
                input_types.push((t.clone(), WireConnection::Green))
            }
        }
        for other_nid in &red_network_nids {
            if let Some(Node::Constant(t)) = self.get_node(other_nid) {
                input_types.push((t.clone(), WireConnection::Red))
            }
        }

        // Check for INPUT nodes in network
        for other_id in &green_network_nids {
            if let Some(Node::InputVariable(t)) = self.get_node(other_id) {
                input_types.push((t.clone(), WireConnection::Green))
            }
        }
        for other_id in &red_network_nids {
            if let Some(Node::InputVariable(t)) = self.get_node(other_id) {
                input_types.push((t.clone(), WireConnection::Red))
            }
        }

        for to_vec in self.adjacency.values() {
            for (to_nid, conn) in to_vec {
                if green_network_nids.contains(to_nid) {
                    if let Connection::Combinator(com) = conn {
                        let input_type = com.get_output_iotype();
                        let input = (input_type, WireConnection::Green);
                        if !input_types.contains(&input) {
                            input_types.push(input)
                        }
                    }
                }
                if red_network_nids.contains(to_nid) {
                    if let Connection::Combinator(com) = conn {
                        let input_type = com.get_output_iotype();
                        let input = (input_type, WireConnection::Red);
                        if !input_types.contains(&input) {
                            input_types.push(input)
                        }
                    }
                }
            }
        }

        input_types
    }

    /// Returns an iterator overr graph connections with the format:
    /// (from_nid, to_nid, connection).
    pub fn iter_conns(&self) -> impl Iterator<Item = (NId, NId, Connection)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec
                .iter()
                .map(|(to_nid, conn)| (from_nid.to_owned(), to_nid.to_owned(), conn.clone()))
        })
    }

    pub fn iter_combinators(&self) -> impl Iterator<Item = (NId, NId, Combinator)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec.iter().filter_map(|(to_nid, conn)| match &conn {
                Connection::Combinator(com) => Some((*from_nid, *to_nid, com.clone())),
                Connection::Wire(_) => None,
            })
        })
    }

    pub fn iter_wires(&self) -> impl Iterator<Item = (NId, NId, &WireKind)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec.iter().filter_map(|(to_nid, conn)| match &conn {
                Connection::Wire(wk) => Some((*from_nid, *to_nid, wk)),
                Connection::Combinator(_) => None,
            })
        })
    }

    /// Get connections pointing away from the node
    pub fn _get_from_connections(&self, nid: &NId) -> Vec<(NId, Connection)> {
        self.adjacency.get(nid).unwrap_or(&vec![]).clone()
    }

    /// Returns 'to' (incomming), 'from' (outgoing) and 'loop'
    /// connections and the nodes they are connected to (to, from, loop).
    /// 'loop' connections are connections where both from and to nodes are this node.
    #[allow(clippy::type_complexity)]
    pub fn _get_connections(
        &self,
        nid: &NId,
    ) -> (
        Vec<(NId, Connection)>,
        Vec<(NId, Connection)>,
        Vec<Connection>,
    ) {
        let mut from_conns = vec![];
        let mut to_conns = vec![];
        let mut loop_conns = vec![];

        for (from_nid, to_nid, conn) in self.iter_conns() {
            if from_nid == *nid && to_nid == *nid {
                loop_conns.push(conn)
            } else if from_nid == *nid {
                from_conns.push((to_nid, conn))
            } else if to_nid == *nid {
                to_conns.push((from_nid, conn))
            }
        }

        (to_conns, from_conns, loop_conns)
    }

    /// Get a graph node by id.
    pub fn get_node(&self, nid: &NId) -> Option<&Node> {
        self.vertices.get(nid)
    }

    /// Get a mutable graph node by id.
    pub fn _get_mut_node(&mut self, nid: &NId) -> Option<&mut Node> {
        self.vertices.get_mut(nid)
    }

    /// Add a node to the graph. Returns the new node's given id.
    pub fn push_node(&mut self, node: Node) -> NId {
        let nid = self.next_nid;
        if self.vertices.insert(nid, node).is_some() {
            panic!("Could not insert node into graph")
        }
        self.next_nid += 1;
        nid
    }

    /// Override a node at a given id.
    pub fn override_node(&mut self, nid: NId, node: Node) -> Option<Node> {
        self.vertices.insert(nid, node)
    }

    /// Push a node of type [InputNode].
    pub fn push_input_node(&mut self, input: IOType) -> NId {
        self.push_node(Node::InputVariable(input))
    }

    /// Push a node of type [InnerNode].
    pub fn push_inner_node(&mut self) -> NId {
        self.push_node(Node::Inner)
    }

    pub fn push_output_node(&mut self, output_type: IOType) -> NId {
        self.push_node(Node::Output(output_type))
    }

    pub fn push_var_node(&mut self, variable_type: IOType) -> NId {
        self.push_node(Node::Variable(variable_type))
    }

    /// Push a connection between two nodes.
    fn push_raw_connection(&mut self, from: NId, to: NId, connection: Connection) {
        self.adjacency
            .entry(from)
            .or_default()
            .push((to, connection));
    }

    /// Push a connection between two nodes.
    pub fn push_connection(&mut self, connection: Connection) -> (NId, NId) {
        let from = self.push_inner_node();
        let to = self.push_inner_node();
        let adjacent_to_from = self.adjacency.entry(from).or_default();
        adjacent_to_from.push((to, connection));
        (from, to)
    }

    pub fn push_wire(&mut self, n1: NId, n2: NId, wire_kind: WireKind) {
        let wire = Connection::Wire(wire_kind);
        self.adjacency
            .entry(n1)
            .or_default()
            .push((n2, wire.clone()));
        self.adjacency.entry(n2).or_default().push((n1, wire));
    }

    pub fn push_combinator(&mut self, com: Combinator) -> (NId, NId) {
        self.push_connection(Connection::Combinator(com))
    }

    pub fn push_gate_connection(&mut self, cond_type: IOType, gate_type: IOType) -> (NId, NId) {
        self.push_connection(Connection::new_gate(GateCombinator {
            left: cond_type,
            right: IOType::Constant(0),
            operation: DeciderOperation::LargerThan,
            gate_type,
        }))
    }

    /// Remove a connection between two nodes.
    pub fn _remove_connection(&mut self, from: &NId, to: &NId) {
        let from_vertex_connections = self.adjacency.get_mut(from).unwrap();
        for (i, (to_nid, _from_conn)) in from_vertex_connections.iter().enumerate() {
            if to_nid == to {
                from_vertex_connections.remove(i);
                break;
            }
        }
    }

    /// Remove a node and all of its connections.
    pub fn remove_node_with_connections(&mut self, nid: &NId) {
        self.adjacency.remove(nid);
        self.vertices.remove(nid);
        for (_, to_vec) in self.adjacency.iter_mut() {
            for (i, (to_nid, _)) in to_vec.clone().iter().enumerate() {
                if to_nid == nid {
                    to_vec.remove(i);
                }
            }
        }
    }

    /// Get all nodes of type [InputNode].
    pub fn get_input_nodes(&self) -> Vec<NId> {
        let mut inputs: Vec<NId> = vec![];
        for (nid, node) in &self.vertices {
            if let Node::InputVariable(_) = node {
                inputs.push(*nid);
            }
        }
        inputs
    }

    /// Get all nodes of type [OutputNode].
    pub fn get_output_nodes(&self) -> Vec<NId> {
        let mut outputs: Vec<NId> = vec![];
        for (nid, node) in &self.vertices {
            if let Node::Output(_) = node {
                outputs.push(*nid);
            }
        }
        outputs
    }

    /// Check if a node is of type [Node::Output].
    fn is_output(&self, nid: NId) -> bool {
        if let Some(Node::Output(_)) = self.get_node(&nid) {
            return true;
        }
        false
    }

    pub fn is_wire_only_node(&self, nid: NId) -> bool {
        for (from, to, conn) in self.iter_conns() {
            if nid == from || nid == to {
                if let Connection::Combinator(_) = conn {
                    return false;
                }
            }
        }
        true
    }

    /// Stitch another graph into this one. Resuts a vector of outputs os the newly stitched in
    /// graph.
    /// NOTE: Order of inputs vec matter. The first input is the first argument to the block.
    #[allow(clippy::single_match)]
    pub fn stitch_graph(
        &mut self,
        other: &Graph,
        inputs: Vec<(NId, IOType)>, // Inputs to the other graph
    ) -> Result<Vec<(NId, IOType)>, String> {
        // Hashmap containing mappings from old to new node ids
        let mut nid_converter: fnv::FnvHashMap<NId, NId> = fnv::FnvHashMap::default();

        let mut other_graph_outputs: FnvHashMap<NId, IOType> = FnvHashMap::default();

        // Copy nodes from the other graph and assign them new ids.
        for (old_nid, node) in other.vertices.clone() {
            let new_nid = self.push_node(Node::Inner);
            nid_converter.insert(old_nid, new_nid);

            // Note this node as output
            if let Node::Output(output_type) = node {
                other_graph_outputs.insert(new_nid, output_type);
            }
        }

        // Copy connections
        for (old_from_nid, old_to_nid, conn) in other.iter_conns() {
            let new_from_nid = nid_converter[&old_from_nid];
            let new_to_nid = nid_converter[&old_to_nid];
            self.push_raw_connection(new_from_nid, new_to_nid, conn.clone());
        }

        let other_graph_inputs = other.get_non_constant_inputs();

        // This should not errror. It should have been checked by the type checker.
        if other_graph_inputs.len() != inputs.len() {
            panic!(
                "Number of arguments does not match with block definition: Expected {}, found {}. This is probably a bug in the typechecker.",
                other_graph_inputs.len(),
                inputs.len()
            );
        }

        for (i, (block_input_nid, block_input_type)) in inputs.iter().enumerate() {
            let (old_other_input_nid, other_input_node) = &other_graph_inputs[i];

            // Translate input nid to id in this graph
            let other_input_nid = nid_converter[old_other_input_nid];

            // Get the input type
            let other_input_type = match &other_input_node {
                Node::InputVariable(input_type) => input_type.clone(),
                _ => panic!("There should only be input nodes here..."),
            };

            match other_input_type {
                IOType::Signal(_) => {
                    // The input types for the (output) node on THIS graph, that is to be stitched togeather
                    // with the input of the other graph.
                    let input_types = self.get_input_iotypes(block_input_nid);

                    // TODO: There should definetly be a better way to get the input type.
                    debug_assert!(
                        input_types.len() == 1,
                        "Block inputs can only have one type. NOTE: This type may be `All`."
                    );

                    let (input_type, wc) = input_types[0].clone();

                    // This node is the transition point from this graph to the other graph, now
                    // stitched inside this one. The middle node contains signals of the type
                    // specified in the block arguments.
                    let middle_node = self.push_node(Node::Inner);

                    self.push_raw_connection(
                        *block_input_nid,
                        middle_node,
                        Connection::new_arithmetic(ArithmeticCombinator::new_convert(
                            input_type,
                            block_input_type.clone(),
                        )),
                    );

                    self.push_raw_connection(
                        middle_node,
                        other_input_nid,
                        Connection::new_arithmetic(ArithmeticCombinator::new_convert(
                            block_input_type.clone(),
                            other_input_type,
                        )),
                    );
                }
                IOType::AnySignal(_) => {
                    todo!()
                    // let new_type = self.get_single_input(block_input_nid).unwrap();
                    // self.replace_iotype(other_input_type, &new_type);
                    // self.push_raw_connection(
                    //     *block_input_nid,
                    //     other_input_nid,
                    //     Connection::new_arithmetic(ArithmeticCombinator::new_pick(new_type)),
                    // );
                }
                IOType::ConstantAny(_) => {
                    todo!()
                }
                IOType::ConstantSignal(_) => todo!(),
                IOType::Constant(_) => {
                    panic!("Compiler Error: Inputs to a block should not be constants.")
                }
                IOType::Everything => todo!(),
                IOType::Anything => todo!(),
                IOType::Each => todo!(),
            }

            match self.vertices.get_mut(block_input_nid) {
                Some(Node::Output(_)) => {
                    self.override_node(*block_input_nid, Node::Inner);
                }
                _ => {}
            }
        }

        Ok(other_graph_outputs
            .iter()
            .map(|(nid, type_o)| (*nid, type_o.clone()))
            .collect())
    }

    pub fn get_single_input(&self, nid: &NId) -> Result<IOType, String> {
        let inputs = self.get_input_iotypes(nid);
        if inputs.len() != 1 {
            return Err(format!("Could not get single input: {inputs:?}"));
        }
        Ok(inputs[0].0.clone())
    }

    fn _get_final_outputs(&self) -> Vec<(NId, Node)> {
        self.vertices
            .iter()
            .filter(|(nid, node)| {
                if let Node::Output(_) = node {
                    match self.adjacency.get(nid) {
                        Some(v) => v.is_empty(),
                        None => true,
                    }
                } else {
                    false
                }
            })
            .map(|(nid, node)| (*nid, node.clone()))
            .collect()
    }

    /// Get all input nodes that are not constants.
    fn get_non_constant_inputs(&self) -> Vec<(NId, Node)> {
        self.vertices
            .iter()
            .filter(|(_nid, node)| {
                if let Node::InputVariable(input_type) = node {
                    !matches!(input_type, IOType::Constant(_))
                } else {
                    false
                }
            })
            .map(|(nid, node)| (*nid, node.clone()))
            .collect()
    }

    pub fn assign_anysignals(&mut self) {
        AnysignalAssigner::new(self).assign();
    }

    /// Print the graph to stout.
    pub fn print(&self) {
        println!("Graph:");
        println!("\tVertecies:");
        for (nid, node) in &self.vertices {
            if self.vertices.get(nid).is_none() {
                println!("Could not find vertex: {}", nid);
                return;
            }
            let repr = match node {
                Node::Inner => "INNER",
                Node::InputVariable(_) => "INPUT_VAR",
                Node::Variable(_) => "VAR",
                Node::Output(_) => "OUTPUT",
                Node::Constant(_) => "CONST",
            };
            println!("\t\t{} : {} : {:?}", nid, repr, self.get_input_iotypes(nid))
        }
        println!("\n\tConnections:");
        for (from_nid, to_nid, conn) in self.iter_conns() {
            println!("\t\t{} -> {} : {}", from_nid, to_nid, conn);
        }
    }

    /// Visualize the graph in an svg.
    pub fn visualize(&self, output_path: &str) -> Result<(), crate::utils::VisualizerError> {
        graph_visualizer::visualize(self, output_path)
    }

    pub fn dot_repr(&self) -> String {
        graph_visualizer::create_dot(self)
    }
}

struct AnysignalAssigner<'a> {
    graph: &'a mut Graph,
    anysignal_to_signal: FnvHashMap<u64, String>,
    next_sig_nr: u64,
    used_signals: HashSet<String>,
    signal_names: Vec<&'static str>,
}

impl<'a> AnysignalAssigner<'a> {
    fn new(graph: &'a mut Graph) -> Self {
        let signal_names: Vec<&str> = BASE_SIGNALS
            .lines()
            .map(|l| {
                l.split_once(':')
                    .expect("there should always be a ':' denoting type:signal")
                    .1
            })
            .collect();
        Self {
            graph,
            anysignal_to_signal: FnvHashMap::default(),
            next_sig_nr: 0,
            used_signals: HashSet::new(),
            signal_names,
        }
    }

    fn get_signal(&self, signal_nr: u64) -> &str {
        self.signal_names[signal_nr as usize]
    }

    fn get_next_signal(&mut self) -> String {
        while self
            .used_signals
            .get(self.get_signal(self.next_sig_nr))
            .is_some()
        {
            self.next_sig_nr += 1
        }
        let sig = self.get_signal(self.next_sig_nr).to_string();
        self.next_sig_nr += 1;
        sig
    }

    fn assign(&mut self) {
        // Keep track of what signals are already used by the blueprint
        for to_vec in self.graph.adjacency.values() {
            for (_, conn) in to_vec {
                if let Connection::Combinator(com) = conn {
                    if let IOType::Signal(s) = com.get_output_iotype() {
                        self.used_signals.insert(s);
                    }
                }
            }
        }

        // Create conversions for anysignals
        for to_vec in self.graph.adjacency.clone().values() {
            for (_, conn) in to_vec {
                if let Connection::Combinator(com) = conn {
                    match com {
                        Combinator::Arithmetic(c) => {
                            self.assign_if_anysignal(&c.left);
                            self.assign_if_anysignal(&c.right);
                            self.assign_if_anysignal(&c.output);
                        }
                        Combinator::Decider(c) => {
                            self.assign_if_anysignal(&c.left);
                            self.assign_if_anysignal(&c.right);
                            self.assign_if_anysignal(&c.output);
                        }
                        Combinator::Gate(c) => {
                            self.assign_if_anysignal(&c.left);
                            self.assign_if_anysignal(&c.right);
                            self.assign_if_anysignal(&c.gate_type);
                        }
                        Combinator::Constant(c) => {
                            self.assign_if_anysignal(&c.type_);
                        }
                    }
                }
            }
        }

        // Convert anysignals
        self.replace_anysignals();
    }

    fn assign_if_anysignal(&mut self, iotype: &IOType) {
        let n = match iotype {
            IOType::AnySignal(n) => n,
            IOType::ConstantAny((n, _number)) => n,
            _ => return,
        };
        if let Some(_signal) = self.anysignal_to_signal.get(n) {
        } else {
            let sig = self.get_next_signal();
            self.anysignal_to_signal.insert(*n, sig);
        }
    }

    fn replace_if_anysignal(&mut self, mut iotype: &mut IOType) {
        match &mut iotype {
            IOType::AnySignal(n) => {
                let sig = self.anysignal_to_signal.get(n).unwrap();
                *iotype = IOType::Signal(sig.to_string());
            }
            IOType::ConstantAny((n, count)) => {
                let sig = self.anysignal_to_signal.get(n).unwrap();
                *iotype = IOType::ConstantSignal((sig.to_string(), *count))
            }
            _ => return,
        };
    }

    /// Replace an [IOType] with another throughout the whole graph. This is usefull when assigning
    /// `IOType::Any` actual factorio signals.
    fn replace_anysignals(&mut self) {
        // Convert connections
        let mut adjacency = self.graph.adjacency.clone();
        for (_, to_vec) in adjacency.iter_mut() {
            for (_, conn) in to_vec {
                match conn {
                    Connection::Combinator(Combinator::Arithmetic(ac)) => {
                        self.replace_if_anysignal(&mut ac.left);
                        self.replace_if_anysignal(&mut ac.right);
                        self.replace_if_anysignal(&mut ac.output);
                    }
                    Connection::Combinator(Combinator::Decider(dc)) => {
                        self.replace_if_anysignal(&mut dc.left);
                        self.replace_if_anysignal(&mut dc.right);
                        self.replace_if_anysignal(&mut dc.output);
                    }
                    Connection::Combinator(Combinator::Gate(gc)) => {
                        self.replace_if_anysignal(&mut gc.left);
                        self.replace_if_anysignal(&mut gc.right);
                        self.replace_if_anysignal(&mut gc.gate_type);
                    }
                    Connection::Combinator(Combinator::Constant(cc)) => {
                        self.replace_if_anysignal(&mut cc.type_);
                    }
                    Connection::Wire(_) => {}
                }
            }
        }
        self.graph.adjacency = adjacency;

        // Convert Nodes
        let mut nodes = self.graph.vertices.clone();
        for node in &mut nodes.values_mut() {
            match node {
                Node::InputVariable(t) => self.replace_if_anysignal(t),
                Node::Variable(t) => self.replace_if_anysignal(t),
                Node::Output(t) => self.replace_if_anysignal(t),
                Node::Constant(t) => self.replace_if_anysignal(t),
                Node::Inner => {}
            }
        }
        self.graph.vertices = nodes;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WireConnection {
    Green,
    Red,
    Both,
}

impl WireConnection {
    pub fn from_wire_kind(wk: &WireKind) -> Self {
        match wk {
            WireKind::Green => Self::Green,
            WireKind::Red => Self::Red,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_node_network() {
        let mut g = Graph::new();
        let conn = Connection::Combinator(Combinator::new_pick(IOType::Signal("test".to_string())));

        let n1 = g.push_inner_node();
        let n2 = g.push_inner_node();
        let (n3, _n4) = g.push_connection(conn.clone());
        let (n6, _n5) = g.push_connection(conn.clone());
        let (n4, n5) = g.push_connection(conn);

        // Network 1: n1 -- n2 -- n3 -- n1 (circle)
        // Network 2: n4 -- n6
        // Network 3: n5
        let wire_kind = WireKind::Green;
        g.push_wire(n1, n2, wire_kind.clone());
        g.push_wire(n2, n3, wire_kind.clone());
        g.push_wire(n3, n1, wire_kind.clone());
        g.push_wire(n4, n6, wire_kind);

        // Check network 1
        let mut network1_1 = g.get_node_network(&n1, WireKind::Green);
        let mut network1_2 = g.get_node_network(&n2, WireKind::Green);
        let mut network1_3 = g.get_node_network(&n3, WireKind::Green);
        network1_1.sort();
        network1_2.sort();
        network1_3.sort();
        assert_eq!(network1_1, vec![n1, n2, n3, 6]);
        assert_eq!(network1_1, network1_2);
        assert_eq!(network1_1, network1_3);

        // Check network 2
        let mut network2_1 = g.get_node_network(&n4, WireKind::Green);
        let mut network2_2 = g.get_node_network(&n6, WireKind::Green);
        network2_1.sort();
        network2_2.sort();
        assert_eq!(network2_1, vec![n4, n6, 7, 8, 10]);
        assert_eq!(network2_1, network2_2);

        // Check network 3
        let mut network3 = g.get_node_network(&n5, WireKind::Green);
        network3.sort();
        assert_eq!(network3, vec![n5, 9, 11]);
    }
}
