//! Implementation of [Graph]. A graph for representing a network of factorio combinators.

#![allow(dead_code)] // TODO: remove

use fnv::FnvHashMap;
use std::{collections::HashSet, fmt::Display, usize};

use crate::utils::BASE_SIGNALS;

use super::graph_visualizer;

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
pub struct DeciderConnection {
    pub left: IOType,
    pub right: IOType,
    pub operation: DeciderOperation,
    pub output: IOType,
}

impl DeciderConnection {
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
    _ConstantSignal((String, i32)), // TODO: Add constant signals
    All,
}

impl IOType {
    pub fn new_signal<S>(signal: S) -> Self
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
            IOType::_ConstantSignal((sig, n)) => format!("({}, {})", sig, n),
            IOType::All => "ALL".to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArithmeticConnection {
    pub left: IOType,
    pub right: IOType,
    pub operation: ArithmeticOperation,
    pub output: IOType,
}

impl ArithmeticConnection {
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct GateConnection {
    pub left: IOType,
    pub right: IOType,
    pub operation: DeciderOperation,
    pub gate_type: IOType,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantConnection {
    pub type_: IOType,
    pub count: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Connection {
    Arithmetic(ArithmeticConnection),
    Decider(DeciderConnection),
    Gate(GateConnection),
    Constant(ConstantConnection),
}

impl Connection {
    pub fn new_pick(signal: IOType) -> Self {
        Self::Arithmetic(ArithmeticConnection::new_pick(signal))
    }

    pub fn new_convert(input_signal: IOType, output_signal: IOType) -> Self {
        Self::Arithmetic(ArithmeticConnection::new_convert(
            input_signal,
            output_signal,
        ))
    }

    pub fn get_output_iotype(&self) -> IOType {
        match self {
            Connection::Arithmetic(ac) => ac.output.clone(),
            Connection::Decider(dc) => dc.output.clone(),
            Connection::Gate(gc) => gc.gate_type.clone(),
            Connection::Constant(cc) => cc.type_.clone(),
        }
    }

    #[allow(irrefutable_let_patterns)]
    pub fn is_pick(&self) -> bool {
        if let Connection::Arithmetic(connection) = self {
            if connection.right == IOType::Constant(0)
                && connection.operation == ArithmeticOperation::Add
                && connection.output == connection.left
            {
                return true;
            }
        }
        false
    }

    #[allow(irrefutable_let_patterns)]
    pub fn is_convert(&self) -> bool {
        if let Connection::Arithmetic(connection) = self {
            connection.right == IOType::Constant(0)
                && connection.operation == ArithmeticOperation::Add
                && connection.output != connection.left
        } else {
            false
        }
    }
}

impl Display for Connection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Connection::Arithmetic(connection) => {
                if self.is_pick() {
                    format!("PICK: {}", connection.output)
                } else if self.is_convert() {
                    format!("CONVERT: {} -> {}", connection.left, connection.output)
                } else {
                    format!(
                        "{}: {}, {}",
                        connection.operation, connection.left, connection.right
                    )
                }
            }
            Connection::Decider(connection) => format!(
                "{}: {}, {}",
                connection.operation, connection.left, connection.right
            ),
            Connection::Gate(gate) => format!(
                "GATE: {}\n{}: {}, {}",
                gate.gate_type, gate.operation, gate.left, gate.right
            ),
            Connection::Constant(con) => format!("CONSTANT : {} = {}", con.type_, con.count),
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

    // A `None` is used when reperesenting a constant combinator. As the graph reperesents all
    // combinators as connections, the constant combinator must also be. The constant combinator
    // does however not take any input, therefore its input is connected to a `None` node.
    Constant(IOType),
}

/// Index of a node in a [Graph].
pub type NId = u64;

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

    pub fn get_input_iotypes(&self, nid: &NId) -> Vec<IOType> {
        match self.vertices.get(nid) {
            Some(Node::InputVariable(input_node)) => {
                return vec![input_node.clone()];
            }
            Some(Node::Constant(const_node)) => {
                return vec![const_node.clone()];
            }
            None => {
                return vec![];
            }
            Some(_) => {} // Continue
        };
        let mut inputs: Vec<IOType> = vec![];

        for (_from_nid, to_nid, conn) in self.iter_conns() {
            if &to_nid == nid {
                inputs.push(conn.get_output_iotype());
            }
        }

        inputs
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

    /// Push a connection between two nodes. Connections represent combinator operations.
    pub fn push_connection(&mut self, from: NId, to: NId, connection: Connection) {
        let adjacent_to_from = self.adjacency.entry(from).or_default();
        adjacent_to_from.push((to, connection));
    }

    pub fn push_gate_connection(
        &mut self,
        input: NId,
        output: NId,
        cond_type: IOType,
        gate_type: IOType,
    ) {
        self.push_connection(
            input,
            output,
            Connection::Gate(GateConnection {
                left: cond_type,
                right: IOType::Constant(0),
                operation: DeciderOperation::LargerThan,
                gate_type,
            }),
        );
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
            self.push_connection(new_from_nid, new_to_nid, conn.clone());
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

                    let input_type = input_types[0].clone();

                    // This node is the transition point from this graph to the other graph, now
                    // stitched inside this one. The middle node contains signals of the type
                    // specified in the block arguments.
                    let middle_node = self.push_node(Node::Inner);

                    self.push_connection(
                        *block_input_nid,
                        middle_node,
                        Connection::Arithmetic(ArithmeticConnection::new_convert(
                            input_type,
                            block_input_type.clone(),
                        )),
                    );

                    self.push_connection(
                        middle_node,
                        other_input_nid,
                        Connection::Arithmetic(ArithmeticConnection::new_convert(
                            block_input_type.clone(),
                            other_input_type,
                        )),
                    );
                }
                IOType::AnySignal(_) => {
                    // TODO: Something is wrong here. Maybe we need a `middle` node? Then we could
                    // merge with the `Signal(_)` case.

                    let new_type = self.get_single_input(block_input_nid).unwrap();
                    self.replace_iotype(other_input_type, &new_type);
                    self.push_connection(
                        *block_input_nid,
                        other_input_nid,
                        Connection::Arithmetic(ArithmeticConnection::new_pick(new_type)),
                    );
                }
                IOType::_ConstantSignal(_) => todo!(),
                IOType::Constant(_) => {
                    panic!("Compiler Error: Inputs to a block should not be constants.")
                }
                IOType::All => todo!(),
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
            return Err("Could not get single input".to_string());
        }
        Ok(inputs[0].clone())
    }

    /// Replace an [IOType] with another throughout the whole graph. This is usefull when assigning
    /// `IOType::Any` actual factorio signals.
    fn replace_iotype(&mut self, old_type: IOType, new_type: &IOType) {
        for (_, to_vec) in self.adjacency.iter_mut() {
            for (_, conn) in to_vec {
                match conn {
                    Connection::Arithmetic(ac) => {
                        if ac.left == old_type {
                            ac.left = new_type.clone()
                        }
                        if ac.right == old_type {
                            ac.right = new_type.clone()
                        }
                        if ac.output == old_type {
                            ac.output = new_type.clone()
                        }
                    }
                    Connection::Decider(dc) => {
                        if dc.left == old_type {
                            dc.left = new_type.clone()
                        }
                        if dc.right == old_type {
                            dc.right = new_type.clone()
                        }
                        if dc.output == old_type {
                            dc.output = new_type.clone()
                        }
                    }
                    Connection::Gate(gc) => {
                        if gc.left == old_type {
                            gc.left = new_type.clone()
                        }
                        if gc.right == old_type {
                            gc.right = new_type.clone()
                        }
                        if gc.gate_type == old_type {
                            gc.gate_type = new_type.clone()
                        }
                    }
                    Connection::Constant(cc) => {
                        if cc.type_ == old_type {
                            cc.type_ = new_type.clone()
                        }
                    }
                }
            }
        }
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
                if let IOType::Signal(s) = conn.get_output_iotype() {
                    self.used_signals.insert(s);
                }
            }
        }

        // Create conversions for anysignals
        for to_vec in self.graph.adjacency.clone().values() {
            for (_, conn) in to_vec {
                match conn {
                    Connection::Arithmetic(c) => {
                        self.assign_if_anysignal(&c.left);
                        self.assign_if_anysignal(&c.right);
                        self.assign_if_anysignal(&c.output);
                    }
                    Connection::Decider(c) => {
                        self.assign_if_anysignal(&c.left);
                        self.assign_if_anysignal(&c.right);
                        self.assign_if_anysignal(&c.output);
                    }
                    Connection::Gate(c) => {
                        self.assign_if_anysignal(&c.left);
                        self.assign_if_anysignal(&c.right);
                        self.assign_if_anysignal(&c.gate_type);
                    }
                    Connection::Constant(c) => {
                        self.assign_if_anysignal(&c.type_);
                    }
                }
            }
        }

        // Convert anysignals
        for (any_n, sig) in self.anysignal_to_signal.clone() {
            self.graph
                .replace_iotype(IOType::AnySignal(any_n), &IOType::Signal(sig))
        }
    }

    fn assign_if_anysignal(&mut self, iotype: &IOType) {
        if let IOType::AnySignal(n) = iotype {
            if let Some(_signal) = self.anysignal_to_signal.get(n) {
            } else {
                let sig = self.get_next_signal();
                self.anysignal_to_signal.insert(*n, sig);
            }
        }
    }
}
