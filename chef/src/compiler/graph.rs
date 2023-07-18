//! Implementation of [Graph]. A graph for representing a network of factorio combinators.

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
            DeciderOperation::LargerThan => "LARGERTHAN",
            DeciderOperation::LargerThanOrEqual => "LARGERTHANOREQUAL",
            DeciderOperation::LessThan => "LESSTHAN",
            DeciderOperation::LessThanOrEqual => "LESSTHANOREQUAL",
            DeciderOperation::Equals => "EQUALS",
            DeciderOperation::NotEquals => "NOTEQUALS",
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

/// Type if inputs/outputs of combinators
#[derive(Clone, Debug, PartialEq)]
pub enum IOType {
    Signal(String),
    AnySignal(u64),
    Constant(i32),
    ConstantSignal((String, i32)),
    All,
}

impl Display for IOType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IOType::Signal(s) => format!("Sig({})", s),
            IOType::AnySignal(n) => format!("Any({})", n),
            IOType::Constant(n) => format!("({})", n),
            IOType::ConstantSignal((sig, n)) => format!("({}, {})", sig, n),
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
pub enum Connection {
    Arithmetic(ArithmeticConnection),
    Decider(DeciderConnection),
    Gate(GateConnection),
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

    pub fn get_output(&self) -> IOType {
        match self {
            Connection::Arithmetic(ac) => ac.output.clone(),
            Connection::Decider(dc) => dc.output.clone(),
            Connection::Gate(gc) => gc.gate_type.clone(),
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
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Inner(InnerNode),
    Input(InputNode),
    Output(OutputNode),
}

#[derive(Clone, Debug)]
pub struct OutputNode {
    pub output_type: IOType,
}

impl OutputNode {
    pub fn new(output_type: IOType) -> Self {
        Self { output_type }
    }
}

#[derive(Clone, Debug)]
pub struct InputNode {
    pub input: IOType,
}

impl InputNode {
    pub fn new(input: IOType) -> Self {
        Self { input }
    }
}

#[derive(Clone, Debug)]
pub struct InnerNode {}

impl InnerNode {
    pub fn new() -> Self {
        Self {}
    }
}

/// Index of a node in a [Graph].
pub type NId = u64;

/// A graph for storing connection and nodes representing factorio combinators.
#[derive(Clone)]
pub struct Graph {
    pub vertices: FnvHashMap<NId, Node>,
    pub adjacency: FnvHashMap<NId, Vec<(NId, Connection)>>,
    pub next_vid: NId,
}

impl Graph {
    /// Instantiate a new [Graph].
    pub fn new() -> Graph {
        Graph {
            vertices: FnvHashMap::default(),
            adjacency: FnvHashMap::default(),
            next_vid: 0,
        }
    }

    pub fn get_inputs(&self, vid: &NId) -> Vec<IOType> {
        match self.vertices.get(vid) {
            Some(Node::Input(input_node)) => {
                return vec![input_node.input.clone()];
            }
            None => {
                return vec![];
            }
            Some(_) => {} // Continue
        };
        let mut inputs: Vec<IOType> = vec![];

        for to_vec in self.adjacency.values() {
            for (to_vid, conn) in to_vec {
                if to_vid == vid {
                    inputs.push(conn.get_output());
                }
            }
        }
        inputs
    }

    /// Get a graph node by id.
    pub fn get_node(&self, vid: &NId) -> Option<&Node> {
        self.vertices.get(vid)
    }

    /// Get a mutable graph node by id.
    pub fn _get_mut_node(&mut self, vid: &NId) -> Option<&mut Node> {
        self.vertices.get_mut(vid)
    }

    /// Add a node to the graph. Returns the new node's given id.
    pub fn push_node(&mut self, node: Node) -> NId {
        let vid = self.next_vid;
        if self.vertices.insert(vid, node).is_some() {
            panic!("Could not insert node into graph")
        }
        self.next_vid += 1;
        vid
    }

    /// Override a node at a given id.
    pub fn override_node(&mut self, vid: NId, node: Node) -> Option<Node> {
        self.vertices.insert(vid, node)
    }

    /// Push a node of type [InputNode].
    pub fn push_input_node(&mut self, input: IOType) -> NId {
        self.push_node(Node::Input(InputNode::new(input)))
    }

    /// Push a node of type [InnerNode].
    pub fn push_inner_node(&mut self) -> NId {
        self.push_node(Node::Inner(InnerNode::new()))
    }

    pub fn push_output_node(&mut self, output_type: IOType) -> NId {
        self.push_node(Node::Output(OutputNode::new(output_type)))
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
        for (i, (to_vid, _from_conn)) in from_vertex_connections.iter().enumerate() {
            if to_vid == to {
                from_vertex_connections.remove(i);
                break;
            }
        }
    }

    /// Remove a node and all of its connections.
    pub fn remove_node_with_connections(&mut self, vid: &NId) {
        self.adjacency.remove(vid);
        self.vertices.remove(vid);
        for (_, to_vec) in self.adjacency.iter_mut() {
            for (i, (to_vid, _)) in to_vec.clone().iter().enumerate() {
                if to_vid == vid {
                    to_vec.remove(i);
                }
            }
        }
    }

    /// Get all nodes of type [InputNode].
    pub fn get_input_nodes(&self) -> Vec<NId> {
        let mut inputs: Vec<NId> = vec![];
        for (vid, node) in &self.vertices {
            if let Node::Input(_) = node {
                inputs.push(*vid);
            }
        }
        inputs
    }

    /// Check if a node is of type [Node::Output].
    fn is_output(&self, vid: NId) -> bool {
        if let Some(Node::Output(_)) = self.get_node(&vid) {
            return true;
        }
        false
    }

    /// Compine two graphs by stitching them togeather.
    /// TODO: Order of inputs vec matter
    #[allow(clippy::single_match)]
    pub fn stitch_graph(
        &mut self,
        other: &Graph,
        inputs: Vec<(NId, IOType)>,
    ) -> Result<Vec<(NId, IOType)>, String> {
        let mut vid_converter: fnv::FnvHashMap<NId, NId> = fnv::FnvHashMap::default();

        let mut outputs: FnvHashMap<NId, IOType> = FnvHashMap::default();

        // copy nodes
        for (old_vid, node) in other.vertices.clone() {
            let new_vid = self.push_node(node);
            vid_converter.insert(old_vid, new_vid);
        }

        // copy connections
        for (old_from_vid, to_vec) in other.adjacency.clone() {
            for (old_to_vid, conn) in to_vec {
                let new_from_vid = vid_converter[&old_from_vid];
                let new_to_vid = vid_converter[&old_to_vid];
                self.push_connection(new_from_vid, new_to_vid, conn.clone());
                if self.is_output(new_to_vid) {
                    outputs.insert(new_to_vid, conn.get_output());
                }
            }
        }

        let other_graph_inputs = other.get_non_constant_inputs();

        if other_graph_inputs.len() != inputs.len() {
            return Err(format!(
                "Number of arguments does not match with block definition: Expected {}, found {}.",
                other_graph_inputs.len(),
                inputs.len()
            ));
        }

        for (i, (block_input_vid, block_input_type)) in inputs.iter().enumerate() {
            let other_t = &other_graph_inputs[i];
            let to_vid = vid_converter[&other_t.0];

            // Get the input type, and convert inputs to inner nodes
            let signal = match &other_t.1 {
                Node::Input(n) => {
                    self.override_node(to_vid, Node::Inner(InnerNode::new()));
                    n.input.clone()
                }
                _ => panic!("There should only be input nodes here..."),
            };

            match signal {
                IOType::Signal(_) => {
                    let middle_node = self.push_node(Node::Inner(InnerNode::new()));
                    let input_types = self.get_inputs(block_input_vid);

                    debug_assert!(input_types.len() == 1);

                    let input_type = input_types[0].clone();

                    self.push_connection(
                        *block_input_vid,
                        middle_node,
                        Connection::Arithmetic(ArithmeticConnection::new_convert(
                            input_type,
                            block_input_type.clone(),
                        )),
                    );

                    self.push_connection(
                        middle_node,
                        to_vid,
                        Connection::Arithmetic(ArithmeticConnection::new_convert(
                            block_input_type.clone(),
                            signal,
                        )),
                    );
                }
                IOType::AnySignal(_) => {
                    // TODO : Something is wrong here
                    let new_type = self.get_single_input(block_input_vid).unwrap();
                    self.replace_iotype(signal, &new_type);
                    self.push_connection(
                        *block_input_vid,
                        to_vid,
                        Connection::Arithmetic(ArithmeticConnection::new_pick(new_type)),
                    );
                }
                IOType::ConstantSignal(_) => todo!(),
                IOType::Constant(_) => {
                    panic!("Compiler Error: Inputs to a block should not be constants.")
                }
                IOType::All => todo!(),
            }

            match self.vertices.get_mut(block_input_vid) {
                Some(Node::Output(_)) => {
                    self.override_node(*block_input_vid, Node::Inner(InnerNode::new()));
                }
                _ => {}
            }
        }

        Ok(outputs
            .iter()
            .map(|(vid, type_o)| (*vid, type_o.clone()))
            .collect())
    }

    pub fn get_single_input(&self, vid: &NId) -> Result<IOType, String> {
        let inputs = self.get_inputs(vid);
        if inputs.len() != 1 {
            return Err("Could not get single input".to_string());
        }
        Ok(inputs[0].clone())
    }

    /// Replace an [IOType] with another throughout the whole graph. This is usefull when assigning
    /// `IOType::Any` actual factorio signals.
    fn replace_iotype(&mut self, old_type: IOType, new_type: &IOType) {
        for (_from_vid, to_vec) in self.adjacency.iter_mut() {
            for (_to_vid, conn) in to_vec {
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
                }
            }
        }
    }

    fn _get_final_outputs(&self) -> Vec<(NId, Node)> {
        self.vertices
            .iter()
            .filter(|(vid, node)| {
                if let Node::Output(_) = node {
                    match self.adjacency.get(vid) {
                        Some(v) => v.is_empty(),
                        None => true,
                    }
                } else {
                    false
                }
            })
            .map(|(vid, node)| (*vid, node.clone()))
            .collect()
    }

    /// Get all input nodes that are not constants.
    fn get_non_constant_inputs(&self) -> Vec<(NId, Node)> {
        self.vertices
            .iter()
            .filter(|(_vid, node)| {
                if let Node::Input(n) = node {
                    !matches!(n.input, IOType::Constant(_))
                } else {
                    false
                }
            })
            .map(|(vid, node)| (*vid, node.clone()))
            .collect()
    }

    pub fn assign_anysignals(&mut self) {
        AnysignalAssigner::new(self).assign();
    }

    /// Print the graph to stout.
    pub fn print(&self) {
        println!("Graph:");
        println!("\tVertecies:");
        for (vid, node) in &self.vertices {
            if self.vertices.get(vid).is_none() {
                println!("Could not find vertex: {}", vid);
                return;
            }
            match node {
                Node::Inner(_) => println!("\t\t{} : INNER : {:?}", vid, self.get_inputs(vid)),
                Node::Input(_) => println!("\t\t{} : INPUT : {:?}", vid, self.get_inputs(vid)),
                Node::Output(_n) => println!("\t\t{} : OUTPUT : {:?}", vid, self.get_inputs(vid)),
            }
        }
        println!("\n\tConnections:");
        for (vid, to) in &self.adjacency {
            for (k, v) in to {
                println!("\t\t{} -> {} : {}", vid, k, v);
            }
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
                if let IOType::Signal(s) = conn.get_output() {
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
