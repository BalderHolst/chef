//! Implementation of [Graph]. A graph for representing a network of factorio combinators.

use fnv::FnvHashMap;
use std::fmt::Debug;
use std::{collections::HashSet, fmt::Display, usize};

use crate::utils::BASE_SIGNALS;

use super::graph_visualizer;

const DEFAULT_WIRE_KIND: WireKind = WireKind::Red;

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
    EveryEquals,
    EveryLargerThan,
    EveryLargerThanEquals,
    EveryLessThan,
    EveryLessThanEquals,
    EveryNotEquals,
    AnyEquals,
    AnyLargerThan,
    AnyLargerThanEquals,
    AnyLessThan,
    AnyLessThanEquals,
    AnyNotEquals,
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
            DeciderOperation::EveryEquals => "EVERY_EQUALS",
            DeciderOperation::EveryLargerThan => "EVERY_LARGER_THAN",
            DeciderOperation::EveryLargerThanEquals => "EVERY_LARGER_THAN_EQUALS",
            DeciderOperation::EveryLessThan => "EVERY_LESS_THAN",
            DeciderOperation::EveryLessThanEquals => "EVERY_LESS_THAN_EQUALS",
            DeciderOperation::EveryNotEquals => "EVERY_NOT_EQUALS",
            DeciderOperation::AnyEquals => "ANY_EQUALS",
            DeciderOperation::AnyLargerThan => "ANY_LARGER_THAN",
            DeciderOperation::AnyLargerThanEquals => "ANY_LARGER_THAN_EQUALS",
            DeciderOperation::AnyLessThan => "ANY_LESS_THAN",
            DeciderOperation::AnyLessThanEquals => "ANY_LESS_THAN_EQUALS",
            DeciderOperation::AnyNotEquals => "ANY_NOT_EQUALS",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeciderOp<S> {
    pub left: S,
    pub right: S,
    pub operation: DeciderOperation,
    pub output: S,
}

impl<LooseSig> DeciderOp<LooseSig> {
    pub fn new(
        left: LooseSig,
        right: LooseSig,
        operation: DeciderOperation,
        output: LooseSig,
    ) -> Self {
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

pub trait Signal<S> {
    fn get_output_iotype(&self) -> S;

    fn is_const(&self) -> bool;

    fn new_const(c: i32) -> Self;
}

/// A determined signal
#[derive(Clone, Debug, PartialEq)]
pub enum DetSig {
    Signal(String),
    Constant(i32),
    ConstantSignal((String, i32)),
    Many,
}

impl Signal<DetSig> for DetSig {
    fn get_output_iotype(&self) -> Self {
        self.clone()
    }

    fn new_const(c: i32) -> Self {
        Self::Constant(c)
    }

    fn is_const(&self) -> bool {
        matches!(self, Self::Constant(_))
    }
}

/// Signals that may now have been determined yet
#[derive(Clone, Debug, PartialEq)]
pub enum LooseSig {
    Signal(String),
    AnySignal(u64),
    Constant(i32),
    ConstantSignal((String, i32)),
    ConstantAny((u64, i32)),
    Many,
}

impl Signal<LooseSig> for LooseSig {
    fn get_output_iotype(&self) -> Self {
        self.clone()
    }

    fn new_const(c: i32) -> Self {
        Self::Constant(c)
    }

    fn is_const(&self) -> bool {
        matches!(self, Self::Constant(_))
    }
}

impl LooseSig {
    pub fn signal<S>(signal: S) -> Self
    where
        S: ToString,
    {
        Self::Signal(signal.to_string())
    }

    pub fn to_combinator_type(&self) -> Self {
        match self {
            Self::ConstantSignal((sig, _)) => LooseSig::Signal(sig.clone()),
            Self::ConstantAny((n, _)) => LooseSig::AnySignal(*n),
            Self::Signal(_) => self.clone(),
            Self::AnySignal(_) => self.clone(),
            Self::Constant(_) => self.clone(),
            Self::Many => self.clone(),
        }
    }

    pub fn to_constant(&self, count: i32) -> Option<Self> {
        match self {
            Self::Signal(s) => Some(Self::ConstantSignal((s.clone(), count))),
            Self::AnySignal(n) => Some(Self::ConstantAny((*n, count))),
            Self::ConstantSignal(_) => Some(self.clone()),
            Self::ConstantAny(_) => Some(self.clone()),
            Self::Constant(_) => None,
            Self::Many => None,
        }
    }
}

impl Display for LooseSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Signal(s) => format!("Sig({})", s),
            Self::AnySignal(n) => format!("Any({})", n),
            Self::Constant(n) => format!("({})", n),
            Self::ConstantSignal((sig, n)) => format!("Const({}, {})", sig, n),
            Self::ConstantAny((sig, n)) => format!("ConstAny({}, {})", sig, n),
            Self::Many => "Many".to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArithmeticOp<S> {
    pub left: S,
    pub right: S,
    pub operation: ArithmeticOperation,
    pub output: S,
}

impl<S> ArithmeticOp<S>
where
    S: Signal<S> + PartialEq,
{
    pub fn new(left: S, right: S, operation: ArithmeticOperation, output: S) -> Self {
        Self {
            left,
            right,
            operation,
            output,
        }
    }

    pub fn new_convert(in_signal: S, out_signal: S) -> Self {
        Self::new(
            in_signal,
            S::new_const(0),
            ArithmeticOperation::Add,
            out_signal,
        )
    }

    fn is_convert(&self) -> bool {
        self.right == S::new_const(0)
            && self.operation == ArithmeticOperation::Add
            && self.output != self.left
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PickOp<S> {
    pub output: S,
}

impl<S> PickOp<S> {
    pub fn new(pick: S) -> Self {
        Self { output: pick }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConvertOp<S> {
    pub input: S,
    pub output: S,
}

impl<S> ConvertOp<S> {
    pub fn new(input: S, output: S) -> Self {
        Self { input, output }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct GateOp<S> {
    pub left: S,
    pub right: S,
    pub operation: DeciderOperation,
    pub gate_type: S,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DelayOp<S> {
    pub output: S,
}

impl<S> DelayOp<S> {
    pub fn new(output: S) -> Self {
        Self { output }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SumOp<S> {
    pub output: S,
}

impl<S> SumOp<S> {
    pub fn new(output: S) -> Self {
        Self { output }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantCombinator {
    pub type_: LooseSig,
    pub count: i32,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
pub enum Operation<S> {
    Arithmetic(ArithmeticOp<S>),
    Decider(DeciderOp<S>),
    Gate(GateOp<S>),
    Pick(PickOp<S>),
    Convert(ConvertOp<S>),

    /// Causes one tick of delay
    Delay(DelayOp<S>),

    /// Calculates the sum of all its inputs
    Sum(SumOp<S>),
}

impl<S> Operation<S>
where
    S: Clone,
{
    pub fn new_pick(signal: S) -> Self {
        Self::Pick(PickOp::new(signal))
    }

    pub fn new_convert(in_signal: S, out_signal: S) -> Self {
        Self::Convert(ConvertOp::new(in_signal, out_signal))
    }

    pub fn new_delay(output: S) -> Self {
        Self::Delay(DelayOp::new(output))
    }

    pub fn get_output_iotype(&self) -> S {
        match self {
            Self::Arithmetic(ac) => ac.output.clone(),
            Self::Decider(dc) => dc.output.clone(),
            Self::Pick(pc) => pc.output.clone(),
            Self::Convert(cc) => cc.output.clone(),
            Self::Gate(gc) => gc.gate_type.clone(),
            Self::Delay(dc) => dc.output.clone(),
            Self::Sum(sc) => sc.output.clone(),
        }
    }
}

impl<S> Display for Operation<S>
where
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Operation::Pick(pc) => {
                format!("PICK: {}", pc.output)
            }
            Operation::Convert(cc) => {
                format!("CONVERT: {}, {}", cc.input, cc.output)
            }
            Operation::Arithmetic(ac) => {
                format!("{}: {}, {}", ac.operation, ac.left, ac.right)
            }
            Operation::Decider(dc) => {
                format!("{}: {}, {}", dc.operation, dc.left, dc.right)
            }
            Operation::Gate(gate) => format!(
                "GATE: {}\n{}: {}, {}",
                gate.gate_type, gate.operation, gate.left, gate.right
            ),
            Operation::Delay(dc) => format!("DELAY: {}", dc.output),
            Operation::Sum(sc) => format!("SUM: {}", sc.output),
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Connection<S>
where
    S: Clone,
{
    Wire(WireKind),
    Combinator(Operation<S>),
}

impl<S> Connection<S>
where
    S: Clone,
{
    fn new_combinator(com: Operation<S>) -> Self {
        Self::Combinator(com)
    }

    pub fn new_arithmetic(ac: ArithmeticOp<S>) -> Self {
        Self::new_combinator(Operation::Arithmetic(ac))
    }

    pub fn new_decider(dc: DeciderOp<S>) -> Self {
        Self::new_combinator(Operation::Decider(dc))
    }

    pub fn new_gate(gate: GateOp<S>) -> Self {
        Self::new_combinator(Operation::Gate(gate))
    }

    pub fn new_pick(signal: S) -> Self {
        Self::new_combinator(Operation::new_pick(signal))
    }

    pub fn new_convert(input_signal: S, output_signal: S) -> Self {
        Self::new_combinator(Operation::new_convert(input_signal, output_signal))
    }
}

impl<S> Display for Connection<S>
where
    S: Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Combinator(com) => com.to_string(),
            Self::Wire(wire) => wire.to_string(),
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug)]
pub enum Node<S> {
    Inner,

    /// Variable node that is an input to the graph
    InputVariable {
        kind: S,
        name: String,
    },

    /// Variable node that is integrated in the graph
    Variable {
        kind: S,
        name: String,
    },

    /// An output of the entire graph
    // TODO: This can probably be removed, as output nodes and their output types can be derived
    // from the graph structure itself.
    Output {
        kind: S,
        name: String,
    },

    // TODO: Maybe this should just contain the constant and not a generic iotype?
    Constant(S),
}

impl Node<LooseSig> {
    pub fn get_constant_value(&self) -> Option<(LooseSig, i32)> {
        match self {
            Self::Constant(LooseSig::ConstantSignal((sig, count))) => {
                Some((LooseSig::Signal(sig.to_string()), *count))
            }
            Self::Constant(LooseSig::ConstantAny((n, count))) => {
                Some((LooseSig::AnySignal(*n), *count))
            }
            _ => None,
        }
    }
}

/// Index of a node in a [Graph].
pub type NId = u64;

/// Identifier for a network of nodes directly connected by wires.
pub type NetworkId = usize;

// TODO: Make `print` the debug or display function
/// A graph for storing connection and nodes representing factorio combinators.
#[derive(Clone)]
pub struct Graph<S>
where
    S: Debug + Clone + PartialEq + Display + Signal<S>,
{
    pub vertices: FnvHashMap<NId, Node<S>>,
    pub adjacency: FnvHashMap<NId, Vec<(NId, Connection<S>)>>,
    pub next_nid: NId,
}

impl<S> Graph<S>
where
    S: Debug + Clone + PartialEq + Display + Signal<S>,
{
    /// Instantiate a new [Graph].
    pub fn new() -> Graph<S> {
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

    pub fn get_node_network_all(&self, nid: &NId) -> Vec<(NId, WireKind)> {
        let mut network = vec![];
        network.extend(
            self.get_node_network(nid, WireKind::Green)
                .iter()
                .map(|nid| (*nid, WireKind::Green)),
        );
        network.extend(
            self.get_node_network(nid, WireKind::Red)
                .iter()
                .map(|nid| (*nid, WireKind::Red)),
        );
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

        let mut inserted: HashSet<NId> = HashSet::new();

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

    // pub fn _get_output_iotype(&self, nid: &NId) -> LooseSig {
    //     let green_network_nids = self.get_node_network(nid, WireKind::Green);
    //     let red_network_nids = self.get_node_network(nid, WireKind::Red);

    //     let mut types = vec![];

    //     for to_vec in self.adjacency.values() {
    //         for (to_nid, conn) in to_vec {
    //             if green_network_nids.contains(to_nid) || red_network_nids.contains(to_nid) {
    //                 if let Connection::Combinator(com) = conn {
    //                     types.push(com.get_output_iotype())
    //                 }
    //             }
    //         }
    //     }

    //     if types.len() == 1 {
    //         types[0].clone()
    //     } else {
    //         LooseSig::Many
    //     }
    // }

    pub fn get_input_iotypes(&self, nid: &NId) -> Vec<(S, WireKind)> {
        let green_network_nids = self.get_node_network(nid, WireKind::Green);
        let red_network_nids = self.get_node_network(nid, WireKind::Red);

        let mut input_types = vec![];

        // Check for CONSTANT nodes in network
        for other_nid in &green_network_nids {
            if let Some(Node::Constant(t)) = self.get_node(other_nid) {
                input_types.push((t.clone(), WireKind::Green))
            }
        }
        for other_nid in &red_network_nids {
            if let Some(Node::Constant(t)) = self.get_node(other_nid) {
                input_types.push((t.clone(), WireKind::Red))
            }
        }

        // Check for INPUT nodes in network
        for other_id in &green_network_nids {
            if let Some(Node::InputVariable { kind, name: _ }) = self.get_node(other_id) {
                input_types.push((kind.clone(), WireKind::Green))
            }
        }
        for other_id in &red_network_nids {
            if let Some(Node::InputVariable { kind, name: _ }) = self.get_node(other_id) {
                input_types.push((kind.clone(), WireKind::Red))
            }
        }

        for to_vec in self.adjacency.values() {
            for (to_nid, conn) in to_vec {
                if green_network_nids.contains(to_nid) {
                    if let Connection::Combinator(com) = conn {
                        let input_type = com.get_output_iotype();
                        let input = (input_type, WireKind::Green);
                        if !input_types.contains(&input) {
                            input_types.push(input)
                        }
                    }
                }
                if red_network_nids.contains(to_nid) {
                    if let Connection::Combinator(com) = conn {
                        let input_type = com.get_output_iotype();
                        let input = (input_type, WireKind::Red);
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
    pub fn iter_conns(&self) -> impl Iterator<Item = (NId, NId, Connection<S>)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec
                .iter()
                .map(|(to_nid, conn)| (from_nid.to_owned(), to_nid.to_owned(), conn.clone()))
        })
    }

    pub fn iter_combinators(&self) -> impl Iterator<Item = (NId, NId, Operation<S>)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec.iter().filter_map(|(to_nid, conn)| match &conn {
                Connection::Combinator(com) => Some((*from_nid, *to_nid, com.clone())),
                Connection::Wire(_) => None,
            })
        })
    }

    pub fn _iter_wires(&self) -> impl Iterator<Item = (NId, NId, &WireKind)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec.iter().filter_map(|(to_nid, conn)| match &conn {
                Connection::Wire(wk) => Some((*from_nid, *to_nid, wk)),
                Connection::Combinator(_) => None,
            })
        })
    }

    /// Get connections pointing away from the node
    pub fn _get_from_connections(&self, nid: &NId) -> Vec<(NId, Connection<S>)> {
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
        Vec<(NId, Connection<S>)>,
        Vec<(NId, Connection<S>)>,
        Vec<Connection<S>>,
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
    pub fn get_node(&self, nid: &NId) -> Option<&Node<S>> {
        self.vertices.get(nid)
    }

    /// Get a mutable graph node by id.
    pub fn _get_mut_node(&mut self, nid: &NId) -> Option<&mut Node<S>> {
        self.vertices.get_mut(nid)
    }

    /// Add a node to the graph. Returns the new node's given id.
    pub fn push_node(&mut self, node: Node<S>) -> NId {
        let nid = self.next_nid;
        if self.vertices.insert(nid, node).is_some() {
            panic!("Could not insert node into graph")
        }
        self.next_nid += 1;
        nid
    }

    /// Override a node at a given id.
    pub fn override_node(&mut self, nid: NId, node: Node<S>) -> Option<Node<S>> {
        self.vertices.insert(nid, node)
    }

    /// Push a node of type [InputNode].
    pub fn push_input_node(&mut self, name: String, variable_type: S) -> NId {
        self.push_node(Node::InputVariable {
            kind: variable_type,
            name,
        })
    }

    /// Push a node of type [InnerNode].
    pub fn push_inner_node(&mut self) -> NId {
        self.push_node(Node::Inner)
    }

    pub fn push_output_node(&mut self, name: String, output_type: S) -> NId {
        self.push_node(Node::Output {
            kind: output_type,
            name,
        })
    }

    pub fn push_var_node(&mut self, variable_type: S, name: String) -> NId {
        self.push_node(Node::Variable {
            kind: variable_type,
            name,
        })
    }

    /// Push a connection between two nodes.
    fn push_raw_connection(&mut self, from: NId, to: NId, connection: Connection<S>) {
        self.adjacency
            .entry(from)
            .or_default()
            .push((to, connection));
    }

    /// Push a connection between two nodes.
    pub fn push_connection(&mut self, connection: Connection<S>) -> (NId, NId) {
        let from = self.push_inner_node();
        let to = self.push_inner_node();
        let adjacent_to_from = self.adjacency.entry(from).or_default();
        adjacent_to_from.push((to, connection));
        (from, to)
    }

    /// Push both a red and green wire between two nodes.
    pub fn push_wire(&mut self, n1: NId, n2: NId) {
        self.push_wire_kind(n1, n2, DEFAULT_WIRE_KIND);
    }

    /// Push a wire of a specific kind between two nodes.
    pub fn push_wire_kind(&mut self, n1: NId, n2: NId, wire_kind: WireKind) {
        let wire = Connection::Wire(wire_kind);
        self.adjacency
            .entry(n1)
            .or_default()
            .push((n2, wire.clone()));
        self.adjacency.entry(n2).or_default().push((n1, wire));
    }

    pub fn push_combinator(&mut self, com: Operation<S>) -> (NId, NId) {
        self.push_connection(Connection::Combinator(com))
    }

    pub fn push_gate_connection(&mut self, cond_type: S, gate_type: S) -> (NId, NId) {
        self.push_connection(Connection::new_gate(GateOp {
            left: cond_type,
            right: S::new_const(0),
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
    pub fn _remove_node_with_connections(&mut self, nid: &NId) {
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

    /// Get names and node ids of all nodes of type [InputNode].
    pub fn get_input_nodes(&self) -> Vec<(String, NId)> {
        let mut inputs = vec![];
        for (nid, node) in &self.vertices {
            if let Node::InputVariable { kind: _, name } = node {
                inputs.push((name.clone(), *nid));
            }
        }
        inputs
    }

    /// Get all nodes of type [OutputNode].
    pub fn get_output_nodes(&self) -> Vec<(String, NId)> {
        let mut outputs: Vec<_> = vec![];
        for (nid, node) in &self.vertices {
            if let Node::Output { kind: _, name } = node {
                outputs.push((name.clone(), *nid));
            }
        }
        outputs
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

    /// Replace an [IOType] with another throughout the whole graph. This is usefull when assigning
    /// `IOType::Any` actual factorio signals.
    fn replace_iotype(&mut self, old_type: S, new_type: &S) {
        for (_, to_vec) in self.adjacency.iter_mut() {
            for (_, conn) in to_vec {
                match conn {
                    Connection::Combinator(Operation::Arithmetic(ac)) => {
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
                    Connection::Combinator(Operation::Decider(dc)) => {
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
                    Connection::Combinator(Operation::Pick(pc)) => {
                        if pc.output == old_type {
                            pc.output = new_type.clone()
                        }
                    }
                    Connection::Combinator(Operation::Convert(pc)) => {
                        if pc.input == old_type {
                            pc.input = new_type.clone()
                        }
                        if pc.output == old_type {
                            pc.output = new_type.clone()
                        }
                    }
                    Connection::Combinator(Operation::Gate(gc)) => {
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
                    Connection::Combinator(Operation::Delay(dc)) => {
                        if dc.output == old_type {
                            dc.output = new_type.clone()
                        }
                    }
                    Connection::Combinator(Operation::Sum(sc)) => {
                        if sc.output == old_type {
                            sc.output = new_type.clone()
                        }
                    }
                    Connection::Wire(_) => {}
                }
            }
        }
    }

    pub fn get_single_input(&self, nid: &NId) -> Result<S, String> {
        let inputs = self.get_input_iotypes(nid);
        let t = match inputs.len() {
            1 => &inputs[0].0,
            2 => {
                let (t1, _w1) = &inputs[0];
                let (t2, _w2) = &inputs[1];
                assert_eq!(t1, t2);
                t1
            }
            _ => return Err(format!("Could not get single input: {inputs:?}")),
        };

        Ok(t.clone())
    }

    fn _get_final_outputs(&self) -> Vec<(NId, Node<S>)> {
        self.vertices
            .iter()
            .filter(|(nid, node)| {
                if let Node::Output { kind: _, name: _ } = node {
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
    fn get_non_constant_inputs(&self) -> Vec<(NId, Node<S>)> {
        self.vertices
            .iter()
            .filter(|(_nid, node)| {
                if let Node::InputVariable {
                    kind: input_type,
                    name: _,
                } = node
                {
                    !input_type.is_const()
                } else {
                    false
                }
            })
            .map(|(nid, node)| (*nid, node.clone()))
            .collect()
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
                Node::InputVariable { kind: _, name: _ } => "INPUT_VAR",
                Node::Variable { kind: _, name: _ } => "VAR",
                Node::Output { kind: _, name: _ } => "OUTPUT",
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

impl Graph<LooseSig> {
    pub fn assign_anysignals(&mut self) {
        AnysignalAssigner::new(self).assign();
    }

    /// Stitch another graph into this one. Resuts a vector of outputs os the newly stitched in
    /// graph.
    /// NOTE: Order of inputs vec matter. The first input is the first argument to the block.
    #[allow(clippy::single_match)]
    pub fn stitch_graph(
        &mut self,
        other: &Graph<LooseSig>,
        inputs: Vec<(NId, LooseSig)>, // Inputs to the other graph
    ) -> Result<Vec<(NId, LooseSig)>, String> {
        // Hashmap containing mappings from old to new node ids
        let mut nid_converter: fnv::FnvHashMap<NId, NId> = fnv::FnvHashMap::default();

        let mut other_graph_outputs: FnvHashMap<NId, LooseSig> = FnvHashMap::default();

        // Copy nodes from the other graph and assign them new ids.
        for (old_nid, node) in other.vertices.clone() {
            let new_node = match node {
                Node::Constant(_) => node.clone(),
                _ => Node::Inner,
            };
            let new_nid = self.push_node(new_node);
            nid_converter.insert(old_nid, new_nid);

            // Note this node as output
            if let Node::Output {
                kind: output_type,
                name: _,
            } = node
            {
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
        // TODO: Handle this with error
        if other_graph_inputs.len() != inputs.len() {
            panic!(
                "Number of arguments does not match with block definition: Expected {}, found {}. This is probably a bug in the typechecker.",
                other_graph_inputs.len(),
                inputs.len()
            );
        }

        // Iterate over the inputs of the other block
        for (i, (block_input_nid, block_input_type)) in inputs.iter().enumerate() {
            let (old_other_input_nid, other_input_node) = &other_graph_inputs[i];

            // Translate input nid to id in this graph
            let other_input_nid = nid_converter[old_other_input_nid];

            // Get the input type
            let other_input_type = match &other_input_node {
                Node::InputVariable {
                    kind: input_type,
                    name: _,
                } => input_type.clone(),
                _ => panic!("There should only be input nodes here..."),
            };

            match other_input_type {
                LooseSig::Signal(_) => {
                    // TODO: Simplity

                    // The input types for the (output) node on THIS graph, that is to be stitched
                    // together with the input of the other graph.
                    let input_types = self.get_input_iotypes(block_input_nid);

                    // TODO: There should definetly be a better way to get the input type.
                    debug_assert!(
                        input_types.len() == 1,
                        "Block inputs can only have one type. NOTE: This type may be `All`."
                    );

                    let (input_type, _wc) = input_types[0].clone();

                    // This node is the transition point from this graph to the other graph, now
                    // stitched inside this one. The middle node contains signals of the type
                    // specified in the block arguments.
                    let middle_node = self.push_node(Node::Inner);

                    self.push_raw_connection(
                        *block_input_nid,
                        middle_node,
                        Connection::new_arithmetic(ArithmeticOp::new_convert(
                            input_type,
                            block_input_type.clone(),
                        )),
                    );

                    self.push_raw_connection(
                        middle_node,
                        other_input_nid,
                        Connection::new_arithmetic(ArithmeticOp::new_convert(
                            block_input_type.clone(),
                            other_input_type,
                        )),
                    );
                }
                LooseSig::AnySignal(_) => {
                    let new_type = self.get_single_input(block_input_nid).unwrap();
                    self.replace_iotype(other_input_type, &new_type);
                    self.push_wire(*block_input_nid, other_input_nid);
                }
                LooseSig::ConstantAny(_) => {
                    todo!()
                }
                LooseSig::ConstantSignal(_) => todo!(),
                LooseSig::Constant(_) => {
                    panic!("Compiler Error: Inputs to a block should not be constants.")
                }
                LooseSig::Many => {
                    self.push_wire(*block_input_nid, other_input_nid);
                }
            }

            match self.vertices.get_mut(block_input_nid) {
                Some(Node::Output { kind: _, name: _ }) => {
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
}

struct AnysignalAssigner<'a> {
    graph: &'a mut Graph<LooseSig>,
    anysignal_to_signal: FnvHashMap<u64, String>,
    next_sig_nr: u64,
    used_signals: HashSet<String>,
    signal_names: Vec<&'static str>,
}

impl<'a> AnysignalAssigner<'a> {
    fn new(graph: &'a mut Graph<LooseSig>) -> Self {
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
                    if let LooseSig::Signal(s) = com.get_output_iotype() {
                        self.used_signals.insert(s);
                    }
                }
            }
        }

        let mut graph = self.graph.clone();

        // Convert anysignals in connections
        for to_vec in graph.adjacency.values_mut() {
            for (_, conn) in to_vec {
                if let Connection::Combinator(com) = conn {
                    match com {
                        Operation::Arithmetic(c) => {
                            self.replace_if_anysignal(&mut c.left);
                            self.replace_if_anysignal(&mut c.right);
                            self.replace_if_anysignal(&mut c.output);
                        }
                        Operation::Decider(c) => {
                            self.replace_if_anysignal(&mut c.left);
                            self.replace_if_anysignal(&mut c.right);
                            self.replace_if_anysignal(&mut c.output);
                        }
                        Operation::Pick(pc) => {
                            self.replace_if_anysignal(&mut pc.output);
                        }
                        Operation::Convert(c) => {
                            self.replace_if_anysignal(&mut c.input);
                            self.replace_if_anysignal(&mut c.output);
                        }
                        Operation::Gate(c) => {
                            self.replace_if_anysignal(&mut c.left);
                            self.replace_if_anysignal(&mut c.right);
                            self.replace_if_anysignal(&mut c.gate_type);
                        }
                        Operation::Delay(c) => {
                            self.replace_if_anysignal(&mut c.output);
                        }
                        Operation::Sum(c) => {
                            self.replace_if_anysignal(&mut c.output);
                        }
                    }
                }
            }
        }

        // Convert anysignals in nodes
        for node in graph.vertices.values_mut() {
            match node {
                Node::InputVariable { kind, name: _ } => self.replace_if_anysignal(kind),
                Node::Variable { kind, name: _ } => self.replace_if_anysignal(kind),
                Node::Output { kind, name: _ } => self.replace_if_anysignal(kind),
                Node::Constant(t) => self.replace_if_anysignal(t),
                Node::Inner => {}
            }
        }

        *self.graph = graph;
    }

    fn replace_if_anysignal(&mut self, mut iotype: &mut LooseSig) {
        match &mut iotype {
            LooseSig::AnySignal(n) => {
                let sig = match self.anysignal_to_signal.get(n) {
                    Some(s) => s.clone(),
                    None => {
                        let s = self.get_next_signal();
                        self.anysignal_to_signal.insert(*n, s.clone());
                        s
                    }
                };
                *iotype = LooseSig::Signal(sig.to_string());
            }
            LooseSig::ConstantAny((n, count)) => {
                let sig = match self.anysignal_to_signal.get(n) {
                    Some(s) => s.clone(),
                    None => {
                        let s = self.get_next_signal();
                        self.anysignal_to_signal.insert(*n, s.clone());
                        s
                    }
                };
                *iotype = LooseSig::ConstantSignal((sig.to_string(), *count))
            }
            _ => (),
        }
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
        let mut g: Graph<LooseSig> = Graph::new();
        let conn =
            Connection::Combinator(Operation::new_pick(LooseSig::Signal("test".to_string())));

        let n1 = g.push_inner_node();
        let n2 = g.push_inner_node();
        let (n3, _n4) = g.push_connection(conn.clone());
        let (n6, _n5) = g.push_connection(conn.clone());
        let (n4, n5) = g.push_connection(conn);

        // Network 1: n1 -- n2 -- n3 -- n1 (circle)
        // Network 2: n4 -- n6
        // Network 3: n5
        let wire_kind = WireKind::Green;
        g.push_wire_kind(n1, n2, wire_kind.clone());
        g.push_wire_kind(n2, n3, wire_kind.clone());
        g.push_wire_kind(n3, n1, wire_kind.clone());
        g.push_wire_kind(n4, n6, wire_kind);

        // Check network 1
        let mut network1_1 = g.get_node_network(&n1, WireKind::Green);
        let mut network1_2 = g.get_node_network(&n2, WireKind::Green);
        let mut network1_3 = g.get_node_network(&n3, WireKind::Green);
        network1_1.sort();
        network1_2.sort();
        network1_3.sort();
        assert_eq!(network1_1, vec![n1, n2, n3]);
        assert_eq!(network1_1, network1_2);
        assert_eq!(network1_1, network1_3);

        // Check network 2
        let mut network2_1 = g.get_node_network(&n4, WireKind::Green);
        let mut network2_2 = g.get_node_network(&n6, WireKind::Green);
        network2_1.sort();
        network2_2.sort();
        assert_eq!(network2_1, vec![n6, n4]);
        assert_eq!(network2_1, network2_2);

        // Check network 3
        let mut network3 = g.get_node_network(&n5, WireKind::Green);
        network3.sort();
        assert_eq!(network3, vec![n5]);
    }
}
