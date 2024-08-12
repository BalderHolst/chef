//! Implementation of [Graph]. A graph for representing a network of factorio combinators.

use fnv::FnvHashMap;
use std::fmt::Debug;
use std::{collections::HashSet, fmt::Display, usize};

use crate::diagnostics::{CompilationError, CompilationResult};

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

    fn get_constant_signal(&self) -> Option<(S, i32)>;
}

/// A determined signal
#[derive(Clone, Debug, PartialEq)]
pub enum DetSig {
    Signal(String),
    Constant(i32),
    ConstantSignal((String, i32)),
    Many,
}

#[cfg(test)]
impl DetSig {
    pub fn signal<S>(signal: S) -> Self
    where
        S: ToString,
    {
        Self::Signal(signal.to_string())
    }
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

    fn get_constant_signal(&self) -> Option<(DetSig, i32)> {
        match self {
            DetSig::ConstantSignal((sig, count)) => Some((DetSig::Signal(sig.to_string()), *count)),
            _ => None,
        }
    }
}

impl Display for DetSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DetSig::Signal(s) => write!(f, "Sig({})", s),
            DetSig::Constant(c) => write!(f, "({})", c),
            DetSig::ConstantSignal((s, c)) => write!(f, "Const({}, {})", s, c),
            DetSig::Many => write!(f, "Many"),
        }
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

    fn get_constant_signal(&self) -> Option<(LooseSig, i32)> {
        match self {
            LooseSig::ConstantSignal((sig, count)) => {
                Some((LooseSig::Signal(sig.to_string()), *count))
            }
            LooseSig::ConstantAny((n, count)) => Some((LooseSig::AnySignal(*n), *count)),
            _ => None,
        }
    }
}

impl LooseSig {
    pub fn signal<S>(signal: S) -> Self
    where
        S: ToString,
    {
        Self::Signal(signal.to_string())
    }

    // TODO: Find other way to do this, if it is even needed
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
}

#[derive(Clone, Debug, PartialEq)]
pub struct PickOp<S> {
    pub pick: S,
}

impl<S> PickOp<S> {
    pub fn new(pick: S) -> Self {
        Self { pick }
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

impl Copy for WireKind {}

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
            Self::Pick(pc) => pc.pick.clone(),
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
                format!("PICK: {}", pc.pick)
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
    Operation(Operation<S>),
}

impl<S> Connection<S>
where
    S: Clone,
{
    fn new_combinator(com: Operation<S>) -> Self {
        Self::Operation(com)
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
            Self::Operation(com) => com.to_string(),
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

        /// Used to figure out order of block inputs
        nr: usize,
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

        /// Used to figure out order of block inputs
        nr: usize,
    },

    // TODO: Maybe this should just contain the constant and not a generic iotype?
    Constant(S),
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
    next_input_nr: usize,
    next_output_nr: usize,
}

impl<S> Default for Graph<S>
where
    S: Debug + Clone + PartialEq + Display + Signal<S>,
{
    fn default() -> Self {
        Self::new()
    }
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
            next_input_nr: 0,
            next_output_nr: 0,
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

    fn get_next_input_nr(&mut self) -> usize {
        let n = self.next_input_nr;
        self.next_input_nr += 1;
        n
    }

    fn get_next_output_nr(&mut self) -> usize {
        let n = self.next_output_nr;
        self.next_output_nr += 1;
        n
    }

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
            if let Some(Node::InputVariable {
                kind,
                name: _,
                nr: _,
            }) = self.get_node(other_id)
            {
                input_types.push((kind.clone(), WireKind::Green))
            }
        }
        for other_id in &red_network_nids {
            if let Some(Node::InputVariable {
                kind,
                name: _,
                nr: _,
            }) = self.get_node(other_id)
            {
                input_types.push((kind.clone(), WireKind::Red))
            }
        }

        for to_vec in self.adjacency.values() {
            for (to_nid, conn) in to_vec {
                if green_network_nids.contains(to_nid) {
                    if let Connection::Operation(com) = conn {
                        let input_type = com.get_output_iotype();
                        let input = (input_type, WireKind::Green);
                        if !input_types.contains(&input) {
                            input_types.push(input)
                        }
                    }
                }
                if red_network_nids.contains(to_nid) {
                    if let Connection::Operation(com) = conn {
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

    /// Returns an iterator overr graph connections with the format:
    /// (from_nid, to_nid, connection).
    pub fn iter_conns_mut(&mut self) -> impl Iterator<Item = (NId, NId, &mut Connection<S>)> + '_ {
        self.adjacency.iter_mut().flat_map(|(from_nid, to_vec)| {
            to_vec
                .iter_mut()
                .map(|(to_nid, conn)| (from_nid.to_owned(), to_nid.to_owned(), conn))
        })
    }

    #[cfg(test)]
    pub fn iter_wires(&self) -> impl Iterator<Item = (NId, NId, &WireKind)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec.iter().filter_map(|(to_nid, conn)| match &conn {
                Connection::Wire(wk) => Some((*from_nid, *to_nid, wk)),
                Connection::Operation(_) => None,
            })
        })
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item = (NId, &Node<S>)> {
        self.vertices.iter().map(|(nid, node)| (*nid, node))
    }

    pub fn iter_nodes_mut(&mut self) -> impl Iterator<Item = (NId, &mut Node<S>)> {
        self.vertices.iter_mut().map(|(nid, node)| (*nid, node))
    }

    pub fn iter_ops(&self) -> impl Iterator<Item = (NId, NId, Operation<S>)> + '_ {
        self.adjacency.iter().flat_map(|(from_nid, to_vec)| {
            to_vec.iter().filter_map(|(to_nid, conn)| match &conn {
                Connection::Operation(com) => Some((*from_nid, *to_nid, com.clone())),
                Connection::Wire(_) => None,
            })
        })
    }

    pub fn iter_ops_mut(&mut self) -> impl Iterator<Item = (NId, NId, &mut Operation<S>)> + '_ {
        self.adjacency.iter_mut().flat_map(|(from_nid, to_vec)| {
            to_vec.iter_mut().filter_map(|(to_nid, conn)| match conn {
                Connection::Operation(com) => Some((*from_nid, *to_nid, com)),
                Connection::Wire(_) => None,
            })
        })
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

    /// Push a node of type [InputNode].
    pub fn push_input_node(&mut self, name: String, variable_type: S) -> NId {
        let nr = self.get_next_input_nr();
        self.push_node(Node::InputVariable {
            kind: variable_type,
            name,
            nr,
        })
    }

    /// Push a node of type [InnerNode].
    pub fn push_inner_node(&mut self) -> NId {
        self.push_node(Node::Inner)
    }

    pub fn push_output_node(&mut self, name: String, output_type: S) -> NId {
        let nr = self.get_next_output_nr();
        self.push_node(Node::Output {
            kind: output_type,
            name,
            nr,
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

    pub fn push_operation(&mut self, com: Operation<S>) -> (NId, NId) {
        self.push_connection(Connection::Operation(com))
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
    pub fn get_input_nodes(&self) -> Vec<(String, NId, &S)> {
        let mut inputs = vec![];
        for (nid, node) in &self.vertices {
            if let Node::InputVariable { kind, name, nr } = node {
                inputs.push((name.clone(), *nid, kind, *nr));
            }
        }
        inputs.sort_by(|a, b| a.3.cmp(&b.3));
        inputs
            .iter()
            .map(|(name, nid, sig, _nr)| (name.clone(), *nid, *sig))
            .collect()
    }

    /// Get all nodes of type [OutputNode].
    pub fn get_output_nodes(&self) -> Vec<(String, NId)> {
        let mut outputs: Vec<_> = vec![];
        for (nid, node) in &self.vertices {
            if let Node::Output { kind: _, name, nr } = node {
                outputs.push((name.clone(), *nid, nr));
            }
        }
        outputs.sort_by(|a, b| a.2.cmp(b.2));
        outputs
            .iter()
            .map(|(name, nid, _nr)| (name.clone(), *nid))
            .collect()
    }

    pub fn is_wire_only_node(&self, nid: NId) -> bool {
        for (from, to, conn) in self.iter_conns() {
            if nid == from || nid == to {
                if let Connection::Operation(_) = conn {
                    return false;
                }
            }
        }
        true
    }

    /// Print the graph to stout.
    pub fn print(&self) {
        println!("Graph:");
        println!("\tVertecies:");
        for (nid, node) in &self.vertices {
            if self.vertices.contains_key(nid) {
                println!("Could not find vertex: {}", nid);
                return;
            }
            let repr = match node {
                Node::Inner => "INNER",
                Node::InputVariable {
                    kind: _,
                    name: _,
                    nr: _,
                } => "INPUT_VAR",
                Node::Variable { kind: _, name: _ } => "VAR",
                Node::Output {
                    kind: _,
                    name: _,
                    nr: _,
                } => "OUTPUT",
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

    /// Stitch another graph into this one. Returns a vector of outputs os the newly stitched in
    /// graph.
    pub fn stitch_graph(
        &mut self,
        sub: &Graph<S>,              // The other graph
        main_outputs: Vec<(NId, S)>, // Inputs to the other graph
    ) -> CompilationResult<Vec<(NId, S)>> {
        // Hash map containing mappings from old to new node ids
        let mut nid_converter: fnv::FnvHashMap<NId, NId> = fnv::FnvHashMap::default();

        let mut sub_outputs = vec![];

        // Copy nodes from the other graph and assign them new ids.
        for (old_nid, node) in sub.vertices.clone() {
            let new_node = match node {
                Node::Constant(_) => node.clone(),
                _ => Node::Inner,
            };
            let new_nid = self.push_node(new_node);
            nid_converter.insert(old_nid, new_nid);

            // If the node is an output, we add its new node to the `sub_outputs` hash map.
            if let Node::Output {
                kind: output_type,
                name: _,
                nr,
            } = node
            {
                sub_outputs.push((new_nid, output_type, nr));
            }
        }

        // Sort the new outputs to make sure their order matches their definition
        sub_outputs.sort_by(|a, b| a.2.cmp(&b.2));

        // Copy connections
        for (old_from_nid, old_to_nid, conn) in sub.iter_conns() {
            let new_from_nid = nid_converter[&old_from_nid];
            let new_to_nid = nid_converter[&old_to_nid];
            self.push_raw_connection(new_from_nid, new_to_nid, conn.clone());
        }

        // TODO: The `get_input_nodes` only happens to return the inputs in the order they were
        // defined in. IDK why this works, but it does and it probably be more robust.
        let sub_graph_inputs = sub.get_input_nodes();
        let sub_graph_inputs: Vec<_> = sub_graph_inputs
            .iter()
            .map(|(_name, nid, var)| (*nid, var))
            .collect();

        // This should not error. It should have been checked by the type checker.
        // TODO: Handle this with error
        if sub_graph_inputs.len() != main_outputs.len() {
            return Err(CompilationError::new_generic(format!("Number of arguments does not match with block definition: Expected {}, found {}. This is probably a bug in the typechecker.",
                sub_graph_inputs.len(),
                main_outputs.len()
            )));
        }

        // Iterate over the connections of main to sub
        for (i, (main_output_nid, main_output_type)) in main_outputs.iter().enumerate() {
            let (old_sub_input_nid, sub_input_type) = sub_graph_inputs[i];

            // Translate input nid to nid in this graph
            let sub_input_nid = nid_converter[&old_sub_input_nid];

            // Connect the output of the main graph to the input of the sub graph
            let (trans_input, trans_output) = self.push_operation(Operation::new_convert(
                main_output_type.clone(),
                (*sub_input_type).clone(),
            ));
            self.push_wire(*main_output_nid, trans_input);
            self.push_wire(trans_output, sub_input_nid);
        }

        Ok(sub_outputs
            .iter()
            .map(|(out_nid, out_type, _nr)| (*out_nid, out_type.clone()))
            .collect())
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
        let conn = Connection::Operation(Operation::new_pick(LooseSig::Signal("test".to_string())));

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
