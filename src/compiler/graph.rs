use fnv::FnvHashMap;
use std::fmt::Display;
use std::io;

use crate::ast::{Variable, VariableType};

use super::graph_visualizer::GraphVisualizer;

#[derive(Clone, Debug, PartialEq)]
pub enum ArithmeticOperation {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
}

impl Display for ArithmeticOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ArithmeticOperation::ADD => "ADD",
            ArithmeticOperation::SUBTRACT => "SUBTRACT",
            ArithmeticOperation::MULTIPLY => "MULTIPLY",
            ArithmeticOperation::DIVIDE => "DIVIDE",
        };
        write!(f, "{s}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IOType {
    Signal(String),
    AnySignal(u64),
    Constant(u32),
    All,
}

impl Display for IOType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IOType::Signal(s) => format!("int({})", s),
            IOType::AnySignal(n) => format!("Any({})", n),
            IOType::Constant(n) => format!("({})", n),
            IOType::All => format!("ALL"),
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
            ArithmeticOperation::ADD,
            signal,
        )
    }

    pub fn new_convert(in_signal: IOType, out_signal: IOType) -> Self {
        Self::new(
            in_signal,
            IOType::Constant(0),
            ArithmeticOperation::ADD,
            out_signal,
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Connection {
    Arithmetic(ArithmeticConnection),
}

impl Connection {
    pub fn pick(signal: IOType) -> Self {
        Self::Arithmetic(ArithmeticConnection::new_pick(signal))
    }

    pub fn get_output(&self) -> Vec<IOType> {
        match self {
            Connection::Arithmetic(ac) => {
                vec![ac.output.clone()]
            },
        }
    }

    pub fn is_pick(&self) -> bool {
        if let Connection::Arithmetic(connection) = self {
            if connection.right == IOType::Constant(0)
                && connection.operation == ArithmeticOperation::ADD
                && connection.output == connection.left
            {
                return true;
            }
        }
        false
    }
}

impl Display for Connection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Connection::Arithmetic(connection) => {
                if self.is_pick() {
                    format!("PICK: {}", connection.output)
                } else {
                    format!(
                        "{}: {}, {}",
                        connection.operation, connection.left, connection.right
                    )
                }
            }
        };
        writeln!(f, "{s}")
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
    pub variable_name: String,
    pub output_type: IOType,
}

impl OutputNode {
    pub fn new(variable_name: String, output_type: IOType) -> Self {
        Self {
            variable_name,
            output_type,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InputNode {
    pub input: IOType,
    pub variable_name: String,
}

impl InputNode {
    pub fn new(variable_name: String, input: IOType) -> Self {
        Self {
            variable_name,
            input,
        }
    }
}

#[derive(Clone, Debug)]
pub struct InnerNode {}

impl InnerNode {
    pub fn new() -> Self {
        Self {}
    }
}

pub type VId = u64;

#[derive(Clone)]
pub struct Graph {
    pub vertices: FnvHashMap<VId, Node>,
    pub adjacency: FnvHashMap<VId, Vec<(VId, Connection)>>,
    next_vid: VId,
}

impl Graph {
    pub fn new() -> Graph {
        Graph {
            vertices: FnvHashMap::default(),
            adjacency: FnvHashMap::default(),
            next_vid: 0,
        }
    }

    pub fn get_inputs(&self, vid: &VId) -> Vec<IOType> {
        match self.vertices.get(&vid) {
            Some(Node::Input(input_node)) => {
                return vec![input_node.input.clone()];
            }
            None => {
                return vec![];
            }
            Some(_) => {}, // Continue
        };
        let mut inputs: Vec<IOType> = vec![];

        for (_from_vid, to_vec) in &self.adjacency {
            for (to_vid, conn) in to_vec {
                if to_vid == vid {
                    for output in conn.get_output() {
                        inputs.push(output)
                    }
                }
            }
        }
        inputs
    }

    pub fn get_vertex(&self, vid: &VId) -> Option<&Node> {
        self.vertices.get(vid)
    }

    pub fn get_mut_vertex(&mut self, vid: &VId) -> Option<&mut Node> {
        self.vertices.get_mut(vid)
    }

    pub fn push_node(&mut self, node: Node) -> VId {
        let vid = self.next_vid;
        if self.vertices.insert(vid, node).is_some() {
            panic!("Could not insert node into graph")
        }
        self.next_vid += 1;
        vid
    }

    pub fn override_node(&mut self, vid: VId, node: Node) -> Option<Node> {
        self.vertices.insert(vid, node)
    }

    pub fn push_input_node(&mut self, variable_name: String, input: IOType) -> VId {
        self.push_node(Node::Input(InputNode::new(variable_name, input)))
    }

    pub fn push_inner_node(&mut self) -> VId {
        self.push_node(Node::Inner(InnerNode::new()))
    }

    pub fn push_connection(&mut self, from: VId, to: VId, connection: Connection) {
        let adjacent_to_from = self.adjacency.entry(from).or_default();
        adjacent_to_from.push((to, connection));
    }

    pub fn remove_connection(&mut self, from: &VId, to: &VId) {
        let from_vertex_connections = self.adjacency.get_mut(from).unwrap();
        for (i, (to_vid, _from_conn)) in from_vertex_connections.iter().enumerate() {
            if to_vid == to {
                from_vertex_connections.remove(i);
                break;
            }
        }
    }

    pub fn remove_node_with_connections(&mut self, vid: &VId) {
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

    // pub fn push_undirected_edge(
    //     &mut self,
    //     from: VId,
    //     to: VId,
    //     edge: Edge,
    //     ) {
    //     self.push_connection(from.clone(), to.clone(), edge.clone());
    //     self.push_connection(from, to, edge);
    // }

    pub fn get_input_nodes(&self) -> Vec<VId> {
        let mut inputs: Vec<VId> = vec![];
        for (vid, node) in &self.vertices {
            if let Node::Input(_) = node {
                inputs.push(vid.clone());
            }
        }
        inputs
    }

    pub fn print(&self) {
        println!("Graph:");
        println!("\tVertecies:");
        for (vid, node) in &self.vertices {
            if self.vertices.get(&vid).is_none() {
                println!("Could not find vertex: {}", vid);
                return;
            }
            match node {
                Node::Inner(_) => println!("\t\t{} : INNER : {:?}", vid, self.get_inputs(vid)),
                Node::Input(n) => println!(
                    "\t\t{} : INPUT({}) : {:?}",
                    vid,
                    n.variable_name,
                    self.get_inputs(vid)
                ),
                Node::Output(n) => println!(
                    "\t\t{} : OUTPUT({}) : {:?}",
                    vid,
                    n.variable_name,
                    self.get_inputs(vid)
                ),
            }
        }
        println!("\n\tConnections:");
        for (vid, to) in &self.adjacency {
            for (k, v) in to {
                println!("\t\t{} -> {} : {:?}", vid, k, v);
            }
        }
    }

    pub fn visualize(&self, output_path: &str) -> io::Result<()> {
        GraphVisualizer::new(self).visualize(output_path)
    }
}
