use fnv::FnvHashMap;
use std::fmt::Display;
use std::io;
use std::rc::Rc;

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
}

impl Display for IOType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IOType::Signal(s) => format!("int({})", s),
            IOType::AnySignal(n) => format!("Any({})", n),
            IOType::Constant(n) => format!("({})", n),
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
        Self::new(signal.clone(), IOType::Constant(0), ArithmeticOperation::ADD, signal)
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum Connection {
    Arithmetic(ArithmeticConnection),
    Wire(VId),
}

impl Connection {
    pub fn pick(signal: IOType) -> Self {
        Self::Arithmetic(ArithmeticConnection::new_pick(signal))
    }
    
    pub fn is_pick(&self) -> bool {
        if let Connection::Arithmetic(connection) = self {
            if connection.right == IOType::Constant(0) &&
                connection.operation == ArithmeticOperation::ADD &&
                    connection.output == connection.left {
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
                if self.is_pick() { format!("PICK: {}", connection.output) }
                else { format!("{}: {}, {}", connection.operation, connection.left, connection.right) }
            },
            Connection::Wire(_) => "WIRE".to_string(),
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

impl NodeInputs for Node {
    fn get_inputs(&self, graph: &Graph) -> Vec<IOType> {
        match self {
            Node::Inner(n) => n.get_inputs(graph),
            Node::Input(n) => n.get_inputs(graph),
            Node::Output(n) => n.get_inputs(graph),
        }
    }
}

pub trait NodeInputs {
    fn get_inputs(&self, graph: &Graph) -> Vec<IOType>;
}

pub trait NodeInputConnections {
    fn remove_to_connection(&mut self, conn: &Rc<Connection>);
}

#[derive(Clone, Debug)]
pub struct OutputNode {
    pub inputs: Vec<Edge>,
    pub variable_name: String,
    pub output_type: IOType,
}

impl OutputNode {
    pub fn new(variable_name: String, output_type: IOType) -> Self {
        Self { inputs: vec![], variable_name, output_type }
    } 

    fn add_input(&mut self, edge: Edge) {
        self.inputs.push(edge);
    }
}

impl NodeInputConnections for OutputNode {
    fn remove_to_connection(&mut self, conn: &Rc<Connection>) {
        for (i, input_conn) in self.inputs.iter().enumerate() {
            if input_conn == conn {
                self.inputs.remove(i);
                break;
            }
        }
    }
}

impl NodeInputs for OutputNode {
    fn get_inputs(&self, graph: &Graph) -> Vec<IOType> {
        self.inputs
            .iter()
            .map(|c| match c.as_ref().clone() {
                Connection::Arithmetic(ac) => vec![ac.output],
                Connection::Wire(vid) => graph.get_vertex(&vid).unwrap().get_inputs(graph),
            })
            .flatten().collect()
    }
}

#[derive(Clone, Debug)]
pub struct InputNode {
    pub inputs: Vec<IOType>,
    pub variable_name: String,
}

impl InputNode {
    pub fn new(variable_name: String, inputs: Vec<IOType>) -> Self {
        Self { variable_name, inputs }
    }
}

impl NodeInputs for InputNode {
    fn get_inputs(&self, _: &Graph) -> Vec<IOType> {
        self.inputs.clone()
    }
}

#[derive(Clone, Debug)]
pub struct InnerNode {
    pub inputs: Vec<Edge>,
}

impl InnerNode {
    pub fn new() -> Self {
        Self { inputs: vec![] }
    }

    fn add_input(&mut self, edge: Edge) {
        self.inputs.push(edge);
    }
}

impl NodeInputConnections for InnerNode {
    fn remove_to_connection(&mut self, conn: &Rc<Connection>) {
        for (i, input_conn) in self.inputs.iter().enumerate() {
            if input_conn == conn {
                self.inputs.remove(i);
                break;
            }
        }
    }
}


impl NodeInputs for InnerNode {
    fn get_inputs(&self, graph: &Graph) -> Vec<IOType> {
        self.inputs
            .iter()
            .map(|c| match c.as_ref().clone() {
                Connection::Arithmetic(ac) => vec![ac.output],
                Connection::Wire(vid) => graph.get_vertex(&vid).unwrap().get_inputs(graph),
            })
            .flatten().collect()
    }
}

pub type Edge = Rc<Connection>;
pub type VId = u64;

#[derive(Clone)]
pub struct Graph {
    pub vertices: FnvHashMap<VId, Node>,
    pub adjacency: FnvHashMap<VId, Vec<(VId, Edge)>>,
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

    pub fn push_input_node(&mut self, variable_name: String, inputs: Vec<IOType>) -> VId {
        self.push_node(Node::Input(InputNode::new(variable_name, inputs)))
    }

    pub fn push_inner_node(&mut self) -> VId {
        self.push_node(Node::Inner(InnerNode::new()))
    }

    pub fn push_connection(&mut self, from: VId, to: VId, connection: Connection) {
        let to_node = self
            .get_mut_vertex(&to)
            .expect(&format!("Could not access vertex: {}", to));
        match to_node {
            Node::Inner(to_vertex) => {
                to_vertex.add_input(Rc::new(connection.clone()));
            }
            Node::Output(to_vertex) => {
                to_vertex.add_input(Rc::new(connection.clone()));
            },
            Node::Input(_) => panic!("A connection cannot point to an input node, as they simply hold input IOTypes."),
        }
        let adjacent_to_from = self.adjacency.entry(from).or_default();
        adjacent_to_from.push((to, Rc::new(connection)));
    }

    pub fn remove_connection(&mut self, from: VId, to: VId, connection: &Rc<Connection>) {
        let to_node = self
            .get_mut_vertex(&to)
            .expect(&format!("Could not access vertex: {}", to));
        match to_node {
            Node::Inner(to_vertex) => {
                for (i, input_conn) in to_vertex.inputs.iter().enumerate() {
                    if input_conn == connection {
                        to_vertex.inputs.remove(i);
                        break;
                    }
                }
            }
            Node::Output(to_vertex) => {
                for (i, input_conn) in to_vertex.inputs.iter().enumerate() {
                    if input_conn == connection {
                        to_vertex.inputs.remove(i);
                        break;
                    }
                }
            },
            Node::Input(_) => panic!("A connection cannot point to an input node, as they simply hold input IOTypes."),
        }

        let from_vertex_connections = self.adjacency.get_mut(&from).unwrap();
        for (i, (_, from_conn)) in from_vertex_connections.iter().enumerate() {
            if from_conn == connection {
                from_vertex_connections.remove(i);
                break;
            }
        }
    }

    pub fn remove_node_with_connections(&mut self, vid: &VId) {
        for (to_vid, conn) in &self.adjacency[vid] {
            match self.vertices.get_mut(to_vid).unwrap() {
                Node::Inner(to_node) => to_node.remove_to_connection(conn),
                Node::Output(to_node) => to_node.remove_to_connection(conn),
                Node::Input(_) => panic!("This should not be posible."),
            }
        }
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
        };
        inputs
    }


    pub fn print(&self) {
        println!("Graph:");
        println!("\tVertecies:");
        for (k, v) in &self.vertices {
            match v {
                Node::Inner(n) => println!("\t\t{} : INNER : {:?}", k, n.get_inputs(&self)),
                Node::Input(n) => println!("\t\t{} : INPUT({}) : {:?}", k, n.variable_name, n.get_inputs(&self)),
                Node::Output(n) => println!("\t\t{} : OUTPUT({}) : {:?}", k, n.variable_name, n.get_inputs(&self)),
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
