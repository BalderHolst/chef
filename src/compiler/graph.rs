use fnv::FnvHashMap;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum ArithmeticOperation {
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
}

#[derive(Clone, Debug)]
pub enum IOType {
    Signal(String),
    AnySignal(u64),
    Constant(u32),
}

#[derive(Clone, Debug)]
pub struct ArithmeticConnection {
    left: IOType,
    right: IOType,
    operation: ArithmeticOperation,
    output: IOType,
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

    pub fn new_pick(signal: &str) -> Self {
        let s = IOType::Signal(signal.to_string());
        Self::new(s.clone(), IOType::Constant(0), ArithmeticOperation::ADD, s)
    }
}

#[derive(Clone, Debug)]
pub enum Connection {
    Arithmetic(ArithmeticConnection),
}

impl Connection {
    pub fn pick(signal: &str) -> Self {
        Self::Arithmetic(ArithmeticConnection::new_pick(signal))
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Inner(InnerNode),
    Input(InputNode),
    Output(OutputNode),
}

trait NodeInputs {
    fn get_inputs(&self) -> Vec<IOType>;
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
}

impl NodeInputs for OutputNode {
    fn get_inputs(&self) -> Vec<IOType> {
        self.inputs
            .iter()
            .map(|c| match c.as_ref().clone() {
                Connection::Arithmetic(ac) => ac.output,
            })
            .collect()
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
    fn get_inputs(&self) -> Vec<IOType> {
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

impl NodeInputs for InnerNode {
    fn get_inputs(&self) -> Vec<IOType> {
        self.inputs
            .iter()
            .map(|c| match c.as_ref().clone() {
                Connection::Arithmetic(ac) => ac.output,
            })
            .collect()
    }
}

pub type Edge = Rc<Connection>;
pub type VId = u64;

pub struct Graph {
    vertices: FnvHashMap<VId, Node>,
    adjacency: FnvHashMap<VId, Vec<(VId, Edge)>>,
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
                let adjacent_to_from = self.adjacency.entry(from).or_default();
                adjacent_to_from.push((to, Rc::new(connection)));
            }
            Node::Output(_) => todo!(),
            Node::Input(_) => panic!("A connection cannot point to an input node, as they simply hold input IOTypes"),
        }
    }


        // let input = self.get_mut_vertex(&input_vid).expect("output_vid is not valid");
        // let new_input_node = if let Node::Output(input_node) = input {
        //     Node::Inner(InnerNode::new())
        // }
        // else { panic!("Incorrect input VId supplied.") };
    // }

    // pub fn push_undirected_edge(
    //     &mut self,
    //     from: VId,
    //     to: VId,
    //     edge: Edge,
    //     ) {
    //     self.push_connection(from.clone(), to.clone(), edge.clone());
    //     self.push_connection(from, to, edge);
    // }

    pub fn print(&self) {
        println!("Graph:");
        println!("\tVertecies:");
        for (k, v) in &self.vertices {
            match v {
                Node::Inner(n) => println!("\t\t{} : INNER : {:?}", k, n.get_inputs()),
                Node::Input(n) => println!("\t\t{} : INPUT({}) : {:?}", k, n.variable_name, n.get_inputs()),
                Node::Output(n) => println!("\t\t{} : OUTPUT({}) : {:?}", k, n.variable_name, n.get_inputs()),
            }
        }
        println!("\n\tConnections:");
        for (vid, to) in &self.adjacency {
            for (k, v) in to {
                println!("\t\t{} -> {} : {:?}", vid, k, v);
            }
        }
    }
}
