use std::{
    fs::OpenOptions,
    io::{self, Write},
    rc::Rc,
};

use fnv::FnvHashMap;

use super::graph::{Connection, Graph, Node, VId};

const NODE_RADIUS: usize = 20;
const NODE_PADDING: usize = 30;
const NODE_SPACE: usize = NODE_RADIUS * 2 + NODE_PADDING * 2;
const NODE_HORIZONTAL_SPACE: usize = NODE_SPACE * 3;
const ARROW_SPACE: usize = 9;

pub struct GraphVisualizer<'a> {
    graph: &'a Graph,
    svg: String,
    drawn_nodes: FnvHashMap<u64, (usize, usize)>,
    levels: Vec<usize>,
    width: usize,
    height: usize,
}

impl<'a> GraphVisualizer<'a> {
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            svg: "".to_string(),
            drawn_nodes: FnvHashMap::default(),
            levels: vec![],
            width: 0,
            height: 0,
        }
    }

    pub fn visualize(&mut self, output_path: &str) -> io::Result<()> {
        // Arrow heads
        self.svg += "<defs> <marker id=\"arrow-head\" viewBox=\"0 0 10 10\" refX=\"1\" refY=\"5\" markerUnits=\"strokeWidth\" markerWidth=\"8\" markerHeight=\"8\" orient=\"auto\"> <path d=\"M 0 0 L 10 5 L 0 10 z\" stroke=\"#000\" fill=\"none\" /> </marker> </defs>\n";

        for vid in self.graph.get_input_nodes() {
            self.visualize_node_network(vid, 0);
        }

        self.width += NODE_SPACE / 2;
        self.height += NODE_SPACE / 2;
        dbg!(&self.width, &self.height);

        self.svg = format!(
            "<svg width=\"{}\" height=\"{}\">\n",
            self.width, self.height
        ) + self.svg.as_str();
        self.svg += "</svg>";

        OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(output_path)?
            .write_all(self.svg.as_bytes())?;
        Ok(())
    }

    fn visualize_node_network(&mut self, vid: VId, level: usize) -> (usize, usize) {
        if let Some((x, y)) = self.drawn_nodes.get(&vid) {
            return (x.clone(), y.clone());
        }

        let (x, y) = self.draw_node(vid, level);
        self.drawn_nodes.insert(vid, (x, y));

        if let Some(connection_pairs) = self.graph.adjacency.get(&vid) {
            for (to_vid, connection) in connection_pairs {
                let to_pos = self.visualize_node_network(to_vid.clone(), level + 1);
                self.draw_connection(connection, (x, y), to_pos);
            }
        }

        (x, y)
    }

    fn draw_connection(
        &mut self,
        connection: &Connection,
        from: (usize, usize),
        to: (usize, usize),
    ) {
        let from_x = from.0;
        let from_y = from.1;
        let to_x = to.0;
        let to_y = to.1;

        let l1 = from_x as f32 - to_x as f32;
        let l2 = from_y as f32 - to_y as f32;

        let angle = l1.atan2(l2);

        let x_offset = angle.sin() * (NODE_RADIUS as f32);
        let y_offset = angle.cos() * (NODE_RADIUS as f32);

        let x_head_offset = angle.sin() * (ARROW_SPACE as f32);
        let y_head_offset = angle.cos() * (ARROW_SPACE as f32);

        self.svg += "<g>\n";
        self.svg += &format!("\t<line id=\"a1\" x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" stroke=\"black\" marker-end=\"url(#arrow-head)\"/>\n",
                             from_x as isize - x_offset as isize,
                             from_y as isize - y_offset as isize,
                             to_x as isize + (x_offset + x_head_offset) as isize,
                             to_y as isize + (y_offset + y_head_offset) as isize,
                             );
        self.svg += &format!("\t<text x=\"{}\" y=\"{}\" alignment-baseline=\"middle\" text-anchor=\"middle\" stroke-width=\"1px\" dy=\"-.3em\" >{}</text>\n",
                             from_x as isize - (l1/2.0) as isize,
                             from_y as isize  - (l2/2.0) as isize,
                             connection,
                            );
        self.svg += "</g>\n";
    }

    fn draw_node(&mut self, vid: VId, x_level: usize) -> (usize, usize) {
        let y_level = if x_level >= self.levels.len() {
            self.levels.push(0);
            0
        }
        else {
            self.levels[x_level] += 1;
            self.levels[x_level]
        };

        let y = y_level * (NODE_SPACE + NODE_PADDING) + NODE_SPACE / 2;
        let x = x_level * NODE_HORIZONTAL_SPACE + NODE_SPACE / 2;

        if y > self.height { self.height = y }
        if x > self.width { self.width = x }

        let inputs = self.graph.get_inputs(&vid);

        let mut types: String = inputs.iter().map(|input| format!("{} | ", input)).collect();
        if types.len() >= 3 {
            types = types.get(..types.len() - 3).unwrap().to_string();
        }

        let fill = match self.graph.get_vertex(&vid) {
            Some(Node::Inner(_)) => "none",
            Some(Node::Input(_)) => "lightgreen",
            Some(Node::Output(_)) => "orange",
            None => "red",
        };

        self.svg += "<g>\n";
        self.svg += &format!(
            "\t<circle cx=\"{}\" cy=\"{}\" r=\"{}\" fill=\"{}\" stroke=\"black\" />\n",
            x, y, NODE_RADIUS, fill
        );
        self.svg += &format!("\t<text x=\"{}\" y=\"{}\" alignment-baseline=\"middle\" text-anchor=\"middle\" stroke=\"black\" stroke-width=\"1px\" dy=\".3em\" >{}</text>\n",
                             x, y, vid);
        self.svg += &format!("\t<text x=\"{}\" y=\"{}\" fill=\"blue\" alignment-baseline=\"middle\" text-anchor=\"middle\" stroke-width=\"1\" dy=\"-.5em\" >{}</text>\n",
                             x, y - NODE_RADIUS, types);
        self.svg += "</g>\n";

        (x, y)
    }
}
