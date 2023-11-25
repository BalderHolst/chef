//! Converted combinator graphs to factorio blueprints.

mod placement;

use std::fmt::Display;

use factorio_blueprint as fb;
use fb::{
    objects::{EntityNumber, Position},
    Container,
};
use noisy_float::types::R64;

use crate::compiler::graph::{self, Connection, Graph};
use placement::Placer;

use self::{graph::NId, placement::TurdMaster2000};

type NetworkId = NId;
type Operation = Connection;

pub(crate) type Coord = i64;

/// 2D Coordinate inside the Factorio world
pub(crate) type CoordSet = (Coord, Coord);

#[derive(Clone, Debug, PartialEq)]
pub struct CombinatorPosition {
    pub input: CoordSet,
    pub output: CoordSet,
}

impl CombinatorPosition {
    pub fn factorio_pos(&self) -> Position {
        let (x1, y1) = self.input;
        let (x2, y2) = self.output;

        let x = (x1 + x2) as f64 / 2.0;
        let y = (y1 + y2) as f64 / 2.0;

        let x = R64::new(x);
        let y = R64::new(y);

        Position { x, y }
    }
}

impl Display for CombinatorPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (x1, y1) = self.input;
        let (x2, y2) = self.output;
        write!(
            f,
            "{{({}, {}) -> ({}, {})}}",
            x1, y1, x2, y2
        )
    }
}

/// Placed Factorio Combinator
#[derive(Clone, Debug, PartialEq)]
pub struct Combinator {
    pub entity_number: EntityNumber,
    pub input_network: NetworkId,
    pub output_network: NetworkId,
    pub operation: Operation,
    pub output_entities: Vec<EntityNumber>,
    pub position: CombinatorPosition,
}

impl Display for Combinator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {} (input for: {:?}): [{}] {} {{{}}}",
            self.input_network,
            self.output_network,
            self.output_entities,
            self.entity_number,
            self.operation,
            self.position
        )
    }
}

/// The maxinum distanct a wire can connect two points in factorio.
const WIRE_RANGE: f64 = 9.0;

pub fn convert_to_graph_to_blueprint_string(graph: Graph) -> fb::Result<String> {
    graph.print();
    println!();
    let combinators = place_combinators(TurdMaster2000::new(graph));
    println!("\nCombinators:");
    for c in &combinators {
        println!("\t{c}");
    }
    let container = combinators_to_blueprint(combinators);
    fb::BlueprintCodec::encode_string(&container)
}

fn place_combinators(placer: impl Placer) -> Vec<Combinator> {
    placer.place()
}

fn combinators_to_blueprint(combinators: Vec<Combinator>) -> Container {
    todo!()
}
