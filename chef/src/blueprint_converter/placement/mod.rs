use std::collections::HashSet;

use factorio_blueprint::Container;

use super::blueprint_graph::BlueprintGraph;

type Coord = i64;

struct Placer<'a> {
    graph: &'a mut BlueprintGraph,
    placed_positions: HashSet<(Coord, Coord)>,
}

impl<'a> Placer<'a> {
    fn new(graph: &'a mut BlueprintGraph) -> Self {
        Self {
            graph,
            placed_positions: HashSet::new(),
        }
    }

    fn place(&mut self) {}
}

pub fn place_combinators(bp_graph: &mut BlueprintGraph) {
    Placer::new(bp_graph).place();
}
