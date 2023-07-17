use std::{cmp, collections::HashSet};

use super::{
    blueprint_graph::{BlueprintGraph, CombinatorPosition},
    WIRE_RANGE,
};

trait Placer {
    fn place(&mut self) {}
}

type Coord = i64;
type CoordSet = (Coord, Coord);

pub(crate) fn is_in_range(p1: CoordSet, p2: CoordSet) -> bool {
    let dx = (p2.0 - p1.0) as f64;
    let dy = (p2.1 - p1.1) as f64;
    let dist = f64::sqrt(dx * dx + dy * dy);
    dist <= WIRE_RANGE
}

/// Tries to arrange combinators in a horrizontal line facing north.
///
/// ### Steps for finding the location of a combinator.
/// 1. Check if any input or output combinators to this one have been placed.
/// 2. If they have, choose an end of the turd and check if the inputs/outputs are in range.
/// 3. If not try the other end.
/// 4. Lastly try placing on top of the turd.
/// 5. If nothing works, backtrack. We will just panic for now... // TODO
/// 6. When placed, connect wires to the other combinators that were placed.
struct TurdMaster2000<'a> {
    graph: &'a mut BlueprintGraph,
    placed_positions: HashSet<CoordSet>,
    max_x: i64,
    min_x: i64,
    max_y: i64,
    min_y: i64,
}

impl<'a> TurdMaster2000<'a> {
    fn new(graph: &'a mut BlueprintGraph) -> Self {
        Self {
            graph,
            placed_positions: HashSet::new(),
            max_x: 0,
            min_x: 1,
            max_y: 0,
            min_y: 1,
        }
    }

    fn place_combinator(&mut self, input_loc: CoordSet, output_loc: CoordSet, com_index: usize) {
        // Reserve space for the combinator.
        if !self.placed_positions.insert(input_loc) || !self.placed_positions.insert(output_loc) {
            panic!("You cannot place a combinator on top of another.")
        }
        let com = self.graph.combinators.get_mut(com_index).unwrap();

        com.position = Some(CombinatorPosition {
            input: input_loc,
            output: output_loc,
        });
    }

    fn try_place_combinator(
        &mut self,
        input_loc: CoordSet,
        output_loc: CoordSet,
        com_index: usize,
    ) -> bool {
        // See if the position if occupied.
        if self.placed_positions.get(&input_loc).is_some()
            || self.placed_positions.get(&output_loc).is_some()
        {
            return false;
        }

        let com = self.graph.combinators[com_index].clone();

        let input_nodes = self.graph.get_other_nodes_in_wire_network(&com.from);
        let output_nodes = self.graph.get_other_nodes_in_wire_network(&com.to);

        // Check if the input_loc allows for connecting to the combinator inputs,
        // and connect if so.
        let mut any_placed = false;
        let mut connected = false;
        for input_nid in input_nodes {
            if let Some(placed_com_pos) =
                &self.graph.get_corresponding_combinator(input_nid).position
            {
                any_placed = true;
                if is_in_range(input_loc, placed_com_pos.output) {
                    connected = true;
                    self.graph.push_wire(input_nid, com.from);
                }
            }
        }
        // If some input combinator(s) were found, but none could be connected to, the placement
        // position is invalid.
        if any_placed && !connected {
            return false;
        }

        // Check if the input_loc allows for connecting to the combinator inputs,
        // and connect if so.
        let mut any_placed = false;
        let mut connected = false;
        for output_nid in output_nodes {
            if let Some(placed_com_pos) =
                &self.graph.get_corresponding_combinator(output_nid).position
            {
                any_placed = true;
                if is_in_range(input_loc, placed_com_pos.output) {
                    connected = true;
                    self.graph.push_wire(output_nid, com.to);
                }
            }
        }
        // If some input combinator(s) were found, but none could be connected to, the placement
        // position is invalid.
        if any_placed && !connected {
            return false;
        }

        self.place_combinator(input_loc, output_loc, com_index);

        true
    }
}

impl<'a> Placer for TurdMaster2000<'a> {
    fn place(&mut self) {
        'outer: for com_index in 0..self.graph.combinators.len() {
            // try to place at the end of turd
            for y in self.min_y - 1..=self.max_y + 1 {
                for x in self.min_x - 1..=self.max_x + 1 {
                    if self.try_place_combinator((x, y * 2), (x, y * 2 + 1), com_index) {
                        self.max_x = cmp::max(self.max_x, x);
                        self.max_y = cmp::max(self.max_y, y);
                        continue 'outer;
                    }
                }
            }
            todo!("COULD NOT PLACE COMBINATOR");
        }
    }
}

pub fn place_combinators(bp_graph: &mut BlueprintGraph) {
    TurdMaster2000::new(bp_graph).place();
}
