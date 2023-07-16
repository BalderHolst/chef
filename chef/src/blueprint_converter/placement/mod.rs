use std::collections::HashSet;

use factorio_blueprint::{objects::EntityNumber, Container};

use super::{
    blueprint_graph::{BlueprintGraph, Combinator, CombinatorPosition},
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
}

impl<'a> TurdMaster2000<'a> {
    fn new(graph: &'a mut BlueprintGraph) -> Self {
        Self {
            graph,
            placed_positions: HashSet::new(),
        }
    }

    fn pos_avg(locs: Vec<CoordSet>) -> (f64, f64) {
        debug_assert!(!locs.is_empty());
        let mut x = 0;
        let mut y = 0;
        let len = locs.len();

        // Sum up positions
        for (loc_x, loc_y) in locs {
            x += loc_x;
            y += loc_y;
        }

        // Divide by number of position to get the average
        let x = x as f64 / len as f64;
        let y = y as f64 / len as f64;

        (x, y)
    }

    fn next_right_pos(&self) -> i64 {
        let mut i: i64 = 0;
        while self.placed_positions.get(&(i, 0)).is_some() {
            i += 1;
        }
        i
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
            println!("COULD NOT CONNECT INPUT: {:?}", input_loc);
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
            println!("COULD NOT CONNECT INPUT: {:?}", input_loc);
            return false;
        }

        // for output_nid in output_nodes {
        //     if self
        //         .graph
        //         .get_corresponding_combinator(output_nid)
        //         .position
        //         .is_some()
        //     {
        //         self.graph.push_wire(output_nid, com.to);
        //     }
        // }

        self.place_combinator(input_loc, output_loc, com_index);

        true
    }
}

impl<'a> Placer for TurdMaster2000<'a> {
    fn place(&mut self) {
        for com_index in 0..self.graph.combinators.len() {
            let x = self.next_right_pos().clone();

            // Input always down, and output always up.
            self.try_place_combinator((x, 0), (x, 1), com_index);
        }
    }
}

pub fn place_combinators(bp_graph: &mut BlueprintGraph) {
    TurdMaster2000::new(bp_graph).place();
}
