use std::collections::HashSet;

use factorio_blueprint::{objects::EntityNumber, Container};

use super::blueprint_graph::{BlueprintGraph, Combinator};

trait Placer {
    fn place(&mut self) {}
}

type Coord = i64;
type CoordSet = (Coord, Coord);

/// Tries to arrange combinators in a horrizontal line facing north.
///
/// ### Steps for finding the location of a combinator.
/// 1. Check if any input or output combinators to this one have been placed.
/// 2. If they have, choose an end of the turd and check if the inputs/outputs are in range.
/// 3. If not try the other end.
/// 4. Lastly try placing on top of turn.
/// 5. If nothing works, backtrack. We will just panic for now... // TODO
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

        print!("{:?} -> ", locs);

        // Sum up positions
        for (loc_x, loc_y) in locs {
            x += loc_x;
            y += loc_y;
        }

        // Divide by number of position to get the average
        let x = x as f64 / len as f64;
        let y = y as f64 / len as f64;

        println!("{:?}", (x, y));

        (x, y)
    }

    fn next_right_pos(&self) -> i64 {
        let mut i: i64 = 0;
        while self.placed_positions.get(&(i, 0)).is_some() {
            i += 1;
        }
        i
    }

    fn place_combinator(&mut self, locs: Vec<CoordSet>, com_index: usize) {
        // Reserve space for the combinator.
        for loc in locs.clone() {
            if !self.placed_positions.insert(loc) {
                panic!("You cannot place a combinator on top of another.")
            }
        }
        let com = self.graph.combinators.get_mut(com_index).unwrap();

        com.position = Some(Self::pos_avg(locs));
    }
}

impl<'a> Placer for TurdMaster2000<'a> {
    fn place(&mut self) {
        for com_index in 0..self.graph.combinators.len() {
            // let connected_and_placed = self.graph.get_placed_connected_combinators(com.entity_number);

            let x = self.next_right_pos().clone();
            self.place_combinator(vec![(x, 0), (x, 1)], com_index)
        }
    }
}

pub fn place_combinators(bp_graph: &mut BlueprintGraph) {
    TurdMaster2000::new(bp_graph).place();
}
