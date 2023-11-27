use std::{cmp, collections::HashSet};

use factorio_blueprint::objects::EntityNumber;

use crate::{
    blueprint_converter::{NetworkId, Operation},
    compiler::graph::Graph,
};

use super::{Combinator, CombinatorPosition, CoordSet, WIRE_RANGE};

pub trait Placer {
    fn place(self) -> Vec<Combinator>;
}

pub(crate) fn is_in_range(p1: &CoordSet, p2: &CoordSet) -> bool {
    let dx = (p2.0 - p1.0) as f64;
    let dy = (p2.1 - p1.1) as f64;
    let dist = f64::sqrt(dx * dx + dy * dy);
    dist <= WIRE_RANGE
}

// TODO: Create more placers

/// Tries to arrange combinators in a horrizontal line facing north.
///
/// ### Steps for finding the location of a combinator.
/// 1. Check if any input or output combinators to this one have been placed.
/// 2. If they have, choose an end of the turd and check if the inputs/outputs are in range.
/// 3. If not try the other end.
/// 4. Lastly try placing on top of the turd.
/// 5. If nothing works, backtrack. We will just panic for now... // TODO
/// 6. When placed, connect wires to the other combinators that were placed.
pub struct TurdMaster2000 {
    graph: Graph,
    placed_combinators: Vec<Combinator>,
    placed_positions: HashSet<CoordSet>,
    max_x: i64,
    min_x: i64,
    max_y: i64,
    min_y: i64,
    next_entity_number: EntityNumber,
}

impl Placer for TurdMaster2000 {
    fn place(mut self) -> Vec<Combinator> {
        let conns: Vec<(NetworkId, NetworkId, Operation)> = self.graph.iter_conns().collect();

        'next_combinator: for (input_network, output_network, operation) in conns {
            for y in self.min_y - 1..=self.max_y + 1 {
                for x in self.min_x - 1..=self.max_x + 1 {
                    if let Some(combinator) =
                        self.try_place_combinator(x, y, input_network, output_network, &operation)
                    {
                        self.placed_combinators.push(combinator);
                        continue 'next_combinator;
                    }
                }
            }
            todo!("COULD NOT PLACE COMBINATOR");
        }

        self.placed_combinators
    }
}

impl TurdMaster2000 {
    pub fn new(graph: Graph) -> Self {
        Self {
            graph,
            placed_combinators: vec![],
            placed_positions: HashSet::new(),
            max_x: 0,
            min_x: 1,
            max_y: 0,
            min_y: 1,
            next_entity_number: EntityNumber::try_from(1).unwrap(),
        }
    }

    fn coordset_is_occupied(&self, coordset: &CoordSet) -> bool {
        self.placed_positions.get(coordset).is_some()
    }

    fn try_place_combinator(
        &mut self,
        x: i64,
        y: i64,
        input_network: NetworkId,
        output_network: NetworkId,
        operation: &Operation,
    ) -> Option<Combinator> {
        let input_coord = (x, y * 2);
        let output_coord = (x, y * 2 + 1);

        if self.coordset_is_occupied(&input_coord) || self.coordset_is_occupied(&output_coord) {
            // println!(
            //     "Coordset was occupied: [{:?}, {:?}]",
            //     input_coord, output_coord
            // );
            return None;
        }

        // True if any combinators with inputs or outputs the same as this combinator has been
        // placed. If they have, we have to be able to connect to the network from the current
        // position.
        let mut input_network_exists = false;
        let mut output_network_exists = false;

        let mut input_combinator = None;
        let mut output_combinator = None;

        // TODO: This can result in skipped entity numbers. Make this that this is ok.
        let this_entity_number = self.get_next_entity_number();

        // TODOOOOOOOOOO: Handle multipule input combinators
        // Check that wire can reach the nessecery placed combinators in this position
        for com in &mut self.placed_combinators {
            if input_combinator.is_none() && com.output_network == input_network {
                input_network_exists = true;
                if is_in_range(&input_coord, &com.position.output) {
                    input_combinator = Some(com);
                }
            } else if output_combinator.is_none() && com.input_network == output_network {
                output_network_exists = true;
                if is_in_range(&output_coord, &com.position.input) {
                    output_combinator = Some(com);
                }
            }
        }

        if (input_network_exists && input_combinator.is_none())
            || (output_network_exists && output_combinator.is_none())
        {
            return None; // Could not connect to the input or output network from this position
        }

        // Update input combinator
        if let Some(com) = input_combinator {
            com.output_entities.push(this_entity_number);
        }

        let output_entities = match output_combinator {
            Some(com) => vec![com.entity_number],
            None if input_network == output_network => vec![this_entity_number], // loopback
            None => vec![],
        };

        self.placed_positions.insert(input_coord);
        self.placed_positions.insert(output_coord);
        self.max_x = cmp::max(self.max_x, x);
        self.max_y = cmp::max(self.max_y, y);

        let c = Combinator {
            entity_number: this_entity_number,
            input_network,
            output_network,
            operation: operation.clone(),
            output_entities,
            position: CombinatorPosition {
                input: input_coord,
                output: output_coord,
            },
        };
        // println!("Placing combinator: {}", &c);
        Some(c)
    }

    fn get_next_entity_number(&mut self) -> EntityNumber {
        let en = self.next_entity_number;
        self.next_entity_number = self.next_entity_number.checked_add(1).unwrap();
        en
    }

    // fn place_combinator(&self, input_loc: CoordSet, output_loc: CoordSet, com_index: usize) {
    // // Reserve space for the combinator.
    // if !self.placed_positions.insert(input_loc) || !self.placed_positions.insert(output_loc) {
    //     panic!("You cannot place a combinator on top of another.")
    // }
    // let com = self.graph.combinators.get_mut(com_index).unwrap();

    // com.position = Some(CombinatorPosition {
    //     input: input_loc,
    //     output: output_loc,
    // });
    // }

    // fn try_place_combinator(
    // &mut self,
    // input_loc: CoordSet,
    // output_loc: CoordSet,
    // com_index: usize,
    // ) -> bool {
    // // See if the position if occupied.
    // if self.placed_positions.get(&input_loc).is_some()
    //     || self.placed_positions.get(&output_loc).is_some()
    // {
    //     return false;
    // }

    // let com = self.unplaced_combinators[com_index].clone();

    // self.graph.get_inputs(vid);
    // let input_nodes = self.graph.get_other_nodes_in_wire_network(&com.input_node);
    // let output_nodes = self.graph.get_other_nodes_in_wire_network(&com.output_node);

    // // Check if the input_loc allows for connecting to the combinator inputs,
    // // and connect if so.
    // let mut any_placed = false;
    // let mut connected = false;
    // for input_nid in input_nodes {
    //     if let Some(placed_com_pos) =
    //         &self.graph.get_corresponding_combinator(input_nid).position
    //     {
    //         any_placed = true;
    //         if is_in_range(input_loc, placed_com_pos.output) {
    //             connected = true;
    //             self.graph.push_wire(input_nid, com.input_node);
    //         }
    //     }
    // }
    // // If some input combinator(s) were found, but none could be connected to, the placement
    // // position is invalid.
    // if any_placed && !connected {
    //     return false;
    // }

    // // Check if the input_loc allows for connecting to the combinator inputs,
    // // and connect if so.
    // let mut any_placed = false;
    // let mut connected = false;
    // for output_nid in output_nodes {
    //     if let Some(placed_com_pos) =
    //         &self.graph.get_corresponding_combinator(output_nid).position
    //     {
    //         any_placed = true;
    //         if is_in_range(input_loc, placed_com_pos.output) {
    //             connected = true;
    //             self.graph.push_wire(output_nid, com.output_node);
    //         }
    //     }
    // }
    // // If some input combinator(s) were found, but none could be connected to, the placement
    // // position is invalid.
    // if any_placed && !connected {
    //     return false;
    // }

    // self.place_combinator(input_loc, output_loc, com_index);

    // true
    // todo!()
    // }
}
