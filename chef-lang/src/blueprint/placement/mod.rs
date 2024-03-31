use std::{
    cmp,
    collections::{HashMap, HashSet},
};

use factorio_blueprint::objects::EntityNumber;

use crate::{
    blueprint::ConnectionPoint,
    compiler::graph::{Graph, NId, Operation, WireKind},
};

use super::{CombinatorPosition, ConnectionPointType, CoordSet, FactorioCombinator, WIRE_RANGE};

pub trait Placer {
    fn place(self) -> Vec<FactorioCombinator>;
}

pub(crate) fn is_in_range(p1: &CoordSet, p2: &CoordSet) -> bool {
    let dx = (p2.0 - p1.0) as f64;
    let dy = (p2.1 - p1.1) as f64;
    let dist = f64::sqrt(dx * dx + dy * dy);
    dist <= WIRE_RANGE
}

#[test]
fn test_is_in_range() {
    assert!(is_in_range(&(0, 0), &(0, WIRE_RANGE as i64)));
    assert!(is_in_range(&(0, 0), &(WIRE_RANGE as i64, 0)));
    assert!(!is_in_range(&(0, 0), &(WIRE_RANGE as i64, 1)));
    assert!(!is_in_range(&(0, 0), &(WIRE_RANGE as i64, -1)));
    assert!(is_in_range(&(0, WIRE_RANGE as i64), &(0, 0)));
    assert!(is_in_range(&(WIRE_RANGE as i64, 0), &(0, 0)));
    assert!(!is_in_range(&(WIRE_RANGE as i64, 1), &(0, 0)));
    assert!(!is_in_range(&(WIRE_RANGE as i64, -1), &(0, 0)));
}

// TODO: Create more placers

/// Tries to arrange combinators in a horrizontal line facing north.
///
/// ### Steps for finding the location of a combinator.
/// 1. Check if any input or output combinators to this one have been placed.
/// 2. If they have, choose an end of the turd and check if the inputs/outputs are in range.
/// 3. If not try the other end.
/// 4. Lastly try placing on top of the turd.
/// 5. If nothing works, backtrack. We will just panic for now...
/// 6. When placed, connect wires to the other combinators that were placed.
pub struct TurdMaster2000 {
    graph: Graph,
    placed_combinators: HashMap<EntityNumber, FactorioCombinator>,
    placed_positions: HashSet<CoordSet>,
    max_x: i64,
    min_x: i64,
    max_y: i64,
    min_y: i64,
    next_entity_number: EntityNumber,
}

impl TurdMaster2000 {
    pub fn new(graph: Graph) -> Self {
        Self {
            graph,
            placed_combinators: HashMap::new(),
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
        input_nid: NId,
        output_nid: NId,
        operation: &Operation,
    ) -> Option<FactorioCombinator> {
        let input_coord = (x, y * 2);
        let output_coord = (x, y * 2 + 1);

        if self.coordset_is_occupied(&input_coord) || self.coordset_is_occupied(&output_coord) {
            return None;
        }

        let input_network_red = self.graph.get_node_network(&input_nid, WireKind::Red);
        let output_network_red = self.graph.get_node_network(&output_nid, WireKind::Red);
        let input_network_green = self.graph.get_node_network(&input_nid, WireKind::Green);
        let output_network_green = self.graph.get_node_network(&output_nid, WireKind::Green);

        // `true` if any combinators with inputs or outputs the same as this combinator has been
        // placed. If they have, we have to be able to connect to the network from the current
        // position.
        let mut input_network_exists = false;
        let mut output_network_exists = false;

        let mut input_combinators = vec![];
        let mut output_combinators = vec![];

        // TODO: This can result in skipped entity numbers. Make this that this is ok.
        let this_entity_number = self.get_next_entity_number();

        // Check that wire can reach the necessary placed combinators in this position
        for other in &mut self.placed_combinators.values_mut() {
            if input_network_red.contains(&other.output_nid) {
                input_network_exists = true;
                if is_in_range(&input_coord, &other.position.output) {
                    input_combinators.push(other);
                }
            } else if output_network_red.contains(&other.input_nid) {
                output_network_exists = true;
                if is_in_range(&output_coord, &other.position.input) {
                    output_combinators.push((other, ConnectionPointType::Input));
                }
            } else if output_network_red.contains(&other.output_nid) {
                output_network_exists = true;
                if is_in_range(&output_coord, &other.position.output) {
                    output_combinators.push((other, ConnectionPointType::Output));
                }
            }
        }

        // TODO: Connect with input network

        if (input_network_exists && input_combinators.is_empty())
            || (output_network_exists && output_combinators.is_empty())
        {
            return None; // Could not connect to the input or output network from this position
        }

        // Update input combinator
        for input_com in input_combinators {
            input_com.output_entities.push((
                this_entity_number,
                operation.get_input_connection_point().try_into().unwrap(),
            ));
        }

        let mut output_entities: Vec<(EntityNumber, ConnectionPoint)> = output_combinators
            .iter()
            .map(|(output_com, point_type)| {
                let point = match point_type {
                    ConnectionPointType::Input => output_com.operation.get_input_connection_point(),
                    ConnectionPointType::Output => {
                        output_com.operation.get_output_connection_point()
                    }
                }
                .try_into()
                .unwrap();
                (output_com.entity_number, point)
            })
            .collect();

        // Add itself as output if output and input networks are the same
        if input_network_red == output_network_red {
            output_entities.push((
                this_entity_number,
                operation.get_input_connection_point().try_into().unwrap(),
            )) // loopback
        }

        self.placed_positions.insert(input_coord);
        self.placed_positions.insert(output_coord);
        self.max_x = cmp::max(self.max_x, x);
        self.max_y = cmp::max(self.max_y, y);

        Some(FactorioCombinator {
            entity_number: this_entity_number,
            input_nid,
            output_nid,
            operation: operation.clone(),
            output_entities,
            position: CombinatorPosition {
                input: input_coord,
                output: output_coord,
            },
        })
    }

    fn get_next_entity_number(&mut self) -> EntityNumber {
        let en = self.next_entity_number;
        self.next_entity_number = self.next_entity_number.checked_add(1).unwrap();
        en
    }
}

impl Placer for TurdMaster2000 {
    fn place(mut self) -> Vec<FactorioCombinator> {
        let coms: Vec<_> = self.graph.iter_combinators().collect();

        'next_combinator: for (input_nid, output_nid, operation) in coms {
            for y in self.min_y - 1..=self.max_y + 1 {
                for x in self.min_x - 1..=self.max_x + 1 {
                    if let Some(combinator) =
                        self.try_place_combinator(x, y, input_nid, output_nid, &operation)
                    {
                        self.placed_combinators
                            .insert(combinator.entity_number, combinator);
                        continue 'next_combinator;
                    }
                }
            }
            todo!("COULD NOT PLACE COMBINATOR");
        }

        self.placed_combinators.into_values().collect()
    }
}
