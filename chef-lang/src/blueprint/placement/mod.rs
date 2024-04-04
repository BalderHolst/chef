use std::{
    cmp,
    collections::{HashMap, HashSet},
    num::NonZeroUsize,
};

use factorio_blueprint::objects as fbo;
use factorio_blueprint::objects::EntityNumber;
use fnv::FnvHashMap;
use noisy_float::prelude::*;

use crate::{
    blueprint::ConnectionPoint,
    compiler::graph::{Graph, LooseSig, NId, Operation, WireKind},
};

use super::{
    CircuitEntity, Combinator, ConnectionPointKind, ConstantCombinator, CoordSet, Placer,
    WIRE_RANGE,
};

pub(crate) fn is_in_range(p1: &CoordSet, p2: &CoordSet) -> bool {
    let dx = p2.0 - p1.0;
    let dy = p2.1 - p1.1;
    let dist = (dx * dx + dy * dy).sqrt();
    dist <= WIRE_RANGE
}

#[test]
#[rustfmt::skip]
fn test_is_in_range() {
    let wire_range = r64(WIRE_RANGE);
    assert!(is_in_range(&(r64(0.0), r64(0.0)), &(r64(0.0), wire_range)));
    assert!(is_in_range(&(r64(0.0), r64(0.0)), &(wire_range, r64(0.0))));
    assert!(!is_in_range(&(r64(0.0), r64(0.0)), &(wire_range, r64(1.0))));
    assert!(!is_in_range(&(r64(0.0), r64(0.0)), &(wire_range, r64(-1.0))));
    assert!(is_in_range(&(r64(0.0), wire_range), &(r64(0.0), r64(0.0))));
    assert!(is_in_range(&(wire_range, r64(0.0)), &(r64(0.0), r64(0.0))));
    assert!(!is_in_range(&(wire_range, r64(1.0)), &(r64(0.0), r64(0.0))));
    assert!(!is_in_range(&(wire_range, r64(-1.0)), &(r64(0.0), r64(0.0))));
}

type TilePos = (i64, i64);

// TODO: Create more placers

/// Tries to arrange combinators in a horizontal line facing north.
///
/// ### Steps for finding the location of a combinator.
/// 1. Check if any input or output combinators to this one have been placed.
/// 2. If they have, choose an end of the turd and check if the inputs/outputs are in range.
/// 3. If not try the other end.
/// 4. Lastly try placing on top of the turd.
/// 5. If nothing works, backtrack. We will just panic for now...
/// 6. When placed, connect wires to the other combinators that were placed.
pub struct TurdMaster2000 {
    graph: Graph<LooseSig>,
    placed_entities: HashMap<EntityNumber, Box<dyn CircuitEntity>>,
    placed_positions: HashSet<TilePos>,
    max_x: i64,
    min_x: i64,
    max_y: i64,
    min_y: i64,
    next_entity_number: EntityNumber,
}

impl TurdMaster2000 {
    pub fn new(graph: Graph<LooseSig>) -> Self {
        Self {
            graph,
            placed_entities: HashMap::new(),
            placed_positions: HashSet::new(),
            max_x: 0,
            min_x: 1,
            max_y: 0,
            min_y: 1,
            next_entity_number: EntityNumber::try_from(1).unwrap(),
        }
    }

    fn tile_is_occupied(&self, tile: &TilePos) -> bool {
        self.placed_positions.get(tile).is_some()
    }

    fn try_place_combinator(
        &mut self,
        x: i64,
        y: i64,
        input_nid: NId,
        output_nid: NId,
        operation: &Operation<LooseSig>,
    ) -> Option<Combinator> {
        let input_tile = (x, y * 2);
        let output_tile = (x, y * 2 + 1);

        // Convert to floats
        let input_coord = (r64(input_tile.0 as f64), r64(input_tile.1 as f64));
        let output_coord = (r64(output_tile.0 as f64), r64(output_tile.1 as f64));

        let combinator_position = (
            (input_coord.0 + output_coord.0) / 2.0,
            (input_coord.1 + output_coord.1) / 2.0,
        );

        if self.tile_is_occupied(&(input_tile)) || self.tile_is_occupied(&(output_tile)) {
            return None;
        }

        let input_network = self.graph.get_node_network_all(&input_nid);
        let output_network = self.graph.get_node_network_all(&output_nid);

        // `true` if any combinators with inputs or outputs the same as this combinator has been
        // placed. If they have, we have to be able to connect to the network from the current
        // position.
        let mut input_network_exists = false;
        let mut output_network_exists = false;

        let mut input_combinators: FnvHashMap<
            NonZeroUsize,
            (&mut Box<dyn CircuitEntity>, HashSet<WireKind>),
        > = FnvHashMap::default();

        #[allow(clippy::type_complexity)] // TODO: REMOVE
        let mut output_combinators: FnvHashMap<
            NonZeroUsize,
            (
                &mut Box<dyn CircuitEntity>,
                ConnectionPointKind,
                HashSet<WireKind>,
            ),
        > = FnvHashMap::default();

        // TODO: This can result in skipped entity numbers. Make this that this is ok.
        let this_entity_number = self.get_next_entity_number();

        // Check that wire can reach the necessary placed combinators in this position
        for other in &mut self.placed_entities.values_mut() {
            let other_output_nid = other.output_nid();
            let other_input_nid = other.input_nid();
            let entity_number = other.entity_number();

            // Red connections
            if input_network.contains(&(other_output_nid, WireKind::Red)) {
                input_network_exists = true;
                if is_in_range(&input_coord, &other.output_pos()) {
                    input_combinators
                        .entry(entity_number)
                        .and_modify(|(_entity, wires)| {
                            let _ = wires.insert(WireKind::Red);
                        })
                        .or_insert((other, HashSet::from_iter([WireKind::Red])));
                }
            } else if output_network.contains(&(other_input_nid, WireKind::Red)) {
                output_network_exists = true;
                if is_in_range(&output_coord, &other.input_pos()) {
                    output_combinators
                        .entry(entity_number)
                        .and_modify(|(_entity, _point_type, wires)| {
                            let _ = wires.insert(WireKind::Red);
                        })
                        .or_insert((
                            other,
                            ConnectionPointKind::Input,
                            HashSet::from([WireKind::Red]),
                        ));
                }
            }

            // TODO: Check if INPUT is in the same network as another placed combinator.
            // TODO: Check if OUTPUT is in the same network as another placed combinator.

            // Green connections
            if input_network.contains(&(other_output_nid, WireKind::Green)) {
                todo!();
            } else if output_network.contains(&(other_input_nid, WireKind::Green)) {
                todo!();
            } else if output_network.contains(&(other_output_nid, WireKind::Green)) {
                todo!();
            }
        }

        if (input_network_exists && input_combinators.is_empty())
            || (output_network_exists && output_combinators.is_empty())
        {
            return None; // Could not connect to the input or output network from this position
        }

        // Update input combinator
        for (input_com, wks) in input_combinators.values_mut() {
            let input_conn_point = input_com.input_conn_point();
            input_com
                .as_mut()
                .output_entities_mut()
                .entry(this_entity_number)
                .and_modify(|(_entity, wires)| wires.extend(wks.iter().cloned()))
                .or_insert((input_conn_point.try_into().unwrap(), wks.clone()));
        }

        let output_entities: FnvHashMap<EntityNumber, (ConnectionPoint, HashSet<WireKind>)> =
            output_combinators
                .values()
                .map(|(output_com, point_type, wires)| {
                    let point = match point_type {
                        ConnectionPointKind::Input => output_com.input_conn_point(),
                        ConnectionPointKind::Output => output_com.output_conn_point(),
                    }
                    .try_into()
                    .unwrap();
                    (output_com.entity_number(), (point, wires.clone()))
                })
                .collect();

        let mut this_com = Combinator {
            entity_number: this_entity_number,
            input_nid,
            output_nid,
            operation: operation.clone(),
            output_entities,
            position: combinator_position,
        };

        // Add itself as output if output and input networks are the same
        if input_network == output_network {
            this_com.output_entities.insert(
                this_entity_number, // loopback
                (
                    this_com.input_conn_point().try_into().unwrap(),
                    HashSet::from([WireKind::Red]), // TODO: This is hardcoded
                ),
            );
        }

        self.placed_positions.insert(input_tile);
        self.placed_positions.insert(output_tile);
        self.max_x = cmp::max(self.max_x, x);
        self.max_y = cmp::max(self.max_y, y);

        Some(this_com)
    }

    fn get_next_entity_number(&mut self) -> EntityNumber {
        let en = self.next_entity_number;
        self.next_entity_number = self.next_entity_number.checked_add(1).unwrap();
        en
    }
}

impl Placer for TurdMaster2000 {
    fn place(mut self) -> Vec<fbo::Entity> {
        // Place input combinators
        for (n, (name, input_nid)) in self.graph.get_input_nodes().iter().enumerate() {
            let x = (n / 2) as i64;
            let y = (n % 2) as i64;
            let pos: CoordSet = (r64(x as f64), r64(y as f64));
            let en = self.get_next_entity_number();
            let const_com = ConstantCombinator::new_sign(name, en, *input_nid, pos);
            self.placed_entities.insert(en, Box::new(const_com));
            self.placed_positions.insert((x, y));
            self.max_x = cmp::max(self.max_x, x);
            self.max_y = cmp::max(self.max_y, y);
        }

        // Place output combinators
        for (n, (name, output_nid)) in self.graph.get_output_nodes().iter().enumerate() {
            let x = (n / 2) as i64 + self.max_x + 1;
            let y = (n % 2) as i64;
            let pos: CoordSet = (r64(x as f64), r64(y as f64));
            let en = self.get_next_entity_number();
            let const_com = ConstantCombinator::new_sign(name, en, *output_nid, pos);
            self.placed_entities.insert(en, Box::new(const_com));
            self.placed_positions.insert((x, y));
            self.max_x = cmp::max(self.max_x, x);
            self.max_y = cmp::max(self.max_y, y);
        }

        // Place combinators
        let coms: Vec<_> = self.graph.iter_combinators().collect();
        'next_combinator: for (input_nid, output_nid, operation) in coms {
            for y in self.min_y - 1..=self.max_y + 1 {
                for x in self.min_x - 1..=self.max_x + 1 {
                    if let Some(combinator) =
                        self.try_place_combinator(x, y, input_nid, output_nid, &operation)
                    {
                        self.placed_entities
                            .insert(combinator.entity_number, Box::new(combinator));
                        continue 'next_combinator;
                    }
                }
            }
            todo!("COULD NOT PLACE COMBINATOR");
        }

        self.placed_entities
            .into_values()
            .map(|c| c.to_blueprint_entity())
            .collect()
    }
}
