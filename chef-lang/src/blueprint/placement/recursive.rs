use std::{collections::HashSet, num::NonZeroUsize};

use factorio_blueprint::objects as fbo;
use fbo::EntityNumber;
use fnv::FnvHashMap;
use noisy_float::prelude::*;

use crate::{
    blueprint::{
        CircuitEntity, Combinator, ConnectionPoint, ConnectionPointKind, Graph, Operation,
    },
    compiler::graph::{NId, WireKind},
};

use super::utils::is_in_range;

type TilePos = (usize, usize);

const MAX_TRIES: usize = 100;

pub struct RecursivePlacer {
    graph: Graph,
    operations: Vec<(NId, NId, Operation)>,
    op_cursor: usize,
    placed_entities: Vec<Box<dyn CircuitEntity>>,
    placed_positions: HashSet<TilePos>,
    max_width: usize,
    next_entity_number: EntityNumber,
}

enum PlacementResult {
    NotPossible,
    Success,
}

impl RecursivePlacer {
    pub fn new(graph: Graph) -> Self {
        let operations: Vec<_> = graph.iter_ops().collect();
        let max_width = (r64(operations.len() as f64 * 1.8).sqrt()).ceil().raw() as usize;
        Self {
            graph,
            operations,
            op_cursor: 0,
            placed_entities: vec![],
            placed_positions: HashSet::new(),
            max_width,
            next_entity_number: EntityNumber::try_from(1).unwrap(),
        }
    }

    fn tile_is_occupied(&self, tile: &TilePos) -> bool {
        self.placed_positions.get(tile).is_some()
    }

    fn try_place_combinator(
        &mut self,
        x: usize,
        y: usize,
        cursor_offset: usize,
    ) -> PlacementResult {
        if self.op_cursor - cursor_offset >= self.operations.len() {
            return PlacementResult::Success;
        }

        let (input_nid, output_nid, operation) = self.operations[self.op_cursor].clone();

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
            return PlacementResult::NotPossible;
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
        for other in &mut self.placed_entities.iter_mut() {
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
            return PlacementResult::NotPossible; // Could not connect to the input or output network from this position
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

        let mut this_entity = Combinator {
            entity_number: this_entity_number,
            input_nid,
            output_nid,
            operation,
            output_entities,
            position: combinator_position,
        };

        // Add itself as output if output and input networks are the same
        if input_network == output_network {
            this_entity.output_entities.insert(
                this_entity_number, // loopback
                (
                    this_entity.input_conn_point().try_into().unwrap(),
                    HashSet::from([WireKind::Red]), // TODO: This is hardcoded
                ),
            );
        }

        // Always add this combinator to the placed entities
        self.op_cursor += 1;
        self.placed_entities.push(Box::new(this_entity));
        self.placed_positions.insert(input_tile);
        self.placed_positions.insert(output_tile);

        let first_available_x = x;
        let first_available_y = y;

        for n in 1..MAX_TRIES {
            let next_x = (first_available_x + n) % self.max_width;
            let next_y = (first_available_y + n) / self.max_width;

            let last_placed_pos = match self.placed_entities.last() {
                Some(c) => c.output_pos(), // TODO: Maybe get center pos?
                None => (r64(0.0), r64(0.0)),
            };

            if !is_in_range(&(r64(next_x as f64), r64(next_y as f64)), &last_placed_pos) {
                return PlacementResult::NotPossible;
            }

            match self.try_place_combinator(next_x, next_y, cursor_offset) {
                PlacementResult::NotPossible => continue,
                PlacementResult::Success => return PlacementResult::Success,
            }
        }

        // Undo placement
        self.op_cursor -= 1;
        self.placed_entities.pop();
        self.placed_positions.remove(&input_tile);
        self.placed_positions.remove(&output_tile);

        PlacementResult::NotPossible
    }

    fn get_next_entity_number(&mut self) -> EntityNumber {
        let en = self.next_entity_number;
        self.next_entity_number = self.next_entity_number.checked_add(1).unwrap();
        en
    }
}

impl super::Placer for RecursivePlacer {
    fn place(graph: Graph) -> Vec<fbo::Entity> {
        let mut placer = RecursivePlacer::new(graph);

        // TODO: Place input combinators

        // TODO: Place output combinators

        match placer.try_place_combinator(0, 0, 0) {
            PlacementResult::NotPossible => panic!("Could not place entities."),
            PlacementResult::Success => (),
        }

        placer
            .placed_entities
            .iter()
            .map(|c| c.to_blueprint_entity())
            .collect()
    }
}
