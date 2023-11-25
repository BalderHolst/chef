//! Converted combinator graphs to factorio blueprints.

mod placement;

use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::vec;

use factorio_blueprint as fb;
use fb::objects::{ArithmeticConditions, ControlFilter, DeciderConditions, SignalID, SignalIDType};
use fb::objects::{Blueprint, ControlBehavior, Entity, EntityNumber, Position};
use fb::Container;
use noisy_float::types::R64;

use crate::compiler::graph::{
    self, ArithmeticOperation, Connection, DeciderOperation, Graph, IOType,
};
use crate::utils::BASE_SIGNALS;

use self::graph::NId;

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

/// Unplaced Factorio Combinator
#[derive(Clone, Debug, PartialEq)]
pub struct Combinator {
    pub input_network: NId,
    pub output_network: NId,
    pub operation: Connection,
}

impl Combinator {
    pub fn to_placed(
        self,
        output_entity: EntityNumber,
        position: CombinatorPosition,
    ) -> PlacedCombinator {
        PlacedCombinator {
            input_network: self.input_network,
            output_network: self.output_network,
            operation: self.operation,
            output_entity,
            position,
        }
    }
}

/// Placed Factorio Combinator
#[derive(Clone, Debug, PartialEq)]
pub struct PlacedCombinator {
    pub input_network: NId,
    pub output_network: NId,
    pub operation: Connection,
    pub output_entity: EntityNumber,
    pub position: CombinatorPosition,
}

/// The maxinum distanct a wire can connect two points in factorio.
const WIRE_RANGE: f64 = 9.0;

pub fn convert_to_blueprint_string(
    opts: &crate::cli::CookOpts,
    graph: Graph,
) -> fb::Result<String> {
    let container = convert_to_blueprint(graph);
    fb::BlueprintCodec::encode_string(&container)
}

fn convert_to_blueprint(graph: Graph) -> Container {
    let combinators: Vec<Combinator> = graph
        .adjacency
        .iter()
        .map(|(from_nid, to_vec)| to_vec.iter().map(|(to_nid, conn)| {
            Combinator {
                input_network: *from_nid,
                output_network: *to_nid,
                operation: *conn,
            }
        }))
        .flatten()
        .collect();

    todo!()
}

//pub struct BlueprintConverter {
//    graph: Graph,

//    /// Holds pairs of combinator ids and combinators
//    combinators: HashMap<EntityNumber, Combinator>,

//    next_entity_number: EntityNumber,
//}

//impl BlueprintConverter {
//    pub fn new(graph: Graph) -> Self {
//        let mut this = Self {
//            graph,
//            combinators: HashMap::new(),
//            next_entity_number: EntityNumber::new(1).unwrap(),
//        };
//        this.populate_combinators();
//        this
//    }

//    fn populate_combinators(&mut self) {
//        for (from_nid, to_vec) in self.graph.adjacency {
//            for (to_nid, conn) in to_vec {
//                self.combinators.insert(
//                    self.get_next_entity_number(),
//                    Combinator {
//                        input_network: from_nid,
//                        output_network: to_nid,
//                        operation: conn,
//                        position: None,
//                    },
//                );
//            }
//        }
//    }

//    fn get_next_entity_number(&mut self) -> EntityNumber {
//        let en = self.next_entity_number;
//        self.next_entity_number = self
//            .next_entity_number
//            .checked_add(1)
//            .expect("This should not overflow... hopefully.");
//        en
//    }

//    /// Create an empty circuit blueprint
//    fn create_blueprint(entities: Vec<Entity>) -> Blueprint {
//        Blueprint {
//            item: "Blueprint".to_string(),
//            label: "Circuit".to_string(),
//            label_color: None,
//            entities,
//            tiles: vec![],
//            icons: vec![],
//            schedules: vec![],
//            version: 0,
//        }
//    }

//    fn get_signal_type(s: &str) -> SignalIDType {
//        for line in BASE_SIGNALS.lines() {
//            let (type_, sig) = line
//                .split_once(':')
//                .expect("Eact line in signale files should be formattet like this: type:signal");
//            if s == sig {
//                return match type_ {
//                    "item" => SignalIDType::Item,
//                    "fluid" => SignalIDType::Fluid,
//                    "virtual" => SignalIDType::Virtual,
//                    _ => panic!("Invalid signal type in signal file: `{}`.", type_),
//                };
//            }
//        }
//        panic!(
//            "Could not find signal: {}. The typechecker should have prevented this.",
//            s
//        );
//    }

//    // Get the corresponding (signal_type, signal_string) pair
//    fn iotype_to_signal_pair(t: IOType) -> (SignalIDType, String) {
//        match t {
//            IOType::Signal(s) => (Self::get_signal_type(s.as_str()), s),
//            IOType::Constant(_) => todo!(),
//            IOType::ConstantSignal(_) => todo!(),
//            IOType::All => todo!(),
//            graph::IOType::AnySignal(_) => panic!("AnySignals should be eradicated at this point."),
//        }
//    }

//    /// Returns (first_constant, first_signal)
//    fn iotype_to_const_signal_pair(t: &graph::IOType) -> (Option<i32>, Option<SignalID>) {
//        match t {
//            graph::IOType::Signal(s) => {
//                let type_ = Self::get_signal_type(s.as_str());
//                (
//                    None,
//                    Some(SignalID {
//                        name: s.clone(),
//                        type_, // TODO
//                    }),
//                )
//            }
//            graph::IOType::ConstantSignal(_) => todo!(),
//            graph::IOType::Constant(n) => (Some(*n), None),
//            graph::IOType::All => todo!(),
//            graph::IOType::AnySignal(_) => panic!("AnySignals should be eradicated at this point."),
//        }
//    }

//    fn arithmetic_operation_to_op_string(op: &ArithmeticOperation) -> String {
//        match op {
//            ArithmeticOperation::Add => "+",
//            ArithmeticOperation::Subtract => "-",
//            ArithmeticOperation::Multiply => "*",
//            ArithmeticOperation::Divide => "/",
//        }
//        .to_string()
//    }

//    fn decider_operation_to_op_string(op: &DeciderOperation) -> String {
//        match op {
//            DeciderOperation::LargerThan => ">",
//            DeciderOperation::LargerThanOrEqual => ">=",
//            DeciderOperation::LessThan => "<",
//            DeciderOperation::LessThanOrEqual => "<=",
//            DeciderOperation::Equals => "=",
//            DeciderOperation::NotEquals => "!=",
//        }
//        .to_string()
//    }

//    /// Convert a grapn connection to the control_behavior of either arithmetic or deciter
//    /// combinator.
//    fn connection_to_control_behavior(conn: &graph::Connection) -> ControlBehavior {
//        match conn {
//            graph::Connection::Arithmetic(ac) => {
//                let (first_constant, first_signal) = Self::iotype_to_const_signal_pair(&ac.left);
//                let (second_constant, second_signal) = Self::iotype_to_const_signal_pair(&ac.right);
//                let (_, output_signal) = Self::iotype_to_const_signal_pair(&ac.output);
//                let operation = Self::arithmetic_operation_to_op_string(&ac.operation);

//                ControlBehavior {
//                    arithmetic_conditions: Some(ArithmeticConditions {
//                        first_constant,
//                        first_signal,
//                        second_constant,
//                        second_signal,
//                        operation,
//                        output_signal,
//                    }),
//                    decider_conditions: None,
//                    filters: None,
//                    is_on: None,
//                }
//            }
//            graph::Connection::Decider(dc) => {
//                let (_first_constant, first_signal) = Self::iotype_to_const_signal_pair(&dc.left);
//                let (second_constant, second_signal) = Self::iotype_to_const_signal_pair(&dc.right);
//                let (_, output_signal) = Self::iotype_to_const_signal_pair(&dc.output);
//                let operation = Self::decider_operation_to_op_string(&dc.operation);

//                ControlBehavior {
//                    arithmetic_conditions: None,
//                    decider_conditions: Some(DeciderConditions {
//                        first_signal,
//                        second_signal,
//                        constant: second_constant,
//                        comparator: operation,
//                        output_signal,
//                        copy_count_from_input: Some(false),
//                    }),
//                    filters: None,
//                    is_on: None,
//                }
//            }
//            graph::Connection::Gate(gc) => {
//                let (_first_constant, first_signal) = Self::iotype_to_const_signal_pair(&gc.left);
//                let (second_constant, second_signal) = Self::iotype_to_const_signal_pair(&gc.right);
//                let (_, gate_signal) = Self::iotype_to_const_signal_pair(&gc.gate_type);
//                let operation = Self::decider_operation_to_op_string(&gc.operation);

//                ControlBehavior {
//                    arithmetic_conditions: None,
//                    decider_conditions: Some(DeciderConditions {
//                        first_signal,
//                        second_signal,
//                        constant: second_constant,
//                        comparator: operation,
//                        output_signal: gate_signal,
//                        copy_count_from_input: Some(true),
//                    }),
//                    filters: None,
//                    is_on: None,
//                }
//            }
//            graph::Connection::Constant(cc) => {
//                let (type_, signal) = Self::iotype_to_signal_pair(cc.type_.clone());
//                ControlBehavior {
//                    arithmetic_conditions: None,
//                    decider_conditions: None,
//                    filters: {
//                        Some(vec![ControlFilter {
//                            signal: SignalID {
//                                name: signal,
//                                type_,
//                            },
//                            index: NonZeroUsize::new(1).unwrap(),
//                            count: cc.count,
//                        }])
//                    },
//                    is_on: Some(true),
//                }
//            }
//        }
//    }

//    fn combinator_to_entity(&self, this_combinator: Combinator) -> Entity {
//        // let mut blueprint_connections: HashMap<EntityNumber, fbo::Connection> = HashMap::new();

//        // if let Some(inputs) = self.graph.wires.get(&this_combinator.input_node) {
//        //     // Connect inputs of the combinator
//        //     for in_nid in inputs {
//        //         let other_com = self.graph.get_corresponding_combinator(*in_nid);

//        //         // Usually the output is connection point 2, but for the constant combinator it is
//        //         // one.
//        //         let other_out_circuit_id = match other_com.operation {
//        //             graph::Connection::Arithmetic(_) => Some(2),
//        //             graph::Connection::Decider(_) => Some(2),
//        //             graph::Connection::Gate(_) => Some(2),
//        //             graph::Connection::Constant(_) => Some(1),
//        //         };

//        //         println!(
//        //             "[INPUT] Connecting {} (point: 1) to {} (point: {})",
//        //             this_combinator.entity_number, other_com.entity_number, other_out_circuit_id.unwrap()
//        //         );

//        //         // Connect to the input (id 1) connection point if THIS conbinator.
//        //         blueprint_connections.insert(NonZeroUsize::new(1).unwrap(), {
//        //             fbo::Connection {
//        //                 red: None,
//        //                 green: Some(vec![fbo::ConnectionData {
//        //                     // Entity number of other conbinator
//        //                     entity_id: other_com.entity_number,
//        //                     // Connect to the output of the other combinator
//        //                     circuit_id: other_out_circuit_id,
//        //                 }]),
//        //             }
//        //         });
//        //     }
//        // }

//        // // Do the same for outputs
//        // if let Some(outputs) = self.graph.wires.get(&this_combinator.output_node) {
//        //     for out_nid in outputs {
//        //         let other_com = self.graph.get_corresponding_combinator(*out_nid);
//        //         println!(
//        //             "[OUTPUT] Connecting {} (point: 2) to {} (point: 1)",
//        //             this_combinator.entity_number, other_com.entity_number
//        //         );

//        //         // Connect to the OUTPUT (id 2) connection point if THIS conbinator.
//        //         blueprint_connections.insert(NonZeroUsize::new(2).unwrap(), {
//        //             fbo::Connection {
//        //                 red: None,
//        //                 green: Some(vec![fbo::ConnectionData {
//        //                     // Entity number of other conbinator
//        //                     entity_id: other_com.entity_number,
//        //                     // Connect to the INPUT of the other combinator
//        //                     circuit_id: Some(1),
//        //                 }]),
//        //             }
//        //         });
//        //     }
//        // }

//        let control_behavior = Some(Self::connection_to_control_behavior(
//            &this_combinator.operation,
//        ));

//        let entity_name = match &this_combinator.operation {
//            graph::Connection::Arithmetic(_) => "arithmetic-combinator",
//            graph::Connection::Decider(_) => "decider-combinator",
//            graph::Connection::Gate(_) => "decider-combinator",
//            graph::Connection::Constant(_) => "constant-combinator",
//        }
//        .to_string();

//        Entity {
//            entity_number: this_combinator.entity_number,
//            name: entity_name,
//            position: this_combinator
//                .position
//                .expect("Combinators should all be placed at this point")
//                .factorio_pos(),
//            direction: None,
//            orientation: None,

//            // Connections are block recorded in both the connected entities.
//            //
//            // connections: Some(
//            // StringIdx(
//            //   {
//            //     "2": ConnectionPoint { // The id of the connection point on THIS combinator
//            //                            // "1" = input & "2" -> output
//            //       red: None,
//            //       green: Some(
//            //         [
//            //           ConnectionData {
//            //             entity_id: 2,  // The BLUEPRINT id of the connected entity
//            //             circuit_id: Some( // The connection point id on the OTHER entity
//            //               1,
//            //             ),
//            //           },
//            //         ],
//            //       ),
//            //     },
//            //   },
//            // ),
//            // ),
//            connections: None, // This will be added later
//            control_behavior,
//            items: None,
//            recipe: None,
//            bar: None,
//            inventory: None,
//            infinity_settings: None,
//            type_: None,
//            input_priority: None,
//            output_priority: None,
//            filter: None,
//            filters: None,
//            filter_mode: None,
//            override_stack_size: None,
//            drop_position: None,
//            pickup_position: None,
//            request_filters: None,
//            request_from_buffers: None,
//            parameters: None,
//            alert_parameters: None,
//            auto_launch: None,
//            variation: None,
//            color: None,
//            station: None,
//        }
//    }

//    pub fn convert_to_blueprint(&mut self) -> Container {
//        let mut entities: HashMap<EntityNumber, Entity> = HashMap::new();
//        for com in &self.combinators {
//            let entity = self.combinator_to_entity(com.clone());
//            entities.insert(com.entity_number, entity);
//        }
//        self.add_wires(&mut entities);
//        let blueprint = Self::create_blueprint(entities.values().map(|e| e.clone()).collect());
//        Container::Blueprint(blueprint)
//    }

//    pub fn add_wires(&mut self, entities: &mut HashMap<EntityNumber, Entity>) {
//        todo!()
//        // for (from, to_vec) in &self.graph.wires {
//        //     let from_com = self.graph.get_corresponding_combinator(*from);
//        //     let mut from_entity = entities.get(&from_com.entity_number).unwrap().to_owned();
//        //     for to in to_vec {
//        //         let to_com = self.graph.get_corresponding_combinator(*to);

//        //         // Usually the output is connection point 2, but for the constant combinator it is
//        //         // one.
//        //         let from_conn_point = match from_com.operation {
//        //             graph::Connection::Arithmetic(_) => NonZeroUsize::try_from(2),
//        //             graph::Connection::Decider(_) => NonZeroUsize::try_from(2),
//        //             graph::Connection::Gate(_) => NonZeroUsize::try_from(2),
//        //             graph::Connection::Constant(_) => NonZeroUsize::try_from(1),
//        //         }
//        //         .unwrap();

//        //         if from_entity.connections.is_none() {
//        //             from_entity.connections = Some(EntityConnections::NumberIdx(HashMap::new()));
//        //         }

//        //         let from_entity_conns = match &mut from_entity.connections {
//        //             Some(EntityConnections::NumberIdx(ref mut conns)) => conns,
//        //             _ => panic!("This should never happen..."),
//        //         };

//        //         from_entity_conns.insert(from_conn_point, {
//        //             fbo::Connection {
//        //                 red: None,
//        //                 green: Some(vec![fbo::ConnectionData {
//        //                     // Entity number of other conbinator
//        //                     entity_id: to_com.entity_number,
//        //                     // Connect to the input of the other combinator
//        //                     circuit_id: Some(1),
//        //                 }]),
//        //             }
//        //         });
//        //     }
//        //     entities.insert(from_com.entity_number, from_entity);
//        // }
//    }

// }
