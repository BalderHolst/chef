//! Converted combinator graphs to factorio blueprints.

mod placement;
pub mod utils;

use std::{collections::HashMap, fmt::Display};

use factorio_blueprint as fb;
use factorio_blueprint::objects as fbo;

use noisy_float::types::R64;

use crate::{
    compiler::{
        graph::{self, ArithmeticOperation, DeciderOperation, Graph, IOType, NetworkId},
        RESERVED_SIGNAL,
    },
    utils::BASE_SIGNALS,
};

use placement::Placer;
use placement::TurdMaster2000;

type Operation = graph::Operation;

pub(crate) type Coord = i64;

/// 2D Coordinate inside the Factorio world
pub(crate) type CoordSet = (Coord, Coord);

#[derive(Clone, Debug, PartialEq)]
pub struct CombinatorPosition {
    pub input: CoordSet,
    pub output: CoordSet,
}

impl CombinatorPosition {
    pub fn factorio_pos(&self) -> fbo::Position {
        let (x1, y1) = self.input;
        let (x2, y2) = self.output;

        let x = (x1 + x2) as f64 / 2.0;
        let y = (y1 + y2) as f64 / 2.0;

        let x = R64::new(x);
        let y = R64::new(y);

        fbo::Position { x, y }
    }
}

impl Display for CombinatorPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (x1, y1) = self.input;
        let (x2, y2) = self.output;
        write!(f, "{{({}, {}) -> ({}, {})}}", x1, y1, x2, y2)
    }
}

type ConnectionPoint = i32;

enum ConnectionPointType {
    Input,
    Output,
}

/// Placed Factorio Combinator
#[derive(Clone, Debug, PartialEq)]
pub struct FactorioCombinator {
    pub entity_number: fbo::EntityNumber,
    pub input_network: NetworkId,
    pub output_network: NetworkId,
    pub operation: Operation,
    pub output_entities: Vec<(fbo::EntityNumber, ConnectionPoint)>,
    pub position: CombinatorPosition,
}

impl Operation {
    // TODO: remove this with removing constants as connections
    fn get_output_connection_point(&self) -> usize {
        2
        // match self {
        //     Connection::Arithmetic(_) => 2,
        //     Connection::Decider(_) => 2,
        //     Connection::Gate(_) => 2,
        //     Connection::Constant(_) => 1,
        // }
    }

    // TODO: remove this with removing constants as connections
    fn get_input_connection_point(&self) -> usize {
        1
        // match self {
        //     Connection::Arithmetic(_) => 1,
        //     Connection::Decider(_) => 1,
        //     Connection::Gate(_) => 1,
        //     Connection::Constant(_) => 1,
        // }
    }
}

macro_rules! each {
    () => {
        factorio_blueprint::objects::SignalID {
            name: "signal-each".to_string(),
            type_: fbo::SignalIDType::Virtual,
        }
    };
}

impl FactorioCombinator {
    pub fn to_blueprint_entity(&self) -> fbo::Entity {
        let output_connections: Vec<fbo::ConnectionData> = self
            .output_entities
            .iter()
            .map(|(out_en, to_conn_point)| fbo::ConnectionData {
                entity_id: *out_en,
                circuit_id: Some(*to_conn_point),
            })
            .collect();

        let output_connection_point = self.operation.get_output_connection_point();
        let mut connections: HashMap<fbo::EntityNumber, fbo::Connection> = HashMap::new();
        connections.insert(
            fbo::OneBasedIndex::new(output_connection_point).unwrap(),
            fbo::Connection {
                red: None,
                green: Some(output_connections),
            },
        );

        let control_behavior = self.get_control_behavior();

        // Internal factorio name of combinator
        let name = match &self.operation {
            graph::Operation::Arithmetic(_) => "arithmetic-combinator",
            graph::Operation::Decider(_) => "decider-combinator",
            graph::Operation::Pick(_) => "arithmetic-combinator",
            graph::Operation::Gate(_) => "decider-combinator",
            graph::Operation::Delay(_) => "arithmetic-combinator",
            graph::Operation::Sum(_) => "arithmetic-combinator",
        }
        .to_string();

        fbo::Entity {
            entity_number: self.entity_number,
            name,
            position: self.position.factorio_pos(),
            direction: None,
            orientation: None,
            connections: Some(fbo::EntityConnections::NumberIdx(connections)),
            control_behavior: Some(control_behavior),
            items: None,
            recipe: None,
            bar: None,
            inventory: None,
            infinity_settings: None,
            type_: None,
            input_priority: None,
            output_priority: None,
            filter: None,
            filters: None,
            filter_mode: None,
            override_stack_size: None,
            drop_position: None,
            pickup_position: None,
            request_filters: None,
            request_from_buffers: None,
            parameters: None,
            alert_parameters: None,
            auto_launch: None,
            variation: None,
            color: None,
            station: None,
        }
    }

    fn get_control_behavior(&self) -> fbo::ControlBehavior {
        match &self.operation {
            graph::Operation::Arithmetic(ac) => {
                let (first_constant, first_signal) = Self::iotype_to_const_signal_pair(&ac.left);
                let (second_constant, second_signal) = Self::iotype_to_const_signal_pair(&ac.right);
                let (_, output_signal) = Self::iotype_to_const_signal_pair(&ac.output);
                let operation = Self::arithmetic_operation_to_op_string(&ac.operation);

                fbo::ControlBehavior {
                    arithmetic_conditions: Some(fbo::ArithmeticConditions {
                        first_constant,
                        first_signal,
                        second_constant,
                        second_signal,
                        operation,
                        output_signal,
                    }),
                    decider_conditions: None,
                    filters: None,
                    is_on: None,
                }
            }
            graph::Operation::Decider(dc) => {
                let (_first_constant, first_signal) = Self::iotype_to_const_signal_pair(&dc.left);
                let (second_constant, second_signal) = Self::iotype_to_const_signal_pair(&dc.right);
                let (_, output_signal) = Self::iotype_to_const_signal_pair(&dc.output);
                let operation = Self::decider_operation_to_op_string(&dc.operation);

                fbo::ControlBehavior {
                    arithmetic_conditions: None,
                    decider_conditions: Some(fbo::DeciderConditions {
                        first_signal,
                        second_signal,
                        constant: second_constant,
                        comparator: operation,
                        output_signal,
                        copy_count_from_input: Some(false),
                    }),
                    filters: None,
                    is_on: None,
                }
            }

            graph::Operation::Pick(pc) => {
                let (first_constant, first_signal) = Self::iotype_to_const_signal_pair(&pc.output);
                let (second_constant, second_signal) = (Some(0), None);
                let output_signal = first_signal.clone();
                let operation = "+".to_string();

                fbo::ControlBehavior {
                    arithmetic_conditions: Some(fbo::ArithmeticConditions {
                        first_constant,
                        first_signal,
                        second_constant,
                        second_signal,
                        operation,
                        output_signal,
                    }),
                    decider_conditions: None,
                    filters: None,
                    is_on: None,
                }
            }

            graph::Operation::Gate(gc) => {
                let (_first_constant, first_signal) = Self::iotype_to_const_signal_pair(&gc.left);
                let (second_constant, second_signal) = Self::iotype_to_const_signal_pair(&gc.right);
                let (_, gate_signal) = Self::iotype_to_const_signal_pair(&gc.gate_type);
                let operation = Self::decider_operation_to_op_string(&gc.operation);

                fbo::ControlBehavior {
                    arithmetic_conditions: None,
                    decider_conditions: Some(fbo::DeciderConditions {
                        first_signal,
                        second_signal,
                        constant: second_constant,
                        comparator: operation,
                        output_signal: gate_signal,
                        copy_count_from_input: Some(true),
                    }),
                    filters: None,
                    is_on: None,
                }
            }
            graph::Operation::Delay(_dc) => {
                let (first_constant, first_signal) = (None, Some(each!()));
                let (second_constant, second_signal) = (Some(0), None);
                let output_signal = Some(each!());
                let operation = "+".to_string();

                fbo::ControlBehavior {
                    arithmetic_conditions: Some(fbo::ArithmeticConditions {
                        first_constant,
                        first_signal,
                        second_constant,
                        second_signal,
                        operation,
                        output_signal,
                    }),
                    decider_conditions: None,
                    filters: None,
                    is_on: None,
                }
            }

            graph::Operation::Sum(sc) => {
                let (first_constant, first_signal) = (None, Some(each!()));
                let (second_constant, second_signal) = (Some(0), None);
                let (_, output_signal) = Self::iotype_to_const_signal_pair(&sc.output);
                let operation = "+".to_string();

                fbo::ControlBehavior {
                    arithmetic_conditions: Some(fbo::ArithmeticConditions {
                        first_constant,
                        first_signal,
                        second_constant,
                        second_signal,
                        operation,
                        output_signal,
                    }),
                    decider_conditions: None,
                    filters: None,
                    is_on: None,
                }
            }
        }
    }

    fn arithmetic_operation_to_op_string(op: &ArithmeticOperation) -> String {
        match op {
            ArithmeticOperation::Add => "+",
            ArithmeticOperation::Subtract => "-",
            ArithmeticOperation::Multiply => "*",
            ArithmeticOperation::Divide => "/",
        }
        .to_string()
    }

    fn decider_operation_to_op_string(op: &DeciderOperation) -> String {
        match op {
            DeciderOperation::LargerThan => ">",
            DeciderOperation::LargerThanOrEqual => ">=",
            DeciderOperation::LessThan => "<",
            DeciderOperation::LessThanOrEqual => "<=",
            DeciderOperation::Equals => "=",
            DeciderOperation::NotEquals => "!=",
            DeciderOperation::EveryEquals => todo!(),
            DeciderOperation::EveryLargerThan => todo!(),
            DeciderOperation::EveryLargerThanEquals => todo!(),
            DeciderOperation::EveryLessThan => todo!(),
            DeciderOperation::EveryLessThanEquals => todo!(),
            DeciderOperation::EveryNotEquals => todo!(),
            DeciderOperation::AnyEquals => todo!(),
            DeciderOperation::AnyLargerThan => todo!(),
            DeciderOperation::AnyLargerThanEquals => todo!(),
            DeciderOperation::AnyLessThan => todo!(),
            DeciderOperation::AnyLessThanEquals => todo!(),
            DeciderOperation::AnyNotEquals => todo!(),
        }
        .to_string()
    }

    /// Returns (first_constant, first_signal)
    fn iotype_to_const_signal_pair(t: &graph::IOType) -> (Option<i32>, Option<fbo::SignalID>) {
        match t {
            graph::IOType::Signal(s) => {
                let type_ = Self::get_signal_type(s.as_str());
                (
                    None,
                    Some(fbo::SignalID {
                        name: s.clone(),
                        type_,
                    }),
                )
            }
            graph::IOType::ConstantSignal((s, n)) => {
                let type_ = Self::get_signal_type(s.as_str());
                (
                    Some(*n),
                    Some(fbo::SignalID {
                        name: s.clone(),
                        type_,
                    }),
                )
            }
            graph::IOType::Constant(n) => (Some(*n), None),
            graph::IOType::Many => (
                None,
                Some(fbo::SignalID {
                    // TODO: check that "everything" is correct
                    name: "signal-everything".to_string(),
                    type_: fbo::SignalIDType::Virtual,
                }),
            ),

            graph::IOType::AnySignal(_) => panic!("AnySignals should be eradicated at this point."),
            graph::IOType::ConstantAny(_) => {
                panic!("CostantAny should be eradicated at this point.")
            }
        }
    }

    // TODO: Convert return type to union
    // Get the corresponding (signal_type, signal_string) pair
    fn _iotype_to_signal_pair(t: IOType) -> (fbo::SignalIDType, String) {
        match t {
            IOType::Signal(s) => (Self::get_signal_type(s.as_str()), s),
            IOType::Constant(_) => todo!(),
            IOType::ConstantSignal(_) => todo!(),
            IOType::Many => todo!(),
            graph::IOType::AnySignal(_) => panic!("AnySignals should be eradicated at this point."),
            graph::IOType::ConstantAny(_) => {
                panic!("ConstantAny should be eradicated at this point.")
            }
        }
    }

    fn get_signal_type(s: &str) -> fbo::SignalIDType {
        for line in BASE_SIGNALS.lines() {
            let (type_, sig) = line
                .split_once(':')
                .expect("Eact line in signale files should be formattet like this: type:signal");
            if s == sig {
                return match type_ {
                    "item" => fbo::SignalIDType::Item,
                    "fluid" => fbo::SignalIDType::Fluid,
                    "virtual" => fbo::SignalIDType::Virtual,
                    _ => panic!("Invalid signal type in signal file: `{}`.", type_),
                };
            } else if s == RESERVED_SIGNAL {
                return fbo::SignalIDType::Virtual;
            }
        }
        panic!(
            "Could not find signal: {}. The typechecker should have prevented this.",
            s
        );
    }
}

impl Display for FactorioCombinator {
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

pub fn convert_to_graph_to_blueprint_string(graph: Graph, verbose: bool) -> fb::Result<String> {
    let combinators = place_combinators(TurdMaster2000::new(graph));
    if verbose {
        println!("Combinators:");
        for c in &combinators {
            println!("\t{c}")
        }
        println!()
    }
    let container = combinators_to_blueprint(combinators);
    fb::BlueprintCodec::encode_string(&container)
}

fn place_combinators(placer: impl Placer) -> Vec<FactorioCombinator> {
    placer.place()
}

/// Create a [Blueprint] from a list of combinators
fn combinators_to_blueprint(combinators: Vec<FactorioCombinator>) -> fb::Container {
    let entities: Vec<fbo::Entity> = combinators
        .iter()
        .map(|c| c.to_blueprint_entity())
        .collect();
    let blueprint = create_blueprint(entities);
    fb::Container::Blueprint(blueprint)
}

/// Create a [Blueprint] containing some entities
fn create_blueprint(entities: Vec<fbo::Entity>) -> fbo::Blueprint {
    fbo::Blueprint {
        item: "Blueprint".to_string(),
        label: "Circuit".to_string(),
        label_color: None,
        entities,
        tiles: vec![],
        icons: vec![],
        schedules: vec![],
        version: 0,
    }
}
