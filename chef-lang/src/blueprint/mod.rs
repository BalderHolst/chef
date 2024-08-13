//! Converted combinator graphs to factorio blueprints.

#![allow(dead_code)] // TODO: REMOVE

pub mod placement;
pub mod utils;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use factorio_blueprint as fb;
use factorio_blueprint::objects as fbo;

use fb::objects::OneBasedIndex;
use fnv::FnvHashMap;
use noisy_float::types::R64;

use crate::{
    compiler::{
        graph::{self, ArithmeticOperation, DeciderOperation, DetSig, WireKind},
        // TODO: remove reserved signal
        RESERVED_SIGNAL,
    },
    utils::BASE_SIGNALS,
};

use self::placement::PlacerName;

type Graph = graph::Graph<DetSig>;

type Operation = graph::Operation<DetSig>;
type FactorioConstant = i32;

pub(crate) type Coord = R64;

/// 2D Coordinate inside the Factorio world
pub(crate) type CoordSet = (Coord, Coord);

type ConnectionPoint = i32;

#[allow(dead_code)] // TODO: REMOVE
enum ConnectionPointKind {
    Input,
    Output,
}

/// An entity that can part of Factorio circuit networks
trait CircuitEntity {
    /// Get the input connection point for the entity
    fn input_conn_point(&self) -> usize;

    /// Get the output connection point for the entity
    fn output_conn_point(&self) -> usize;

    fn input_nid(&self) -> graph::NId;
    fn output_nid(&self) -> graph::NId;
    fn entity_number(&self) -> fbo::EntityNumber;
    fn output_entities(
        &self,
    ) -> &FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)>;
    fn output_entities_mut(
        &mut self,
    ) -> &mut FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)>;

    /// Returns the (width, height) of the entity
    fn dimensions(&self) -> (usize, usize);

    /// Returns the input coordinate to the entity
    fn input_pos(&self) -> CoordSet;

    /// Returns the output coordinate to the entity
    fn output_pos(&self) -> CoordSet;

    /// Converts the entity to a [fbo::Entity] to be included in a blueprint
    fn to_blueprint_entity(&self) -> fbo::Entity;
}

struct Substation {
    entity_number: fbo::EntityNumber,
    output_entities: FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)>,
    nid: graph::NId,
    position: CoordSet,
}

impl CircuitEntity for Substation {
    fn input_conn_point(&self) -> usize {
        1
    }

    fn output_conn_point(&self) -> usize {
        1
    }

    fn input_nid(&self) -> graph::NId {
        self.nid
    }

    fn output_nid(&self) -> graph::NId {
        self.nid
    }

    fn entity_number(&self) -> fbo::EntityNumber {
        self.entity_number
    }

    fn output_entities(
        &self,
    ) -> &FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &self.output_entities
    }

    fn output_entities_mut(
        &mut self,
    ) -> &mut FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &mut self.output_entities
    }

    fn dimensions(&self) -> (usize, usize) {
        (2, 2)
    }

    fn input_pos(&self) -> CoordSet {
        self.position
    }

    fn output_pos(&self) -> CoordSet {
        self.position
    }

    fn to_blueprint_entity(&self) -> fbo::Entity {
        let connections = to_factorio_conns(&self.output_entities, self.output_conn_point());
        fbo::Entity {
            entity_number: self.entity_number,
            name: "substation".to_string(),
            position: fbo::Position {
                x: self.position.0,
                y: self.position.1,
            },
            direction: None,
            orientation: None,
            connections: Some(fbo::EntityConnections::NumberIdx(connections)),
            control_behavior: None,
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
}

struct MediumElectricPole {
    entity_number: fbo::EntityNumber,
    output_entities: FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)>,
    nid: graph::NId,
    position: CoordSet,
}

impl CircuitEntity for MediumElectricPole {
    fn input_conn_point(&self) -> usize {
        1
    }

    fn output_conn_point(&self) -> usize {
        1
    }

    fn input_nid(&self) -> graph::NId {
        self.nid
    }

    fn output_nid(&self) -> graph::NId {
        self.nid
    }

    fn entity_number(&self) -> fbo::EntityNumber {
        self.entity_number
    }

    fn output_entities(
        &self,
    ) -> &FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &self.output_entities
    }

    fn output_entities_mut(
        &mut self,
    ) -> &mut FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &mut self.output_entities
    }

    fn dimensions(&self) -> (usize, usize) {
        (1, 1)
    }

    fn input_pos(&self) -> CoordSet {
        self.position
    }

    fn output_pos(&self) -> CoordSet {
        self.position
    }

    fn to_blueprint_entity(&self) -> fbo::Entity {
        let connections = to_factorio_conns(&self.output_entities, self.output_conn_point());
        fbo::Entity {
            entity_number: self.entity_number,
            name: "medium-electric-pole".to_string(),
            position: fbo::Position {
                x: self.position.0,
                y: self.position.1,
            },
            direction: None,
            orientation: None,
            connections: Some(fbo::EntityConnections::NumberIdx(connections)),
            control_behavior: None,
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
}

fn to_factorio_conns(
    output_entities: &FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)>,
    output_conn_point: usize,
) -> HashMap<OneBasedIndex, fbo::ConnectionPoint> {
    let mut connections_red = vec![];
    let mut connections_green = vec![];

    for (out_en, (to_conn_point, wires)) in output_entities.iter() {
        let conn_data = fbo::ConnectionData {
            entity_id: *out_en,
            circuit_id: Some(*to_conn_point),
        };
        match wires.len() {
            1 if wires.contains(&WireKind::Red) => {
                connections_red.push(conn_data);
            }
            1 if wires.contains(&WireKind::Green) => {
                connections_green.push(conn_data);
            }
            2 => {
                connections_red.push(conn_data.clone());
                connections_green.push(conn_data);
            }
            other => {
                panic!(
                    "Invalid number of wires for connection: {}. Should only be 1 or 2.",
                    other
                );
            }
        }
    }

    let mut connections = HashMap::new();
    connections.insert(
        OneBasedIndex::new(output_conn_point).unwrap(),
        fbo::Connection {
            red: Some(connections_red),
            green: Some(connections_green),
        },
    );

    connections
}

struct ConstantCombinator {
    entity_number: fbo::EntityNumber,
    nid: graph::NId,
    position: CoordSet,
    signals: Vec<(fbo::SignalID, FactorioConstant)>,
    output_entities: FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)>,
}

fn char_to_signal(c: char) -> (String, fbo::SignalIDType) {
    match c.to_uppercase().next() {
        Some(c) if c.is_ascii() && c.is_alphabetic() => {
            (format!("signal-{c}"), fbo::SignalIDType::Virtual)
        }
        _ => ("signal-unknown".to_string(), fbo::SignalIDType::Virtual),
    }
}

impl ConstantCombinator {
    pub fn new_sign(message: &str, en: OneBasedIndex, nid: graph::NId, position: CoordSet) -> Self {
        let signals: Vec<_> = message
            .chars()
            .map(char_to_signal)
            .map(|(sig, type_)| (fbo::SignalID { name: sig, type_ }, 0))
            .collect();

        Self {
            entity_number: en,
            nid,
            position,
            signals,
            output_entities: HashMap::default(),
        }
    }
}

impl CircuitEntity for ConstantCombinator {
    fn input_conn_point(&self) -> usize {
        1
    }

    fn output_conn_point(&self) -> usize {
        self.input_conn_point()
    }

    fn input_nid(&self) -> graph::NId {
        self.nid
    }

    fn output_nid(&self) -> graph::NId {
        self.nid
    }

    fn entity_number(&self) -> fbo::EntityNumber {
        self.entity_number
    }

    fn output_entities(
        &self,
    ) -> &FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &self.output_entities
    }

    fn output_entities_mut(
        &mut self,
    ) -> &mut FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &mut self.output_entities
    }

    fn dimensions(&self) -> (usize, usize) {
        (1, 1)
    }

    fn input_pos(&self) -> CoordSet {
        self.position
    }

    fn output_pos(&self) -> CoordSet {
        self.position
    }

    fn to_blueprint_entity(&self) -> fbo::Entity {
        let connections = to_factorio_conns(&self.output_entities, self.output_conn_point());

        fbo::Entity {
            entity_number: self.entity_number,
            name: "constant-combinator".to_string(),
            position: fbo::Position {
                x: self.position.0,
                y: self.position.1,
            },
            direction: None,
            orientation: None,
            connections: Some(fbo::EntityConnections::NumberIdx(connections)),
            control_behavior: Some(fbo::ControlBehavior {
                arithmetic_conditions: None,
                decider_conditions: None,
                filters: Some(
                    self.signals
                        .iter()
                        .enumerate()
                        .map(|(n, (signal_id, count))| fbo::ControlFilter {
                            signal: signal_id.clone(),
                            index: OneBasedIndex::try_from(n + 1).unwrap(),
                            count: *count,
                        })
                        .collect(),
                ),
                is_on: None,
            }),
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
}

/// Placed Factorio Combinator
#[derive(Clone, Debug, PartialEq)]
pub struct Combinator {
    pub entity_number: fbo::EntityNumber,
    pub input_nid: graph::NId,
    pub output_nid: graph::NId,
    pub operation: Operation,
    pub output_entities: FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)>,
    pub position: CoordSet,
}

macro_rules! each {
    () => {
        factorio_blueprint::objects::SignalID {
            name: "signal-each".to_string(),
            type_: fbo::SignalIDType::Virtual,
        }
    };
}

impl CircuitEntity for Combinator {
    fn input_conn_point(&self) -> usize {
        1
    }

    fn output_conn_point(&self) -> usize {
        2
    }

    fn input_nid(&self) -> graph::NId {
        self.input_nid
    }

    fn output_nid(&self) -> graph::NId {
        self.output_nid
    }

    fn entity_number(&self) -> fbo::EntityNumber {
        self.entity_number
    }

    fn output_entities(
        &self,
    ) -> &FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &self.output_entities
    }

    fn output_entities_mut(
        &mut self,
    ) -> &mut FnvHashMap<fbo::EntityNumber, (ConnectionPoint, HashSet<WireKind>)> {
        &mut self.output_entities
    }

    fn dimensions(&self) -> (usize, usize) {
        (1, 2)
    }

    fn input_pos(&self) -> CoordSet {
        (self.position.0, self.position.1 - 0.5)
    }

    fn output_pos(&self) -> CoordSet {
        (self.position.0, self.position.1 + 0.5)
    }

    fn to_blueprint_entity(&self) -> fbo::Entity {
        let connections = to_factorio_conns(&self.output_entities, self.output_conn_point());

        let control_behavior = self.get_control_behavior();

        // Internal Factorio name of combinator
        let name = match &self.operation {
            graph::Operation::Arithmetic(_) => "arithmetic-combinator",
            graph::Operation::Decider(_) => "decider-combinator",
            graph::Operation::Pick(_) => "arithmetic-combinator",
            graph::Operation::Convert(_) => "arithmetic-combinator",
            graph::Operation::Gate(_) => "decider-combinator",
            graph::Operation::Delay(_) => "arithmetic-combinator",
            graph::Operation::Sum(_) => "arithmetic-combinator",
        }
        .to_string();

        fbo::Entity {
            entity_number: self.entity_number,
            name,
            position: fbo::Position {
                x: self.position.0,
                y: self.position.1,
            },
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
}

impl Combinator {
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
                let (first_constant, first_signal) = Self::iotype_to_const_signal_pair(&pc.pick);
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

            graph::Operation::Convert(cc) => {
                let (first_constant, first_signal) = Self::iotype_to_const_signal_pair(&cc.input);
                let (second_constant, second_signal) = (Some(0), None);
                let (_, output_signal) = Self::iotype_to_const_signal_pair(&cc.output);
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
            DeciderOperation::Equal => "=",
            DeciderOperation::NotEqual => "!=",
            DeciderOperation::EveryEqual => todo!(),
            DeciderOperation::EveryLargerThan => todo!(),
            DeciderOperation::EveryLargerThanOrEqual => todo!(),
            DeciderOperation::EveryLessThan => todo!(),
            DeciderOperation::EveryLessThanOrEqual => todo!(),
            DeciderOperation::EveryNotEqual => todo!(),
            DeciderOperation::AnyEqual => todo!(),
            DeciderOperation::AnyLargerThan => todo!(),
            DeciderOperation::AnyLargerThanOrEqual => todo!(),
            DeciderOperation::AnyLessThan => todo!(),
            DeciderOperation::AnyLessThanOrEqual => todo!(),
            DeciderOperation::AnyNotEqual => todo!(),
        }
        .to_string()
    }

    /// Returns (first_constant, first_signal)
    fn iotype_to_const_signal_pair(t: &graph::DetSig) -> (Option<i32>, Option<fbo::SignalID>) {
        match t {
            graph::DetSig::Signal(s) => {
                let type_ = Self::get_signal_type(s.as_str());
                (
                    None,
                    Some(fbo::SignalID {
                        name: s.clone(),
                        type_,
                    }),
                )
            }
            graph::DetSig::ConstantSignal((s, n)) => {
                let type_ = Self::get_signal_type(s.as_str());
                (
                    Some(*n),
                    Some(fbo::SignalID {
                        name: s.clone(),
                        type_,
                    }),
                )
            }
            graph::DetSig::Constant(n) => (Some(*n), None),
            graph::DetSig::Many => (None, Some(each!())),
        }
    }

    // TODO: Convert return type to union
    // Get the corresponding (signal_type, signal_string) pair
    fn _iotype_to_signal_pair(t: DetSig) -> (fbo::SignalIDType, String) {
        match t {
            DetSig::Signal(s) => (Self::get_signal_type(s.as_str()), s),
            DetSig::Constant(_) => todo!(),
            DetSig::ConstantSignal(_) => todo!(),
            DetSig::Many => todo!(),
        }
    }

    fn get_signal_type(s: &str) -> fbo::SignalIDType {
        for line in BASE_SIGNALS.lines() {
            let (type_, sig) = line
                .split_once(':')
                // TODO: better error handling
                .expect("Each line in signale files should be formattet like this: type:signal");
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

impl Display for Combinator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {} (input for: {:?}):\t[{}] {} {{{:?}}}",
            self.input_nid,
            self.output_nid,
            self.output_entities.keys().collect::<Vec<_>>(),
            self.entity_number,
            self.operation,
            self.position
        )
    }
}

/// The maxinum distanct a wire can connect two points in factorio.
const WIRE_RANGE: f64 = 9.0;

pub fn convert_to_graph_to_blueprint_string(
    placer: &PlacerName,
    graph: Graph,
) -> fb::Result<String> {
    let combinators = placer.place(graph);
    let container = combinators_to_blueprint(combinators);
    fb::BlueprintCodec::encode_string(&container)
}

/// Create a [Blueprint] from a list of combinators
fn combinators_to_blueprint(entities: Vec<fbo::Entity>) -> fb::Container {
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
