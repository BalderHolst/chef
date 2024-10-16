use std::collections::{BTreeMap, HashMap};

use factorio_blueprint::objects::{self as fbo, ConnectionPoint};
use noisy_float::prelude::*;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantSignal {
    pub name: String,
    pub count: i32,
}

impl ConstantSignal {
    pub fn new<S>(name: S, count: i32) -> Self
    where
        S: ToString,
    {
        Self {
            name: name.to_string(),
            count,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EntitySignal {
    Signal(String),
    Constant(i32),
}

impl EntitySignal {
    pub fn signal<S>(s: S) -> Self
    where
        S: ToString,
    {
        Self::Signal(s.to_string())
    }

    pub fn constant(c: i32) -> Self {
        Self::Constant(c)
    }

    pub fn unknown() -> Self {
        Self::Signal("UNKNOWN".to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WireColor {
    Red,
    Green,
}

impl Copy for WireColor {}

pub type EntityConnections = Vec<(fbo::OneBasedIndex, fbo::EntityNumber, i32, WireColor)>;

#[derive(Debug, Clone, PartialEq)]
pub struct Entity {
    pub entity_number: fbo::EntityNumber,
    pub x: f32,
    pub y: f32,
    pub direction: fbo::Direction,
    pub connections: EntityConnections,
    pub kind: EntityKind,
}

impl Entity {
    pub fn name(&self) -> &str {
        self.kind.name()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArithmeticCombinator {
    pub left: EntitySignal,
    pub right: EntitySignal,
    pub output: EntitySignal,
    pub operator: fbo::ArithmeticOperation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeciderCombinator {
    pub left: EntitySignal,
    pub right: EntitySignal,
    pub output: EntitySignal,
    pub operator: fbo::DeciderComparator,
    pub copy_count_from_input: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantCombinator {
    pub signals: BTreeMap<fbo::OneBasedIndex, ConstantSignal>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EntityKind {
    ArithmeticCombinator(ArithmeticCombinator),
    DeciderCombinator(DeciderCombinator),
    ConstantCombinator(ConstantCombinator),
    Other(String),
}

impl EntityKind {
    pub fn name(&self) -> &str {
        match self {
            EntityKind::ArithmeticCombinator(_) => "arithmetic-combinator",
            EntityKind::DeciderCombinator(_) => "decider-combinator",
            EntityKind::ConstantCombinator(_) => "constant-combinator",
            EntityKind::Other(name) => name.as_str(),
        }
    }
}

impl From<Entity> for fbo::Entity {
    fn from(entity: Entity) -> Self {
        let mut connections = HashMap::new();

        for (this_port, other_en, other_port, wire) in &entity.connections {
            let data = fbo::ConnectionData {
                entity_id: *other_en,
                circuit_id: Some(*other_port),
                wire_id: None,
            };

            connections
                .entry(*this_port)
                .and_modify(|c: &mut ConnectionPoint| match wire {
                    WireColor::Red => {
                        if let Some(v) = &mut c.red {
                            v.push(data.clone())
                        } else {
                            c.red = Some(vec![data.clone()])
                        }
                    }
                    WireColor::Green => {
                        if let Some(v) = &mut c.green {
                            v.push(data.clone())
                        } else {
                            c.green = Some(vec![data.clone()])
                        }
                    }
                })
                .or_insert({
                    let (red, green) = match wire {
                        WireColor::Red => (Some(vec![data]), None),
                        WireColor::Green => (None, Some(vec![data])),
                    };
                    fbo::ConnectionPoint { red, green }
                });
        }

        let connections = Some(fbo::EntityConnections::NumberIdx(
            connections
                .into_iter()
                .map(|(from_port, point)| (from_port, fbo::Connection::Single(point)))
                .collect(),
        ));

        let mut fbo_entity = fbo::Entity {
            entity_number: entity.entity_number,
            name: entity.name().to_string(),
            position: fbo::Position {
                x: r64(entity.x.into()),
                y: r64(entity.y.into()),
            },
            direction: Some(entity.direction),
            orientation: None,
            connections,
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
            switch_state: None,
            manual_trains_limit: None,
            neighbours: None,
        };

        let mut control_behaviour = fbo::ControlBehavior {
            connect_to_logistic_network: None,
            arithmetic_conditions: None,
            decider_conditions: None,
            logistic_condition: None,
            filters: None,
            is_on: None,
            use_colors: None,
            circuit_condition: None,
            circuit_mode_of_operation: None,
            circuit_enable_disable: None,
            circuit_contents_read_mode: None,
            circuit_hand_read_mode: None,
            circuit_read_hand_contents: None,
            circuit_set_stack_size: None,
            stack_control_input_signal: None,
            circuit_parameters: None,
            output_signal: None,
            read_from_train: None,
            read_stopped_train: None,
            read_trains_count: None,
            set_trains_limit: None,
            send_to_train: None,
            train_stopped_signal: None,
            trains_count_signal: None,
            trains_limit_signal: None,
            read_logistics: None,
            read_robot_stats: None,
            available_construction_output_signal: None,
            available_logistic_output_signal: None,
            total_construction_output_signal: None,
            total_logistic_output_signal: None,
            circuit_open_gate: None,
            circuit_read_sensor: None,
            circuit_close_signal: None,
            circuit_read_signal: None,
        };

        fn entity_signal_to_signal_pair(
            signal: EntitySignal,
        ) -> (Option<i32>, Option<fbo::SignalID>) {
            match signal {
                EntitySignal::Signal(s) => (
                    None,
                    Some(fbo::SignalID {
                        type_: fbo::SignalIDType::Item, // TODO: Make this dynamic
                        name: s,
                    }),
                ),
                EntitySignal::Constant(c) => (Some(c), None),
            }
        }

        match entity.kind {
            EntityKind::ArithmeticCombinator(ArithmeticCombinator {
                left,
                right,
                output,
                operator,
            }) => {
                let (first_constant, first_signal) = entity_signal_to_signal_pair(left);
                let (second_constant, second_signal) = entity_signal_to_signal_pair(right);
                let output_signal = entity_signal_to_signal_pair(output).1;
                control_behaviour.arithmetic_conditions = Some(fbo::ArithmeticConditions {
                    first_constant,
                    first_signal,
                    second_constant,
                    second_signal,
                    operation: operator,
                    output_signal,
                });
            }
            EntityKind::DeciderCombinator(DeciderCombinator {
                left,
                right,
                output,
                operator,
                copy_count_from_input,
            }) => {
                let (first_constant, first_signal) = entity_signal_to_signal_pair(left);
                assert_eq!(
                    first_constant, None,
                    "Decider Combinators cannot have constant on left input"
                );

                let (constant, second_signal) = entity_signal_to_signal_pair(right);
                let output_signal = entity_signal_to_signal_pair(output).1;
                control_behaviour.decider_conditions = Some(fbo::DeciderConditions {
                    first_signal,
                    second_signal,
                    constant,
                    output_signal,
                    comparator: operator,
                    copy_count_from_input: copy_count_from_input.then_some(true),
                });
            }
            EntityKind::ConstantCombinator(ConstantCombinator { signals }) => {
                control_behaviour.filters = Some(
                    signals
                        .into_iter()
                        .map(|(index, signal)| fbo::ControlFilter {
                            index,
                            count: signal.count,
                            signal: fbo::SignalID {
                                type_: fbo::SignalIDType::Item, // TODO: Make this dynamic
                                name: signal.name,
                            },
                        })
                        .collect(),
                );
            }
            EntityKind::Other(_) => (),
        }

        fbo_entity.control_behavior = Some(control_behaviour);

        fbo_entity
    }
}

impl From<fbo::Entity> for Entity {
    fn from(fbo_entity: fbo::Entity) -> Self {
        let connections = (|| {
            let conns = match fbo_entity.connections?.clone() {
                fbo::EntityConnections::StringIdx(conns) => conns
                    .iter()
                    .map(|(port, conn)| {
                        let port = port.parse::<fbo::OneBasedIndex>().unwrap();
                        (port, conn.clone())
                    })
                    .collect::<HashMap<fbo::OneBasedIndex, fbo::Connection>>(),
                fbo::EntityConnections::NumberIdx(conns) => conns,
            };

            let conns = conns.into_iter().map(|(port, c)| match c {
                fbo::Connection::Single(c) => (port, c),
                fbo::Connection::Multiple(_) => todo!(),
            });

            Some({
                conns
                    .into_iter()
                    .flat_map(|(port, conn)| {
                        conn.red
                            .map(move |red_cons| {
                                red_cons.into_iter().map(move |red_conn| {
                                    (
                                        port,
                                        red_conn.entity_id,
                                        red_conn.circuit_id.unwrap_or(1),
                                        WireColor::Red,
                                    )
                                })
                            })
                            .into_iter()
                            .flatten()
                            .chain(
                                conn.green
                                    .map(move |green_cons| {
                                        green_cons.into_iter().map(move |green_conn| {
                                            (
                                                port,
                                                green_conn.entity_id,
                                                green_conn.circuit_id.unwrap_or(1),
                                                WireColor::Green,
                                            )
                                        })
                                    })
                                    .into_iter()
                                    .flatten(),
                            )
                    })
                    .collect::<Vec<_>>()
            })
        })()
        .unwrap_or(vec![]);

        let direction = fbo_entity.direction.unwrap_or_default();

        let kind = match fbo_entity.name.as_str() {
            "arithmetic-combinator" => {
                let c = fbo_entity
                    .control_behavior
                    .as_ref()
                    .unwrap()
                    .arithmetic_conditions
                    .as_ref()
                    .unwrap();
                let left = c.first_signal.as_ref().map_or(
                    c.first_constant
                        .map_or(EntitySignal::unknown(), EntitySignal::constant),
                    |s| EntitySignal::signal(&s.name),
                );
                let right = c.second_signal.as_ref().map_or(
                    c.second_constant
                        .map_or(EntitySignal::unknown(), EntitySignal::constant),
                    |s| EntitySignal::signal(&s.name),
                );
                let output = c
                    .output_signal
                    .as_ref()
                    .map_or(EntitySignal::unknown(), |s| EntitySignal::signal(&s.name));
                EntityKind::ArithmeticCombinator(ArithmeticCombinator {
                    left,
                    right,
                    output,
                    operator: c.operation.clone(),
                })
            }
            "decider-combinator" => {
                let c = fbo_entity
                    .control_behavior
                    .as_ref()
                    .unwrap()
                    .decider_conditions
                    .as_ref()
                    .unwrap();
                let left = c
                    .first_signal
                    .as_ref()
                    .map_or(EntitySignal::unknown(), |s| EntitySignal::signal(&s.name));
                let right = c.second_signal.as_ref().map_or(
                    c.constant
                        .map_or(EntitySignal::unknown(), EntitySignal::constant),
                    |s| EntitySignal::signal(&s.name),
                );
                let output = c
                    .output_signal
                    .as_ref()
                    .map_or(EntitySignal::unknown(), |s| EntitySignal::signal(&s.name));

                EntityKind::DeciderCombinator(DeciderCombinator {
                    left,
                    right,
                    output,
                    operator: c.comparator.clone(),
                    copy_count_from_input: c.copy_count_from_input.unwrap_or_default(),
                })
            }
            "constant-combinator" => {
                let signals = (|| {
                    Some(
                        fbo_entity
                            .control_behavior?
                            .filters?
                            .into_iter()
                            .map(|filter| {
                                (
                                    filter.index,
                                    ConstantSignal {
                                        name: filter.signal.name,
                                        count: filter.count,
                                    },
                                )
                            })
                            .collect::<BTreeMap<fbo::OneBasedIndex, ConstantSignal>>(),
                    )
                })()
                .unwrap_or(BTreeMap::default());

                EntityKind::ConstantCombinator(ConstantCombinator { signals })
            }
            _ => EntityKind::Other(fbo_entity.name),
        };

        Entity {
            entity_number: fbo_entity.entity_number,
            x: f64::from(fbo_entity.position.x) as f32,
            y: f64::from(fbo_entity.position.y) as f32,
            direction,
            connections,
            kind,
        }
    }
}
