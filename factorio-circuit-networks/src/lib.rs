use std::collections::{BTreeMap, HashMap};

use factorio_blueprint::objects as fbo;

#[derive(Debug)]
pub struct ConstantSignal {
    pub name: String,
    pub count: i32,
}

#[derive(Debug)]
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

#[derive(Debug, Clone)]
pub enum WireColor {
    Red,
    Green,
}

impl Copy for WireColor {}

pub type EntityConnections = Vec<(fbo::OneBasedIndex, fbo::EntityNumber, i32, WireColor)>;

#[derive(Debug)]
pub struct Entity {
    pub entity_number: fbo::EntityNumber,
    pub name: String,
    pub x: f32,
    pub y: f32,
    pub direction: fbo::Direction,
    pub connections: EntityConnections,
    pub kind: EntityKind,
}

#[derive(Debug)]
pub enum EntityKind {
    ArithmeticCombinator {
        left: EntitySignal,
        right: EntitySignal,
        output: EntitySignal,
        operator: fbo::ArithmeticOperation,
    },
    DeciderCombinator {
        left: EntitySignal,
        right: EntitySignal,
        output: EntitySignal,
        operator: fbo::DeciderComparator,
    },
    ConstantCombinator {
        signals: BTreeMap<fbo::OneBasedIndex, ConstantSignal>,
    },
    Other,
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
                EntityKind::ArithmeticCombinator {
                    left,
                    right,
                    output,
                    operator: c.operation.clone(),
                }
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
                let right = c
                    .second_signal
                    .as_ref()
                    .map_or(EntitySignal::unknown(), |s| EntitySignal::signal(&s.name));
                let output = c
                    .output_signal
                    .as_ref()
                    .map_or(EntitySignal::unknown(), |s| EntitySignal::signal(&s.name));

                EntityKind::DeciderCombinator {
                    left,
                    right,
                    output,
                    operator: c.comparator.clone(),
                }
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

                EntityKind::ConstantCombinator { signals }
            }
            _ => EntityKind::Other,
        };

        Entity {
            entity_number: fbo_entity.entity_number,
            name: fbo_entity.name,
            x: f64::from(fbo_entity.position.x) as f32,
            y: f64::from(fbo_entity.position.y) as f32,
            direction,
            connections,
            kind,
        }
    }
}
