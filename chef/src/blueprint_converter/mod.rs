//! Converted combinator graphs to factorio blueprints.

pub mod blueprint_graph;
pub mod placement;

use std::collections::HashMap;

use factorio_blueprint as fb;
use fb::objects::{self as fbo, ArithmeticConditions, EntityConnections, SignalID};
use fb::objects::{Blueprint, ControlBehavior, Entity, EntityNumber, OneBasedIndex, Position};
use fb::BlueprintCodec;
use noisy_float::types::R64;

use crate::blueprint_converter::blueprint_graph::BlueprintGraph;
use crate::compiler::graph::{self, ArithmeticConnection, ArithmeticOperation, Graph, IOType, NId};

pub struct BlueprintConverter {}

/// The maxinum distanct a wire can connect two points in factorio.
const WIRE_RANGE: f64 = 10.0;

impl BlueprintConverter {
    pub fn new() -> Self {
        Self {}
    }

    /// Create an empty circuit blueprint
    fn create_blueprint(entities: Vec<Entity>) -> Blueprint {
        Blueprint {
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

    /// Returns (first_constant, first_signal)
    fn iotype_to_signal_pair(t: graph::IOType) -> (Option<i32>, Option<SignalID>) {
        match t {
            graph::IOType::Signal(s) => (
                None,
                Some(SignalID {
                    name: s,
                    type_: fbo::SignalIDType::Virtual, // TODO
                }),
            ),
            graph::IOType::Constant(n) => (Some(n), None),
            graph::IOType::All => todo!(),
            graph::IOType::AnySignal(_) => panic!("AnySignals should be eradicated at this point."),
        }
    }

    fn operation_to_op_string(op: ArithmeticOperation) -> String {
        match op {
            ArithmeticOperation::Add => "+".to_string(),
            ArithmeticOperation::Subtract => "-".to_string(),
            ArithmeticOperation::Multiply => "*".to_string(),
            ArithmeticOperation::Divide => "/".to_string(),
        }
    }

    fn connection_to_control_behavior(conn: graph::Connection) -> ControlBehavior {
        match conn {
            graph::Connection::Arithmetic(ac) => {
                let (first_constant, first_signal) = Self::iotype_to_signal_pair(ac.left);
                let (second_constant, second_signal) = Self::iotype_to_signal_pair(ac.right);
                let (_, output_signal) = Self::iotype_to_signal_pair(ac.output);
                let operation = Self::operation_to_op_string(ac.operation);

                ControlBehavior {
                    arithmetic_conditions: Some(ArithmeticConditions {
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
            graph::Connection::Decider(_) => todo!(),
            graph::Connection::Gate(_) => todo!(),
        }
    }

    fn create_arithmetic_combinator(
        id: EntityNumber,
        position: Position,
        connections: Vec<EntityNumber>,
        operation: graph::Connection,
    ) -> Entity {
        let mut blueprint_connections: HashMap<EntityNumber, fbo::Connection> = HashMap::new();

        let connection_point_from = OneBasedIndex::new(1).unwrap();
        let connection_point_to = 2;

        for connected_entity in connections {
            blueprint_connections.insert(
                connection_point_from,
                fbo::Connection {
                    red: None,
                    green: Some(vec![fbo::ConnectionData {
                        entity_id: connected_entity,
                        circuit_id: Some(connection_point_to),
                    }]),
                },
            );
        }

        let control_behavior = Some(Self::connection_to_control_behavior(operation));

        Entity {
            entity_number: id,
            name: "arithmetic-combinator".to_string(),
            position,
            direction: None,
            orientation: None,

            // Connections are block recorded in both the connected entities.
            //
            // connections: Some(
            // StringIdx(
            //   {
            //     "2": ConnectionPoint { // The id of the connection point on THIS combinator
            //                            // "1" = input & "2" -> output
            //       red: None,
            //       green: Some(
            //         [
            //           ConnectionData {
            //             entity_id: 2,  // The BLUEPRINT id of the connected entity
            //             circuit_id: Some( // The connection point id on the OTHER entity
            //               1,
            //             ),
            //           },
            //         ],
            //       ),
            //     },
            //   },
            // ),
            // ),
            connections: Some(EntityConnections::NumberIdx(blueprint_connections)),
            control_behavior,
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

    pub fn convert_to_blueprint(&mut self, graph: Graph) {
        let mut bp_graph = BlueprintGraph::from_graph(graph);
        placement::place_combinators(&mut bp_graph);
        bp_graph.visualize("fgraph.svg").unwrap();

        // self.test();
    }

    fn test(&self) {
        use crate::cli;
        // let bstring = "0eNq9k9FuwjAMRf/FrwsbDWxAfgVNVdp6YIkmVeKiVaj/PieVGAimiT3sJZKT65vro+QE1aHHLpBjMCeg2rsIZnuCSDtnD2mPhw7BADG2oMDZNlU2EO9bZKpntW8rcpZ9gFEBuQY/wRTjuwJ0TEw4GeZiKF3fVhhE8IuVgs5H6fYuZRDH2fL5VcEAZi63SEwO/lBWuLdHErlovn1KOW5yb0wHHxQilzcDHSlwLzvnIJNiloCkSSImm+QV2SY8cwW+w2CnUPAknb7nrn/AO2AD4zgN4LA+R9Rp2QVEd8mKGjBatBTqnjiXwjX13+DUD+Nc/BPOPPIdmvqa5ssfaNaDdXdxFj/iLK5x6oxTnmp+3ebiMyg4Yog5m14Xy9VGr942er5e6HH8AjHWHFw=";
        // let bstring = "0eNq9k2FrgzAQhv/LfV3cqrXY5q+MIlFv7YEmkpwykfz3JQpdRwvDfdiXwCXvvXnvIZmhagfsLWkGOQPVRjuQ7zM4umjVxj2eegQJxNiBAK26WClLfO2QqU5q01WkFRsLXgDpBj9Bpv4sADUTE66GSzGVeugqtEHwi5WA3rjQbXTMEByT/PUgYAK5C7eEmGxNW1Z4VSMFedB8+5ThuFl6XTz4IOu4fBhoJMtD2LkFWRVJBBIncRhtopdjFfHsBJgerVpDwUvoNAP3wwZviw14vw6gsb5FzOJysYj6nhU1ILOgJVsPxGvpz7H/AWe2Gef+n3AuIz+hmf2k+fYHmvWk9Eac6TOc4akur1vefQYBI1q3ZMuOaV6csqLYF6fDMff+CzP3HGs=";
        let bstring = "0eNrNU8tqwzAQ/JWyx6KUyE5won/IpdcSjGwvyYItGUkOMUb/3pUNbfqAPi7tRTC7M6MdpJ2gagfsHZkAagKqrfGgnibwdDK6TbUw9ggKKGAHAozuEtKOwrnDQPWqtl1FRgfrIAog0+AVlIxHAWgCBcLFcAZjaYauQseEL6wE9Naz2po0AzsWD1sBI6iVlHwNzxmcbcsKz/pCzGfSq1HJ7WYW+9TwmHAq+qBT0LUA26PTiz3cQ1wsDdYvoiwdJ4dobsenBlTGXHL1QGGB8RhZ/yFh9tuE6z/OJ9/nE2/a+ffi5z+NL/9L/k/fl7/zvAHqZmEEtLpCXhI46OvdI+r6zLULOj/fm+3kpthnRZEX++1uE+MzyiovNA==";

        let parsed = BlueprintCodec::decode_string(bstring).expect("Invalid Blueprint");

        cli::print_label("PARSED");
        dbg!(parsed);

        cli::print_label("CREATED");
        let op = graph::Connection::Arithmetic(ArithmeticConnection::new(
            IOType::Signal("signal-blue".to_string()),
            IOType::Constant(0),
            ArithmeticOperation::Add,
            IOType::Signal("signal-red".to_string()),
        ));
        let x = R64::new(-4.5);
        let y = R64::new(0.0);
        dbg!(Self::create_arithmetic_combinator(
            EntityNumber::new(1).unwrap(),
            Position { x, y },
            vec![],
            op
        ));
    }
}
