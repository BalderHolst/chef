//! Converted combinator graphs to factorio blueprints.

use std::collections::HashMap;

use factorio_blueprint as fb;
use fb::BlueprintCodec;
use fb::objects::{self as fbo, ArithmeticConditions, EntityConnections, SignalID};
use fb::objects::{Blueprint, Entity, EntityNumber, Position, ControlBehavior, OneBasedIndex};

use fnv::FnvHashMap;

use crate::compiler::graph::{self, Graph, NId, ArithmeticOperation};

pub struct _BlueprintConverter {
    vid_to_entity_number: FnvHashMap<NId, EntityNumber>,
    graph: Graph,
}

impl _BlueprintConverter {
    pub fn _new(graph: Graph) -> Self { 
        let mut m: FnvHashMap<NId, EntityNumber> = FnvHashMap::default();

        // Create translation between vid and entity_number
        for (i, (vid, _node)) in graph.vertices.iter().enumerate() {
            m.insert(*vid, EntityNumber::new(i + 1).unwrap());
        }

        Self { 
            vid_to_entity_number: m,
            graph
        }
    }

    fn _create_blueprint(entities: Vec<Entity>) -> Blueprint {
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
    fn _iotype_to_signal_pair(t: graph::IOType) -> (Option<i32>, Option<SignalID>) {
        match t {
            graph::IOType::Signal(s) => (None, Some(SignalID {
                name: s,
                type_: fbo::SignalIDType::Virtual, // TODO
            })),
            graph::IOType::Constant(n) => (Some(n), None),
            graph::IOType::All => todo!(),
            graph::IOType::AnySignal(_) => panic!("AnySignals should be eradicated at this point."),
        }
    }

    fn _operation_to_operation_string(op: ArithmeticOperation) -> String {
        match op {
            ArithmeticOperation::Add => "+".to_string(),
            ArithmeticOperation::Subtract => "-".to_string(),
            ArithmeticOperation::Multiply => "*".to_string(),
            ArithmeticOperation::Divide => "/".to_string(),
        }
    }

    fn _connection_to_control_behavior(conn: graph::Connection) -> ControlBehavior {
        match conn {
            graph::Connection::Arithmetic(ac) => {
                let (first_constant, first_signal) = Self::_iotype_to_signal_pair(ac.left);
                let (second_constant, second_signal) = Self::_iotype_to_signal_pair(ac.right);
                let (_, output_signal) = Self::_iotype_to_signal_pair(ac.output);
                let operation = Self::_operation_to_operation_string(ac.operation);

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
            },
        }
    }

    fn _create_arithmetic_combinator(
        id: EntityNumber,
        position: Position,
        connections: Vec<EntityNumber>,
        operation: graph::Connection,
        ) -> Entity {

        let mut blueprint_connections: HashMap<EntityNumber, fbo::Connection> = HashMap::new();

        let connection_point_from = OneBasedIndex::new(1).unwrap();
        let connection_point_to = 2;

        for connected_entity in connections {
            blueprint_connections.insert(connection_point_from, fbo::Connection {
                red: None,
                green: Some(vec![fbo::ConnectionData {
                    entity_id: connected_entity,
                    circuit_id: Some(connection_point_to),
                }]),
            });
        }

        let control_behavior = Some(Self::_connection_to_control_behavior(operation));

        Entity {
            entity_number: id,
            name: "arithmetic-combinator".to_string(), // TODO: check
            position,
            direction: None,
            orientation: None,
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

    pub fn _convert_to_blueprint(&mut self) -> Blueprint {
        let bstring = "0eNq9k9FuwjAMRf/FrwsbDWxAfgVNVdp6YIkmVeKiVaj/PieVGAimiT3sJZKT65vro+QE1aHHLpBjMCeg2rsIZnuCSDtnD2mPhw7BADG2oMDZNlU2EO9bZKpntW8rcpZ9gFEBuQY/wRTjuwJ0TEw4GeZiKF3fVhhE8IuVgs5H6fYuZRDH2fL5VcEAZi63SEwO/lBWuLdHErlovn1KOW5yb0wHHxQilzcDHSlwLzvnIJNiloCkSSImm+QV2SY8cwW+w2CnUPAknb7nrn/AO2AD4zgN4LA+R9Rp2QVEd8mKGjBatBTqnjiXwjX13+DUD+Nc/BPOPPIdmvqa5ssfaNaDdXdxFj/iLK5x6oxTnmp+3ebiMyg4Yog5m14Xy9VGr942er5e6HH8AjHWHFw=";

        let blueprint = BlueprintCodec::decode_string(bstring).expect("Invalid Blueprint");
        dbg!(blueprint);

        todo!()
    }
}
