//! Converted combinator graphs to factorio blueprints.

pub mod blueprint_graph;
pub mod placement;

use std::collections::HashMap;
use std::num::NonZeroUsize;

use factorio_blueprint as fb;
use fb::objects::{
    self as fbo, ArithmeticConditions, DeciderConditions, EntityConnections, SignalID, SignalIDType,
};
use fb::objects::{Blueprint, ControlBehavior, Entity, EntityNumber};
use fb::Container;

use crate::blueprint_converter::blueprint_graph::BlueprintGraph;
use crate::compiler::graph::{self, ArithmeticOperation, DeciderOperation, Graph};
use crate::utils::BASE_SIGNALS;

use self::blueprint_graph::Combinator;

pub struct BlueprintConverter {
    graph: BlueprintGraph,
}

/// The maxinum distanct a wire can connect two points in factorio.
const WIRE_RANGE: f64 = 9.0;

impl BlueprintConverter {
    pub fn new(graph: Graph) -> Self {
        let mut bp_graph = BlueprintGraph::from_graph(graph);
        placement::place_combinators(&mut bp_graph);
        Self { graph: bp_graph }
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

    fn get_signal_type(s: &str) -> SignalIDType {
        for line in BASE_SIGNALS.lines() {
            let (type_, sig) = line
                .split_once(':')
                .expect("Eact line in signale files should be formattet like this: type:signal");
            if s == sig {
                return match type_ {
                    "item" => SignalIDType::Item,
                    "fluid" => SignalIDType::Fluid,
                    "virtual" => SignalIDType::Virtual,
                    _ => panic!("Invalid signal type in signal file: `{}`.", type_),
                };
            }
        }
        panic!(
            "Could not find signal: {}. The typechecker should have prevented this.",
            s
        );
    }

    /// Returns (first_constant, first_signal)
    fn iotype_to_signal_pair(t: &graph::IOType) -> (Option<i32>, Option<SignalID>) {
        match t {
            graph::IOType::Signal(s) => {
                let type_ = Self::get_signal_type(s.as_str());
                (
                    None,
                    Some(SignalID {
                        name: s.clone(),
                        type_, // TODO
                    }),
                )
            }
            graph::IOType::ConstantSignal(_) => todo!(),
            graph::IOType::Constant(n) => (Some(*n), None),
            graph::IOType::All => todo!(),
            graph::IOType::AnySignal(_) => panic!("AnySignals should be eradicated at this point."),
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
        }
        .to_string()
    }

    fn connection_to_control_behavior(conn: &graph::Connection) -> ControlBehavior {
        match conn {
            graph::Connection::Arithmetic(ac) => {
                let (first_constant, first_signal) = Self::iotype_to_signal_pair(&ac.left);
                let (second_constant, second_signal) = Self::iotype_to_signal_pair(&ac.right);
                let (_, output_signal) = Self::iotype_to_signal_pair(&ac.output);
                let operation = Self::arithmetic_operation_to_op_string(&ac.operation);

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
            graph::Connection::Decider(dc) => {
                let (_first_constant, first_signal) = Self::iotype_to_signal_pair(&dc.left);
                let (second_constant, second_signal) = Self::iotype_to_signal_pair(&dc.right);
                let (_, output_signal) = Self::iotype_to_signal_pair(&dc.output);
                let operation = Self::decider_operation_to_op_string(&dc.operation);

                ControlBehavior {
                    arithmetic_conditions: None,
                    decider_conditions: Some(DeciderConditions {
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
            graph::Connection::Gate(gc) => {
                let (_first_constant, first_signal) = Self::iotype_to_signal_pair(&gc.left);
                let (second_constant, second_signal) = Self::iotype_to_signal_pair(&gc.right);
                let (_, gate_signal) = Self::iotype_to_signal_pair(&gc.gate_type);
                let operation = Self::decider_operation_to_op_string(&gc.operation);

                ControlBehavior {
                    arithmetic_conditions: None,
                    decider_conditions: Some(DeciderConditions {
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
            graph::Connection::Constant(_) => todo!(),
        }
    }

    fn combinator_to_entity(&self, com: Combinator) -> Entity {
        let mut blueprint_connections: HashMap<EntityNumber, fbo::Connection> = HashMap::new();

        if let Some(inputs) = self.graph.wires.get(&com.from) {
            for in_nid in inputs {
                let other_com = self.graph.get_corresponding_combinator(*in_nid);

                // Connect to the input (id 1) connection point if THIS conbinator.
                blueprint_connections.insert(NonZeroUsize::new(1).unwrap(), {
                    fbo::Connection {
                        red: None,
                        green: Some(vec![fbo::ConnectionData {
                            // Entity number of other conbinator
                            entity_id: other_com.entity_number,
                            // Connect to the output of the other combinator
                            circuit_id: Some(2),
                        }]),
                    }
                });
            }
        }

        // Do the same for outputs
        if let Some(outputs) = self.graph.wires.get(&com.to) {
            for out_nid in outputs {
                let other_com = self.graph.get_corresponding_combinator(*out_nid);

                // Connect to the OUTPUT (id 2) connection point if THIS conbinator.
                blueprint_connections.insert(NonZeroUsize::new(2).unwrap(), {
                    fbo::Connection {
                        red: None,
                        green: Some(vec![fbo::ConnectionData {
                            // Entity number of other conbinator
                            entity_id: other_com.entity_number,
                            // Connect to the INPUT of the other combinator
                            circuit_id: Some(1),
                        }]),
                    }
                });
            }
        }

        let control_behavior = Some(Self::connection_to_control_behavior(&com.operation));

        let name = match &com.operation {
            graph::Connection::Arithmetic(_) => "arithmetic-combinator",
            graph::Connection::Decider(_) => "decider-combinator",
            graph::Connection::Gate(_) => "decider-combinator",
            graph::Connection::Constant(_) => "constant-combinator",
        }
        .to_string();

        Entity {
            entity_number: com.entity_number,
            name,
            position: com
                .position
                .expect("Combinators should all be placed at this point")
                .factorio_pos(),
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

    pub fn convert_to_blueprint(&mut self) -> Container {
        let mut entities: Vec<Entity> = vec![];
        for com in &self.graph.combinators {
            entities.push(self.combinator_to_entity(com.clone()));
        }
        let blueprint = Self::create_blueprint(entities);
        Container::Blueprint(blueprint)
    }

    pub fn convert_to_blueprint_string(
        &mut self,
        opts: &crate::cli::CookOpts,
    ) -> fb::Result<String> {
        let container = self.convert_to_blueprint();
        if let Some(path) = &opts.fgraph {
            if self.graph.visualize(path.as_str()).is_err() {
                eprintln!("Could not visualize factorio graph.");
            }
        }
        fb::BlueprintCodec::encode_string(&container)
    }

    // fn test(&self) {
    //     use crate::cli;
    //     // let bstring = "0eNq9k9FuwjAMRf/FrwsbDWxAfgVNVdp6YIkmVeKiVaj/PieVGAimiT3sJZKT65vro+QE1aHHLpBjMCeg2rsIZnuCSDtnD2mPhw7BADG2oMDZNlU2EO9bZKpntW8rcpZ9gFEBuQY/wRTjuwJ0TEw4GeZiKF3fVhhE8IuVgs5H6fYuZRDH2fL5VcEAZi63SEwO/lBWuLdHErlovn1KOW5yb0wHHxQilzcDHSlwLzvnIJNiloCkSSImm+QV2SY8cwW+w2CnUPAknb7nrn/AO2AD4zgN4LA+R9Rp2QVEd8mKGjBatBTqnjiXwjX13+DUD+Nc/BPOPPIdmvqa5ssfaNaDdXdxFj/iLK5x6oxTnmp+3ebiMyg4Yog5m14Xy9VGr942er5e6HH8AjHWHFw=";
    //     // let bstring = "0eNq9k2FrgzAQhv/LfV3cqrXY5q+MIlFv7YEmkpwykfz3JQpdRwvDfdiXwCXvvXnvIZmhagfsLWkGOQPVRjuQ7zM4umjVxj2eegQJxNiBAK26WClLfO2QqU5q01WkFRsLXgDpBj9Bpv4sADUTE66GSzGVeugqtEHwi5WA3rjQbXTMEByT/PUgYAK5C7eEmGxNW1Z4VSMFedB8+5ThuFl6XTz4IOu4fBhoJMtD2LkFWRVJBBIncRhtopdjFfHsBJgerVpDwUvoNAP3wwZviw14vw6gsb5FzOJysYj6nhU1ILOgJVsPxGvpz7H/AWe2Gef+n3AuIz+hmf2k+fYHmvWk9Eac6TOc4akur1vefQYBI1q3ZMuOaV6csqLYF6fDMff+CzP3HGs=";
    //     let bstring = "0eNrNU8tqwzAQ/JWyx6KUyE5won/IpdcSjGwvyYItGUkOMUb/3pUNbfqAPi7tRTC7M6MdpJ2gagfsHZkAagKqrfGgnibwdDK6TbUw9ggKKGAHAozuEtKOwrnDQPWqtl1FRgfrIAog0+AVlIxHAWgCBcLFcAZjaYauQseEL6wE9Naz2po0AzsWD1sBI6iVlHwNzxmcbcsKz/pCzGfSq1HJ7WYW+9TwmHAq+qBT0LUA26PTiz3cQ1wsDdYvoiwdJ4dobsenBlTGXHL1QGGB8RhZ/yFh9tuE6z/OJ9/nE2/a+ffi5z+NL/9L/k/fl7/zvAHqZmEEtLpCXhI46OvdI+r6zLULOj/fm+3kpthnRZEX++1uE+MzyiovNA==";

    // let parsed = BlueprintCodec::decode_string(bstring).expect("Invalid Blueprint");

    // cli::print_label("PARSED");
    // dbg!(parsed);

    //         cli::print_label("CREATED");
    //         let op = graph::Connection::Arithmetic(ArithmeticConnection::new(
    //             IOType::Signal("signal-blue".to_string()),
    //             IOType::Constant(0),
    //             ArithmeticOperation::Add,
    //             IOType::Signal("signal-red".to_string()),
    //         ));
    //         let x = R64::new(-4.5);
    //         let y = R64::new(0.0);
    //         dbg!(Self::create_arithmetic_combinator(
    //             EntityNumber::new(1).unwrap(),
    //             Position { x, y },
    //             vec![],
    //             op
    //         ));
    //     }
}
