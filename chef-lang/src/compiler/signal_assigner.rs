use std::collections::HashSet;

use fnv::FnvHashMap;

use crate::{
    compiler::graph::{ConvertOp, DeciderOp, DelayOp, Node, PickOp, SumOp},
    utils::BASE_SIGNALS,
};

use super::graph::{ArithmeticOp, Connection, DetSig, GateOp, Graph, LooseSig, Operation};

pub fn assign_signals(graph: &Graph<LooseSig>) -> Graph<DetSig> {
    AnysignalAssigner::assign(graph)
}

struct AnysignalAssigner<'a> {
    input_graph: &'a Graph<LooseSig>,
    output_graph: Graph<DetSig>,
    anysignal_to_signal: FnvHashMap<u64, String>,
    next_sig_nr: u64,
    used_signals: HashSet<String>,
    signal_names: Vec<&'static str>,
}

impl<'a> AnysignalAssigner<'a> {
    fn new(graph: &'a Graph<LooseSig>) -> Self {
        let signal_names: Vec<&str> = BASE_SIGNALS
            .lines()
            .map(|l| {
                l.split_once(':')
                    .expect("there should always be a ':' denoting type:signal")
                    .1
            })
            .collect();
        Self {
            input_graph: graph,
            output_graph: Graph::new(),
            anysignal_to_signal: FnvHashMap::default(),
            next_sig_nr: 0,
            used_signals: HashSet::new(),
            signal_names,
        }
    }

    fn assign(graph: &Graph<LooseSig>) -> Graph<DetSig> {
        let mut ass = AnysignalAssigner::new(graph);

        // Keep track of what signals are already used by the blueprint
        // TODO: also add used signals in nodes or maybe it does not matter?
        for (_, _, conn) in ass.input_graph.iter_conns() {
            if let Connection::Operation(op) = conn {
                if let LooseSig::Signal(s) = op.get_output_iotype() {
                    ass.used_signals.insert(s);
                }
            }
        }

        // Convert any signals in nodes
        for (nid, node) in ass.input_graph.iter_nodes() {
            let new_node = match node {
                Node::Inner => Node::Inner,
                Node::InputVariable { kind, name, nr } => {
                    let kind = ass.assign_sig(kind);
                    Node::InputVariable {
                        kind,
                        name: name.clone(),
                        nr: *nr,
                    }
                }
                Node::Variable { kind, name } => {
                    let kind = ass.assign_sig(kind);
                    Node::Variable {
                        kind,
                        name: name.clone(),
                    }
                }
                Node::Output { kind, name, nr } => {
                    let kind = ass.assign_sig(kind);
                    Node::Output {
                        kind,
                        name: name.clone(),
                        nr: *nr,
                    }
                }
                Node::Constant(t) => {
                    let t = ass.assign_sig(t);
                    Node::Constant(t)
                }
            };
            debug_assert!(ass.output_graph.vertices.insert(nid, new_node).is_none());
        }

        // Assign connection signals
        for (from, to, conn) in ass.input_graph.iter_conns() {
            let new_conn = match conn {
                Connection::Operation(op) => {
                    let new_op = match op {
                        Operation::Arithmetic(ac) => {
                            let left = ass.assign_sig(&ac.left);
                            let right = ass.assign_sig(&ac.right);
                            let output = ass.assign_sig(&ac.output);
                            let operation = ac.operation.clone();
                            Operation::Arithmetic(ArithmeticOp::new(left, right, operation, output))
                        }
                        Operation::Decider(dc) => {
                            let left = ass.assign_sig(&dc.left);
                            let right = ass.assign_sig(&dc.right);
                            let output = ass.assign_sig(&dc.output);
                            let operation = dc.operation.clone();
                            Operation::Decider(DeciderOp::new(left, right, operation, output))
                        }
                        Operation::Gate(gc) => {
                            let left = ass.assign_sig(&gc.left);
                            let right = ass.assign_sig(&gc.right);
                            let gate_type = ass.assign_sig(&gc.gate_type);
                            let operation = gc.operation.clone();
                            Operation::Gate(GateOp {
                                left,
                                right,
                                operation,
                                gate_type,
                            })
                        }
                        Operation::Pick(pc) => {
                            let pick = ass.assign_sig(&pc.pick);
                            Operation::Pick(PickOp::new(pick))
                        }
                        Operation::Convert(cc) => {
                            let input = ass.assign_sig(&cc.input);
                            let output = ass.assign_sig(&cc.output);
                            Operation::Convert(ConvertOp::new(input, output))
                        }
                        Operation::Delay(dc) => {
                            let output = ass.assign_sig(&dc.output);
                            Operation::Delay(DelayOp::new(output))
                        }
                        Operation::Sum(sc) => {
                            let output = ass.assign_sig(&sc.output);
                            Operation::Sum(SumOp::new(output))
                        }
                    };
                    Connection::Operation(new_op)
                }
                // Copy wires directly
                Connection::Wire(wk) => Connection::Wire(wk),
            };
            ass.output_graph
                .adjacency
                .entry(from)
                .and_modify(|to_vec| to_vec.push((to, new_conn.clone())))
                .or_insert(vec![(to, new_conn)]);
        }

        ass.output_graph
    }

    fn assign_sig(&mut self, sig: &LooseSig) -> DetSig {
        match sig {
            LooseSig::AnySignal(n) => {
                let s = self.translate_anysignal(*n);
                DetSig::Signal(s)
            }
            LooseSig::ConstantAny((n, c)) => {
                let s = self.translate_anysignal(*n);
                DetSig::ConstantSignal((s, *c))
            }
            LooseSig::Signal(s) => DetSig::Signal(s.clone()),
            LooseSig::Constant(c) => DetSig::Constant(*c),
            LooseSig::ConstantSignal((s, c)) => DetSig::ConstantSignal((s.clone(), *c)),
            LooseSig::Many => DetSig::Many,
        }
    }

    fn get_signal(&self, signal_nr: u64) -> &str {
        self.signal_names[signal_nr as usize]
    }

    fn get_next_signal(&mut self) -> String {
        while self
            .used_signals
            .contains(self.get_signal(self.next_sig_nr))
        {
            self.next_sig_nr += 1
        }
        let sig = self.get_signal(self.next_sig_nr).to_string();
        self.next_sig_nr += 1;
        sig
    }

    fn translate_anysignal(&mut self, anysignal: u64) -> String {
        match self.anysignal_to_signal.get(&anysignal) {
            Some(s) => s.clone(),
            None => {
                let s = self.get_next_signal();
                self.anysignal_to_signal.insert(anysignal, s.clone());
                s
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_graph_size() {
        let expected_nodes = 6;
        let expected_wires = 5 * 2;
        let expected_conns = 11;

        let mut g: Graph<LooseSig> = Graph::new();
        let c1 = g.push_inner_node();
        let c2 = g.push_inner_node();
        let c3 = g.push_inner_node();
        let c4 = g.push_inner_node();
        let (c5, c6) = g.push_operation(Operation::Pick(PickOp::new(LooseSig::AnySignal(0))));
        g.push_wire(c1, c2);
        g.push_wire(c2, c3);
        g.push_wire(c3, c4);
        g.push_wire(c4, c5);
        g.push_wire(c6, c1);

        assert_eq!(g.vertices.len(), expected_nodes);
        assert_eq!(g.iter_wires().count(), expected_wires);
        assert_eq!(g.iter_conns().count(), expected_conns);

        let g = assign_signals(&g);
        assert_eq!(g.vertices.len(), expected_nodes);
        assert_eq!(g.iter_wires().count(), expected_wires);
        assert_eq!(g.iter_conns().count(), expected_conns);
    }

    #[test]
    fn assign_io_nodes() {
        let mut g: Graph<LooseSig> = Graph::new();
        g.push_input_node(
            "input".to_string(),
            LooseSig::Signal("input_signal".to_string()),
        );
        g.push_output_node(
            "output".to_string(),
            LooseSig::Signal("output_signal".to_string()),
        );

        assert_eq!(g.get_input_nodes().len(), 1);
        assert_eq!(g.get_output_nodes().len(), 1);

        let g = assign_signals(&g);

        assert_eq!(g.get_input_nodes().len(), 1);
        assert_eq!(g.get_output_nodes().len(), 1);
    }
}
