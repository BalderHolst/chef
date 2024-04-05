use std::collections::HashSet;

use fnv::FnvHashMap;

use crate::{
    compiler::graph::{ConvertOp, DeciderOp, DelayOp, Node, PickOp, SumOp},
    utils::BASE_SIGNALS,
};

use super::graph::{ArithmeticOp, Connection, DetSig, Graph, LooseSig, Operation, Signal};

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
        for (nid, node) in ass.input_graph.nodes() {
            let new_node = match node {
                Node::Inner => Node::Inner,
                Node::InputVariable { kind, name } => {
                    let kind = ass.assign_sig(kind);
                    Node::InputVariable {
                        kind,
                        name: name.clone(),
                    }
                }
                Node::Variable { kind, name } => {
                    let kind = ass.assign_sig(kind);
                    Node::InputVariable {
                        kind,
                        name: name.clone(),
                    }
                }
                Node::Output { kind, name } => {
                    let kind = ass.assign_sig(kind);
                    Node::InputVariable {
                        kind,
                        name: name.clone(),
                    }
                }
                Node::Constant(t) => {
                    let t = ass.assign_sig(t);
                    Node::Constant(t)
                }
            };
            ass.output_graph.vertices.insert(nid, new_node);
        }

        // Assign connection signals
        for (from, to, conn) in ass.input_graph.iter_conns() {
            if let Connection::Operation(op) = conn {
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
                        Operation::Decider(DeciderOp::new(left, right, operation, gate_type))
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
                let new_conn = Connection::Operation(new_op);
                ass.output_graph
                    .adjacency
                    .entry(from)
                    .and_modify(|to_vec| to_vec.push((to, new_conn.clone())))
                    .or_insert(vec![(to, new_conn)]);
            }
        }

        todo!()
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
            .get(self.get_signal(self.next_sig_nr))
            .is_some()
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
