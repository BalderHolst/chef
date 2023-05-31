use crate::compiler::graph::*;
use crate::ast::AST;
use crate::ast::{Statement, StatementKind, VariableType};

pub mod graph;

pub struct Compiler {
    ast: AST,
    cursor: usize,
    next_anysignal: u64,
}

impl Compiler {
    pub fn new(ast: AST) -> Self {
        Self { ast, cursor:0, next_anysignal: 0 }
    }

    pub fn compile_next(&mut self) -> Option<Graph> {
        let statement = self.ast.statements.get(self.cursor)?.clone();
        Some(self.compile_statement(statement))
    }

    fn get_new_anysignal(&mut self) -> IOType {
        let signal = IOType::AnySignal(self.next_anysignal);
        self.next_anysignal += 1;
        signal
    }

    fn compile_statement(&mut self, statement: Statement) -> Graph {
        let mut graph = Graph::new();
        match &statement.kind {
            StatementKind::Block(block) => {
                // let all = graph.push_vertex();
                // let input = graph.push_vertex();

                // for input_var in &block.inputs {
                //     let io_type = match &input_var.variable_type {
                //         VariableType::Int(t) => IOType::Signal(t.clone()),
                //         VariableType::Any => self.get_new_anysignal(),
                //         VariableType::All => {
                //             todo!("All not implemented as input")
                //         }
                //     };

                //     graph.push_edge(all, input, Edge::new(
                //             Connection::Arithmetic(
                //                 ArithmeticConnection::new_pick(io_type)
                //                 )
                //             ))
                // }

                for statement in &block.statements {
                    match &statement.kind {
                        StatementKind::Expression(expr) => {
                             
                        },
                        StatementKind::Block(_) => todo!("Blocks within block are not implemented.")
                    }
                }

            }
            _ => todo!("Only block statements inplemented"),
        }

        graph.print();

        todo!()
    }

}
