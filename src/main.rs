use crate::ast::lexer::Lexer;
use crate::ast::lexer::Token;

mod ast;

fn main() {
    let lexer = Lexer::new("151+2            / 3");
    let tokens: Vec<Token> = lexer.collect();
    println!("{:?}", tokens);
}
