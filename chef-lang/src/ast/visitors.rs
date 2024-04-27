//!  Traits for traversing the abstract syntax tree.

// This file constains the traits Visitor and MutVisitor, both of these traits allow a struct to
// traverse the AST. Both clases have almost identical code, the only difference being the `mut`
// keyword. This is less than optimal, but i have no ideas how to write macros, or if they could
// even solve this. Suggestions very welcome!

use super::{
    parser::StatementList, BinaryExpression, Block, BlockLinkExpression, Declaration,
    DeclarationDefinition, Definition, DelayExpression, Expression, ExpressionKind,
    IndexExpression, ParenthesizedExpression, PickExpression, SizeOfExpression, Statement,
    StatementKind, TupleDeclarationDefinition, VariableRef, WhenStatement,
};

use make_visitors::make_visitors;

// For documentation references
#[allow(unused_imports)]
use super::AST;

make_visitors!();
