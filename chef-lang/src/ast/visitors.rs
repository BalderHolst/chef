//!  Traits for traversing the abstract syntax tree.
//!
//! This module contains the traits [Visitor] and [MutVisitor]. Both of these traits allow a struct to
//! traverse the [AST]. The [Visitor] trait is for read-only traversals, while the [MutVisitor] trait
//! is for traversals that may modify the [AST].
//!
//! These traits are generated by the [make_visitors!] macro as they share implementation apart from
//! the type of to the [AST] nodes.

use super::{
    BinaryExpression, Block, BlockLinkExpression, Declaration, DeclarationDefinition, Definition,
    DelayExpression, Expression, ExpressionKind, IndexExpression, NegativeExpression,
    ParenthesizedExpression, PickExpression, SizeOfExpression, Statement, StatementKind,
    TupleDeclarationDefinition, Variable, VariableRef, WhenStatement,
};

use make_visitors::make_visitors;

// For documentation references
#[allow(unused_imports)]
use super::AST;

pub type Number = i32;

// Generate trait implementations for the `Visitor` and `MutVisitor` traits
make_visitors!();
