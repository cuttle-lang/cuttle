use crate::types::Type;
use super::{ASTNode, Span, Node, Result, NodeType};

#[derive(Debug)]
pub struct Namespace {
    scope: Vec<(String, Span)>,
    body: Vec<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Namespace {
    pub fn new(scope: Vec<(String, Span)>, body: Vec<NodeType>, span: Span) -> Self {
        Self{ scope, body, typ: None, span }
    }
}

impl ASTNode for Namespace {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}
