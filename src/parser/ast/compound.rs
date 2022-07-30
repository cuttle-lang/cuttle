
use crate::types::Type;
use super::{ASTNode, Span, NodeType};

#[derive(Debug)]
pub struct Array {
    elements: Vec<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Array {
    pub fn new(elements: Vec<NodeType>, span: Span) -> Self {
        Self{ elements, typ: None, span }
    }
}

impl ASTNode for Array {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct Tuple {
    elements: Vec<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Tuple {
    pub fn new(elements: Vec<NodeType>, span: Span) -> Self {
        Self{ elements, typ: None, span }
    }
}

impl ASTNode for Tuple {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}