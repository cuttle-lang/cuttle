
use crate::types::Type;
use super::{Span, NodeType};

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