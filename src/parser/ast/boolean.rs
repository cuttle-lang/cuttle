use crate::types::Type;
use super::{ASTNode, Span};

#[derive(Debug)]
pub struct Boolean {
    value: bool,
    typ: Option<Type>,
    span: Span
}

impl Boolean {
    pub fn new(value: bool, span: Span) -> Self {
        Self{ value, typ: None, span }
    }
}

impl ASTNode for Boolean {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}