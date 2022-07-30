use crate::types::Type;
use super::{ASTNode, Span, NodeType};

#[derive(Debug)]
pub struct Break {
    typ: Option<Type>,
    span: Span
}

#[derive(Debug)]
pub struct Return {
    value: Box<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Return {
    pub fn new(value: NodeType, span: Span) -> Self {
        Self{ value: Box::new(value), typ: None, span }
    }
}

impl ASTNode for Return {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

impl Break {
    pub fn new(span: Span) -> Self {
        Self{ typ: None, span }
    }
}

impl ASTNode for Break {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct Continue {
    typ: Option<Type>,
    span: Span
}

impl Continue {
    pub fn new(span: Span) -> Self {
        Self{ typ: None, span }
    }
}

impl ASTNode for Continue {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}