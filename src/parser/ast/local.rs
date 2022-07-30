use crate::types::Type;
use super::{ASTNode, Span};

#[derive(Debug)]
pub struct Local {
    name: String,
    typ: Option<Type>,
    span: Span
}

impl Local {
    pub fn new(name: String, span: Span) -> Self {
        Self{ name, typ: None, span }
    }
}

impl ASTNode for Local {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}


#[derive(Debug)]
pub struct Reference {
    scope: Vec<(String, Span)>,
    mutable: bool,
    typ: Option<Type>,
    span: Span
}

impl Reference {
    pub fn new(scope: Vec<(String, Span)>, mutable: bool, span: Span) -> Self {
        Self{ scope, mutable, typ: None, span }
    }
}

impl ASTNode for Reference {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}
