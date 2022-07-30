use std::collections::HashMap;

pub type Members = HashMap<String, (NodeType, Span)>;

use crate::types::Type;
use super::{ASTNode, Span, NodeType, GenericCallArguments, CallArguments};

#[derive(Debug)]
pub struct StructConstructor {
    struct_scope: Vec<(String, Span)>,
    generic_args: Option<GenericCallArguments>,
    members: Members,
    typ: Option<Type>,
    span: Span
}

impl StructConstructor {
    pub fn new(struct_scope: Vec<(String, Span)>, generic_args: Option<GenericCallArguments>, members: Members, span: Span) -> Self {
        Self{ struct_scope, generic_args, members, typ: None, span }
    }
}

impl ASTNode for StructConstructor {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct ClassConstructor {
    class_scope: Vec<(String, Span)>,
    generic_args: Option<GenericCallArguments>,
    call_args: CallArguments,
    typ: Option<Type>,
    span: Span
}

impl ClassConstructor {
    pub fn new(class_scope: Vec<(String, Span)>, generic_args: Option<GenericCallArguments>, call_args: CallArguments, span: Span) -> Self {
        Self{ class_scope, generic_args, call_args, typ: None, span }
    }
}

impl ASTNode for ClassConstructor {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

