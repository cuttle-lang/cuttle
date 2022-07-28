use std::collections::HashMap;

pub type Members = HashMap<String, (NodeType, Span)>;

use crate::types::Type;
use super::{Span, NodeType};

#[derive(Debug)]
pub struct StructConstructor {
    struct_scope: Vec<(String, Span)>,
    members: Members,
    typ: Option<Type>,
    span: Span
}

impl StructConstructor {
    pub fn new(struct_scope: Vec<(String, Span)>, members: Members, span: Span) -> Self {
        Self{ struct_scope, members, typ: None, span }
    }
}