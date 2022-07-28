use crate::types::Type;
use super::Span;

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