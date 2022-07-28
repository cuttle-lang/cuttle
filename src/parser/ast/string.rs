use crate::types::Type;
use super::Span;

#[derive(Debug)]
pub struct Str {
    string: String,
    typ: Option<Type>,
    span: Span
}

impl Str {
    pub fn new(string: String, span: Span) -> Self {
        Self{ string, typ: None, span }
    }
}