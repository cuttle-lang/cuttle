use crate::types::Type;
use super::Span;

#[derive(Debug)]
pub struct Identifier {
    name: String,
    typ: Option<Type>,
    span: Span
}

impl Identifier {
    pub fn new(name: String, span: Span) -> Self {
        Self{ name, typ: None, span }
    }
    pub fn get_name(&self) -> String {
        self.name.clone()
    }
}