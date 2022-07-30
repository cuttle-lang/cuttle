use crate::types::Type;
use super::{ASTNode, Span, Node, Result};

#[derive(Debug)]
pub struct Integer {
    value: i128,
    typ: Option<Type>,
    span: Span
}

impl Integer {
    pub fn new(value: i128, span: Span) -> Self {
        Self{ value, typ: None, span }
    }
}

impl ASTNode for Integer {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

pub fn integer_decimal(input: Node) -> Result<i128> {
    Ok( match input.as_str()
        .to_owned()
        .parse::<i128>() {
        Ok(int) => int,
        Err(error) => panic!("{:?}", error)
    })
}