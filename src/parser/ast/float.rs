use crate::types::Type;
use super::{ASTNode, Span, Node, Result};

#[derive(Debug)]
pub struct Float {
    value: f64,
    typ: Option<Type>,
    span: Span
}

impl Float {
    pub fn new(value: f64, span: Span) -> Self {
        Self{ value, typ: None, span }
    }
}

impl ASTNode for Float {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

pub fn float(input: Node) -> Result<Float> {
    let span: Span = Span::from_span(input.as_span());
    Ok( match input.as_str()
            .to_owned()
            .parse::<f64>() {
        Ok(float) => Float::new(float, span),
        Err(error) => panic!{"Invalid Float: {:?}", error}
    })
}