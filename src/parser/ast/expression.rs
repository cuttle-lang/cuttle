use crate::parser::ast::{
    Rule, Result, Node, NodeType, Span,
    BinaryOperation, BinaryOperator
};
use crate::types::Type;

#[derive(Debug)]
pub struct Expression {
    expression: NodeType,
    typ: Option<Type>,
    span: Span
}

impl Expression {
    pub fn new(expression: NodeType, span: Span) -> Self {
        Self{ expression, typ: None, span }
    }
}

pub fn expression(left: NodeType, op: Node, right: NodeType) -> Result<NodeType> {
    match op.as_rule() {
        Rule::assign
            | Rule::logical_or
            | Rule::logical_and
            | Rule::equal
            | Rule::not_equal
            | Rule::greater_than_or_equal
            | Rule::less_than_or_equal
            | Rule::greater_than
            | Rule::less_than
            | Rule::bitwise_xor
            | Rule::bitwise_or
            | Rule::bitwise_and
            | Rule::shift_right
            | Rule::shift_left
            | Rule::plus
            | Rule::minus
            | Rule::multiply
            | Rule::divide
            | Rule::exponent 
        => {
            let span: Span = Span::from_span(op.as_span());
            let op = BinaryOperator::from_node(op);
            
            Ok(NodeType::BinaryOperation(Box::new(BinaryOperation::new(
                left,
                op,
                right,
                span
            ))))
        }
        rule => Err(op.error(format!("Rule {:?} isn't an operator", rule)))?
    }
}