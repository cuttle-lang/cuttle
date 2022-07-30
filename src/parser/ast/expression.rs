use super::{
    Rule, Result, Node, NodeType, Span,
    BinaryOperation, BinaryOperator, ASTNode
};
use crate::types::Type;

#[derive(Debug)]
pub struct Expression {
    expression: Box<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Expression {
    pub fn new(expression: NodeType, span: Span) -> Self {
        Self{ expression: Box::new(expression), typ: None, span }
    }
}

impl ASTNode for Expression {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct If {
    expression: Box<NodeType>,
    block: Vec<NodeType>,
    else_if: Vec<Box<If>>,
    else_block: Vec<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl If {
    pub fn new(expression: NodeType, block: Vec<NodeType>, else_if: Vec<Box<If>>, else_block: Vec<NodeType>, span: Span) -> Self {
        Self{ expression: Box::new(expression), block, else_if, else_block, typ: None, span }
    }
}

impl ASTNode for If {
    fn get_span(&self) -> Span {
        self.span.clone()
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
            | Rule::cast
        => {
            let span: Span = Span::from_span(op.as_span());
            let op = BinaryOperator::from_node(op);
            
            Ok(NodeType::BinaryOperation(BinaryOperation::new(
                left,
                op,
                right,
                span
            )))
        }
        rule => Err(op.error(format!("Rule {:?} isn't an operator", rule)))?
    }
}