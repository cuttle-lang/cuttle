use crate::types::Type;
use super::{ASTNode, Rule, NodeType, Span, Node};

#[derive(Debug)]
pub enum UnaryOperator {
    Positive,
    Negative,
    LogicalNot
}

impl UnaryOperator {
    pub fn from_node(op: Node) -> Self {
        match op.as_rule() {
            Rule::plus => Self::Positive,
            Rule::minus => Self::Negative,
            Rule::logical_not => Self::LogicalNot,
            rule => { panic!("Rule {:?} isn't an operator", rule); }
        }
    }
}

#[derive(Debug)]
pub struct UnaryOperation {
    operator: UnaryOperator,
    operand: Box<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl UnaryOperation {
    pub fn new(operator: UnaryOperator, operand: NodeType, span: Span) -> Self {
        Self{ operator, operand: Box::new(operand), typ: None, span }
    }
}

impl ASTNode for UnaryOperation {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}