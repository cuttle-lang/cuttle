use crate::types::Type;
use super::{ASTNode, Node, Rule, Span, NodeType };

#[derive(Debug)]
pub enum BinaryOperator {
    Exponent,
    Multiply,
    Divide,
    Modulus,
    Plus,
    Minus,
    ShiftLeft,
    ShiftRight,
    LessThanOrEqual,
    LessThan,
    GreaterThanOrEqual,
    GreaterThan,
    NotEqual,
    Equal,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXOR,
    Assign,
    Cast
}

impl BinaryOperator {
    pub fn from_node(op: Node) -> Self {
        match op.as_rule() {
            Rule::exponent => Self::Exponent,
            Rule::multiply => Self::Multiply,
            Rule::divide => Self::Divide,
            Rule::modulus => Self::Modulus,
            Rule::plus => Self::Plus,
            Rule::minus => Self::Minus,
            Rule::shift_left => Self::ShiftLeft,
            Rule::shift_right => Self::ShiftRight,
            Rule::less_than_or_equal => Self::LessThanOrEqual,
            Rule::less_than => Self::LessThan,
            Rule::greater_than_or_equal => Self::GreaterThanOrEqual,
            Rule::greater_than => Self::GreaterThan,
            Rule::not_equal => Self::NotEqual,
            Rule::equal => Self::Equal,
            Rule::logical_and => Self::LogicalAnd,
            Rule::logical_or => Self::LogicalOr,
            Rule::bitwise_and => Self::BitwiseAnd,
            Rule::bitwise_or => Self::BitwiseOr,
            Rule::bitwise_xor => Self::BitwiseXOR,
            Rule::assign => Self::Assign,
            Rule::cast => Self::Cast,
            rule => { panic!("Rule {:?} isn't an operator", rule); }
        }
    }
}

impl ASTNode for BinaryOperation {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct BinaryOperation {
    left: Box<NodeType>,
    operator: BinaryOperator,
    right: Box<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl BinaryOperation {
    pub fn new(left: NodeType, operator: BinaryOperator, right: NodeType, span: Span ) -> Self {
        Self{ left: Box::new(left), operator, right: Box::new(right), typ: None, span }
    }
}