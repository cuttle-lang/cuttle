pub mod expression;
pub mod binop;
pub mod access;
pub mod boolean;
pub mod integer;
pub mod float;
pub mod string;
pub mod compound;
pub mod instance;

use std::collections::HashMap;

use crate::parser::{
    grammar::{Grammar, Rule},
    precedence::*
};
use pest_consume::{Error, match_nodes, Parser};
use expression::Expression;
use binop::{BinaryOperation, BinaryOperator};
use boolean::Boolean;
use integer::Integer;
use float::Float;
use string::Str;
use compound::Array;
use instance::{Members, StructConstructor};
use access::{Identifier};

pub type Result<T> = std::result::Result<T, Error<Rule>>;
pub type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LineColumn {
    line: usize,
    column: usize
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    start: LineColumn,
    end: LineColumn
}

impl Span {
    pub fn from_span(span: pest::Span) -> Self {
        let start = span.start_pos().line_col();
        let end = span.end_pos().line_col();

        Self{
            start: LineColumn{ line: start.0, column: start.1 },
            end: LineColumn{ line: end.0, column: end.1 }
        }
    }
}

#[enum_dispatch]
#[derive(Debug)]
pub enum NodeType {
    Expression(Box<Expression>),
    BinaryOperation(Box<BinaryOperation>),

    Boolean(Boolean),
    Integer(Integer),
    Float(Float),
    String(Str),

    Array(Box<Array>),
    StructConstructor(StructConstructor)
}

#[pest_consume::parser]
impl Grammar {
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }

    // TODO: Attributes
    // fn scope_attributes(input: Node) -> Result<Vec<NodeType::Attribute>> {
    //     panic!("scope_attributes Not Implemented");
    // }

    fn ident(input: Node) -> Result<Identifier> {
        let span: Span = Span::from_span(input.as_span());
        Ok(Identifier::new(input.as_str().to_owned(), span))
    }

    fn boolean_true(input: Node) -> Result<bool> {
        Ok(true)
    }
    fn boolean_false(input: Node) -> Result<bool> {
        Ok(false)
    }
    fn boolean(input: Node) -> Result<Boolean> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [boolean_true(bool_true)] => Boolean::new(bool_true, span),
            [boolean_false(bool_false)] => Boolean::new(bool_false, span)
        ))
    }

    fn integer_decimal(input: Node) -> Result<i128> {
        integer::integer_decimal(input)
    }
    fn integer_zero(input: Node) -> Result<i128> {
        Ok(0)
    }
    fn integer(input: Node) -> Result<Integer> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [integer_decimal(decimal)] => Integer::new(decimal, span),
            [integer_zero(zero)] => Integer::new(zero, span)
        ))
    }

    fn float(input: Node) -> Result<Float> {
        float::float(input)
    }

    fn string_content(input: Node) -> Result<String> {
        Ok(input.as_str().to_owned())
    }
    fn string(input: Node) -> Result<Str> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [string_content(content)] => Str::new(content, span)
        ))
    }

    fn array(input: Node) -> Result<Array> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(exprs)..] => {
                let mut elements: Vec<NodeType> = vec![];
                for expr in exprs {
                    elements.push(expr);
                }
                Array::new(elements, span)
            }
        ))
    }

    fn struct_map_pair(input: Node) -> Result<(String, NodeType, Span)> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [ident(ident), expression(expr)] => (ident.get_name(), expr, span)
        ))
    }
    fn struct_map(input: Node) -> Result<Members> {
        Ok(match_nodes!(input.into_children();
            [struct_map_pair(pairs)..] => {
                let mut members: Members = HashMap::new();
                for pair in pairs {
                    members.insert(pair.0, (pair.1, pair.2));
                }
                members
            }
        ))
    }
    fn struct_constructor(input: Node) -> Result<StructConstructor> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [scope_resolution(scope), struct_map(members)] => 
                StructConstructor::new(scope, members, span)
        ))
    }

    fn scope_resolution(input: Node) -> Result<Vec<(String, Span)>> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [ident(idents)..] => {
                let mut scope: Vec<(String, Span)> = vec![];
                for ident in idents {
                    scope.push((ident.get_name(), span));
                }
                scope
            }
        ))
    }

    fn term(input: Node) -> Result<NodeType> {
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => expr,
            [boolean(boolean)] => NodeType::Boolean(boolean),
            [integer(int)] => NodeType::Integer(int),
            [float(float)] => NodeType::Float(float),
            [string(string)] => NodeType::String(string),
            [array(array)] => NodeType::Array(Box::new(array)),
            [struct_constructor(struc)] => NodeType::StructConstructor(struc)
        ))
    }
    
    #[prec_climb(term, PRECCLIMBER)]
    fn expression(left: NodeType, op: Node, right: NodeType) -> Result<NodeType> {
        expression::expression(left, op, right)
    }

    fn expr_statement(input: Node) -> Result<Expression> { 
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => Expression::new(expr, span)
        ))
    }

    fn file(input: Node) -> Result<AST> {
        let span: Span = Span::from_span(input.as_span());
        let mut statements: Vec<NodeType> = vec![];
        for node in input.into_children() {
            match node.as_rule() {
                Rule::EOI => {},
                Rule::expr_statement => statements.push(
                    NodeType::Expression(Box::new(Self::expr_statement(node)?))
                ),
                _ => {}
            }
        }

        Ok(AST{ /* attributes, */ statements, span })
    }
}

#[derive(Debug)]
pub struct AST {
    // TODO: Attributes
    // pub attributes: Vec<NodeType::Attribute>,
    pub statements: Vec<NodeType>,
    pub span: Span
}

pub fn parse_cuttle(input_str: &str) -> Result<AST> {
    let inputs = Grammar::parse_with_userdata(Rule::file, input_str, ())?;
    let input = inputs.single()?;

    Grammar::file(input)
}