use crate::types::Type;
use super::{
    ASTNode, Span, Node, Result, Rule, Grammar,
    Identifier, Visibility, TypeSpecifier, NodeType
};

#[derive(Debug)]
pub enum DeclarationType {
    Immutable,
    Mutable,
    // Constant     // Evaluated at compile time
}

#[derive(Debug)]
pub struct Declaration {
    kind: DeclarationType,
    visibility: Visibility,
    is_static: bool,
    name: Identifier,
    typespec: TypeSpecifier,
    typ: Option<Type>,
    span: Span
}

impl Declaration {
    pub fn new(kind: DeclarationType, visibility: Visibility, is_static: bool, name: Identifier, typespec: TypeSpecifier, span: Span) -> Self {
        Self{ kind, visibility, is_static, name, typespec, typ: None, span }
    }
}

impl ASTNode for Declaration {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct Initialisation {
    kind: DeclarationType,
    visibility: Visibility,
    is_static: bool,
    name: Identifier,
    typespec: Option<TypeSpecifier>,
    value: Box<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Initialisation {
    pub fn new(kind: DeclarationType, visibility: Visibility, is_static: bool, name: Identifier, typespec: Option<TypeSpecifier>, value: NodeType, span: Span) -> Self {
        Self{ kind, visibility, is_static, name, typespec, value: Box::new(value), typ: None, span }
    }
}

impl ASTNode for Initialisation {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

pub fn build_declaration(input: Node) -> Result<NodeType> {
    let span: Span = Span::from_span(input.as_span());
    let mut kind: DeclarationType = DeclarationType::Immutable;
    let mut visibility: Visibility = Visibility::Private;
    let mut is_static: bool = false;
    let mut ident: Option<Identifier> = None;
    let mut typespec: Option<TypeSpecifier> = None;
    let mut expression: Option<Box<NodeType>> = None;
    let mut children = input.into_children()
        .peekable();
    while let Some(node) = children.next() {
        match node.as_rule() {
            Rule::mutable => { kind = DeclarationType::Immutable; },
            Rule::visibility => { visibility = Grammar::visibility(node)?; },
            Rule::status => { is_static = true; },
            Rule::ident => { ident = Some(Grammar::ident(node)?); },
            Rule::typespec => { typespec = Some(Grammar::typespec(node)?); },
            Rule::expression => { expression = Some(Box::new(Grammar::expression(node)?)); },
            _ => {}
        }
    }
    if expression.is_none() {
        return Ok(NodeType::Declaration(Declaration::new(kind, visibility, is_static, ident.unwrap(), typespec.unwrap(), span)));
    }
    else {
        return Ok(NodeType::Initialisation(Initialisation::new(kind, visibility, is_static, ident.unwrap(), typespec, *expression.unwrap(), span)));
    }
}