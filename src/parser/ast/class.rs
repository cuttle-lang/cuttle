use crate::types::Type;
use super::{
    ASTNode, Span, Node, Result, Rule, Grammar, NodeType,
    Visibility, GenericDefArguments, ScopeResolution, Identifier,
    DefineArguments
};

#[derive(Debug)]
pub struct Class {
    visibility: Visibility,
    ident: Identifier,
    generic_args: Option<GenericDefArguments>,
    implements: bool,
    extends: bool,
    constraint: Option<Vec<(String, Span)>>,
    body: Vec<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Class {
    pub fn new(visibility: Visibility, ident: Identifier, generic_args: Option<GenericDefArguments>, implements: bool, extends: bool, constraint: Option<Vec<(String, Span)>>, body: Vec<NodeType>, span: Span) -> Self {
        Self{ visibility, ident, generic_args, implements, extends, constraint, body, typ: None, span }
    }
}

impl ASTNode for Class {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct ClassConstructMethod {
    args: DefineArguments,
    body: Vec<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl ClassConstructMethod {
    pub fn new(args: DefineArguments, body: Vec<NodeType>, span: Span) -> Self {
        Self{ args, body, typ: None, span }
    }
}

impl ASTNode for ClassConstructMethod {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}


pub fn build_class(input: Node) -> Result<Class> {
    let span: Span = Span::from_span(input.as_span());

    let mut visibility: Visibility = Visibility::Private;
    let mut ident: Option<Identifier> = None;
    let mut generic_args: Option<GenericDefArguments> = None;
    let mut implements: bool = false;
    let mut extends: bool = false;
    let mut constraint: Option<Vec<(String, Span)>> = None;
    let mut body: Vec<NodeType> = vec![];

    let mut children = input.into_children().peekable();
    while let Some(node) = children.next() {
        match node.as_rule() {
            Rule::visibility => { visibility = Visibility::from_node(node); },
            Rule::ident => { ident = Some(Grammar::ident(node)?); },
            Rule::generic_def_args => { generic_args = Some(Grammar::generic_def_args(node)?); },
            Rule::class_implements => { implements = true },
            Rule::class_extends => { extends = true },
            Rule::scope_resolution => { constraint = Some(Grammar::scope_resolution(node)?); },
            Rule::class_block => { body = Grammar::class_block(node)?; },
            _ => {}
        }
    }

    Ok(Class::new(visibility, ident.unwrap(), generic_args, implements, extends, constraint, body, span))
}