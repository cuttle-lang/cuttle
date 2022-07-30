use crate::types::Type;
use super::{
    ASTNode, Span, TypeSpecifier, NodeType, Local, GenericCallArguments, Str,
    Rule, Grammar, Node, Result
};

pub type DefineArguments = (Vec<DefineArgument>, Span);
pub type DefineArgument = ((String, Span), (TypeSpecifier, Span));

pub type CallArguments = (Vec<CallArgument>, Span);
pub type CallArgument = (Box<NodeType>, Span);

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

impl ASTNode for Identifier {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct Dotted {
    atom: Box<NodeType>,
    member: Identifier,
    typ: Option<Type>,
    span: Span
}

impl Dotted {
    pub fn new(atom: NodeType, member: Identifier, span: Span) -> Self {
        Self{ atom: Box::new(atom), member, typ: None, span }
    }
}

impl ASTNode for Dotted {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct ScopeResolution {
    atom: Box<NodeType>,
    member: Identifier,
    typ: Option<Type>,
    span: Span
}

impl ScopeResolution {
    pub fn new(atom: NodeType, member: Identifier, span: Span) -> Self {
        Self{ atom: Box::new(atom), member, typ: None, span }
    }
}

impl ASTNode for ScopeResolution {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct Index {
    atom: Box<NodeType>,
    index: Box<NodeType>,
    typ: Option<Type>,
    span: Span
}

impl Index {
    pub fn new(atom: NodeType, index: NodeType, span: Span) -> Self {
        Self{ atom: Box::new(atom), index: Box::new(index), typ: None, span }
    }
}

impl ASTNode for Index {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct Call {
    atom: Box<NodeType>,
    generic_args: Option<GenericCallArguments>,
    call_args: CallArguments,
    typ: Option<Type>,
    span: Span
}

impl Call {
    pub fn new(atom: NodeType, generic_args: Option<GenericCallArguments>, call_args: CallArguments, span: Span) -> Self {
        Self{ atom: Box::new(atom), generic_args, call_args, typ: None, span }
    }
}

impl ASTNode for Call {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

pub fn build_access(input: Node) -> Result<NodeType> {
    let mut children = input.into_children().peekable();
    let local = Grammar::ident(children.next().unwrap())?;
    let local_node = NodeType::Local(Local::new(local.get_name(), local.get_span()));
    if children.peek().is_none() {
        return Ok(local_node);
    }
    let mut atom: NodeType = NodeType::Identifier(local);
    while let Some(node) = children.next() {
        let span: Span = Span::from_span(node.as_span());
        match node.as_rule() {
            Rule::dot => {
                let member = Grammar::ident(children.next().unwrap()).unwrap();
                atom = NodeType::Dotted(Dotted::new(atom, member, span))
            },
            Rule::scope => {
                let member = Grammar::ident(children.next()
                    .unwrap())
                    .unwrap();
                atom = NodeType::ScopeResolution(ScopeResolution::new(atom, member, span))
            },
            Rule::index => {
                let index = Grammar::index(node)?;
                atom = NodeType::Index(Index::new(atom, index, span))
            },
            Rule::generic_call_args => {
                let generic_args: GenericCallArguments = Grammar::generic_call_args(node).unwrap();
                let call_args = Grammar::call_arguments(children.next()
                    .unwrap())
                    .unwrap();
                atom = NodeType::Call(Call::new(atom, Some(generic_args), call_args, span))
            }
            Rule::call_arguments => {
                let call_args = Grammar::call_arguments(node)
                    .unwrap();
                atom = NodeType::Call(Call::new(atom, None, call_args, span))
            },
            _ => { panic!("Invalid access node"); }
        }
        if children.peek().is_none() {
            return Ok(atom);
        }
    }
    panic!("Access not implemented");
}

#[derive(Debug)]
pub struct Import {
    path: Str,
    name: Option<Identifier>,
    typ: Option<Type>,
    span: Span
}

impl Import {
    pub fn new(path: Str, name: Option<Identifier>, span: Span) -> Self {
        Self{ path, name, typ: None, span }
    }
}

impl ASTNode for Import {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct Use {
    path: Vec<(String, Span)>,
    use_all: bool,
    typ: Option<Type>,
    span: Span
}

impl Use {
    pub fn new(path: Vec<(String, Span)>, use_all: bool, span: Span) -> Self {
        Self{ path, use_all, typ: None, span }
    }
}

impl ASTNode for Use {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}