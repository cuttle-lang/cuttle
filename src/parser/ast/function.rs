use crate::types::Type;
use super::{
    ASTNode, Span, Node, Result, Rule, Grammar,
    Visibility, Identifier, GenericDefArguments, DefineArguments, TypeSpecifier, NodeType
};

#[derive(Debug)]
pub struct Function {
    visibility: Visibility,
    is_static: bool,
    is_extern: bool,
    signature: FunctionSignature,
    body: Option<Vec<NodeType>>,
    typ: Option<Type>,
    span: Span
}

impl Function {
    pub fn new(visibility: Visibility, is_static: bool, is_extern: bool, signature: FunctionSignature, body: Option<Vec<NodeType>>, span: Span) -> Self {
        Self{ visibility, is_static, is_extern, signature, body, typ: None, span }
    }
    pub fn is_defined(&self) -> bool {
        !self.body.is_none()
    }
}

impl ASTNode for Function {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct FunctionSignature {
    name: Identifier,
    generic_args: Option<GenericDefArguments>,
    parameters: DefineArguments,
    output: TypeSpecifier,
    typ: Option<Type>,
    span: Span
}

impl FunctionSignature {
    pub fn new(name: Identifier, generic_args: Option<GenericDefArguments>, parameters: DefineArguments, output: TypeSpecifier, span: Span) -> Self {
        Self{ name, generic_args, parameters, output, typ: None, span }
    }
}

impl ASTNode for FunctionSignature {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

pub fn build_function(input: Node) -> Result<Function> {
    let span: Span = Span::from_span(input.as_span());
    let mut visibility: Visibility = Visibility::Private;
    let mut is_static: bool = false;
    let mut is_extern: bool = false;
    let mut children = input.into_children().peekable();
    while let Some(node) = children.next() {
        match node.as_rule() {
            Rule::visibility => { visibility = Visibility::from_node(node); },
            Rule::status => { is_static = true },
            Rule::external => { is_extern = true }
            Rule::function_signature => {
                let signature = Grammar::function_signature(node)?;
                if children.peek().is_none() {
                    return Ok(Function::new(visibility, is_static, is_extern, signature, None, span));
                }
                let body = Grammar::function_block(children.next().unwrap())?;
                return Ok(Function::new(visibility, is_static, is_extern, signature, Some(body), span));
            }
            _ => { panic!("Unexpected node in function"); }
        }
    }
    panic!("Unexpected node in function");
}