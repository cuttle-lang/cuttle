use crate::types::Type;
use super::{ASTNode, Span, Identifier};

pub type GenericCallArguments = Vec<(Box<TypeSpecifier>, Span)>;
pub type GenericDefArguments = (Vec<GenericDefArgument>, Span);

#[derive(Debug)]
pub struct TypeSpecifier {
    scope: Option<Vec<(String, Span)>>,
    tuple: Option<Vec<TypeSpecifier>>,
    generic_args: Option<GenericCallArguments>,
    pointer_level: u8,
    array_dimensions: Vec<(u32, Span)>,
    typ: Option<Type>,
    span: Span
}

impl TypeSpecifier {
    pub fn new(scope: Option<Vec<(String, Span)>>, tuple: Option<Vec<TypeSpecifier>>, generic_args: Option<GenericCallArguments>, pointer_level: u8, array_dimensions: Vec<(u32, Span)>, span: Span) -> Self {
        Self{ scope, tuple, generic_args, pointer_level, array_dimensions, typ: None, span }
    }
}

impl ASTNode for TypeSpecifier {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct TypeDefinition {
    name: Identifier,
    generic_args: Option<GenericDefArguments>,
    typespec: TypeSpecifier,
    typ: Option<Type>,
    span: Span
}

impl TypeDefinition {
    pub fn new(name: Identifier, generic_args: Option<GenericDefArguments>, typespec: TypeSpecifier, span: Span) -> Self {
        Self{ name, generic_args, typespec, typ: None, span }
    }
}

impl ASTNode for TypeDefinition {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct GenericDefArgument {
    name: Identifier,
    implements: bool,
    extends: bool,
    constraint_name: Option<Identifier>,
    typ: Option<Type>,
    span: Span
}

impl GenericDefArgument {
    pub fn new(name: Identifier, implements: bool, extends: bool, constraint_name: Option<Identifier>, span: Span) -> Self {
        Self{ name, implements, extends, constraint_name, typ: None, span }
    }
}

impl ASTNode for GenericDefArgument {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}