use crate::types::Type;
use super::{
    ASTNode, Span,
    Identifier, GenericDefArguments, Visibility, TypeSpecifier
};

#[derive(Debug)]
pub struct Struct {
    visibility: Visibility,
    name: Identifier,
    generic_args: Option<GenericDefArguments>,
    declarations: Vec<StructDeclaration>,
    typ: Option<Type>,
    span: Span
}

impl Struct {
    pub fn new(visibility: Visibility, name: Identifier, generic_args: Option<GenericDefArguments>, declarations: Vec<StructDeclaration>, span: Span) -> Self {
        Self{ visibility, name, generic_args, declarations, typ: None, span }
    }
}

impl ASTNode for Struct {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug)]
pub struct StructDeclaration {
    visibility: Visibility,
    name: Identifier,
    typespec: TypeSpecifier,
    typ: Option<Type>,
    span: Span
}

impl StructDeclaration {
    pub fn new(visibility: Visibility, name: Identifier, typespec: TypeSpecifier, span: Span) -> Self {
        Self{ visibility, name, typespec, typ: None, span }
    }
}

impl ASTNode for StructDeclaration {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}