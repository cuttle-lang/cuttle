pub mod expression;
pub mod unaryop;
pub mod binop;
pub mod access;
pub mod boolean;
pub mod integer;
pub mod float;
pub mod string;
pub mod compound;
pub mod instance;
pub mod typespec;
pub mod local;
pub mod function;
pub mod control;
pub mod structs;
pub mod class;
pub mod namespace;
pub mod declaration;

use std::collections::HashMap;

use crate::parser::{
    grammar::{Grammar, Rule},
    precedence::*
};
use pest_consume::{Error, match_nodes, Parser};
use expression::{Expression, If};
use unaryop::{UnaryOperation, UnaryOperator};
use binop::{BinaryOperation, BinaryOperator};
use boolean::Boolean;
use integer::Integer;
use float::Float;
use string::Str;
use compound::{Array, Tuple};
use instance::{Members, StructConstructor, ClassConstructor};
use access::{
    Identifier, Dotted, ScopeResolution, Index, Call,
    Import, Use,
    DefineArgument, DefineArguments,
    CallArgument, CallArguments
};
use typespec::{GenericCallArguments, GenericDefArguments, GenericDefArgument,
    TypeSpecifier, TypeDefinition
};
use local::{Local, Reference};
use function::{Function, FunctionSignature};
use control::{Return, Break, Continue};
use structs::{Struct, StructDeclaration};
use class::{Class, ClassConstructMethod};
use namespace::Namespace;
use declaration::{Declaration, Initialisation, DeclarationType};

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

#[derive(Debug)]
pub enum Visibility {
    Public,
    Private
}

impl Visibility {
    fn from_node(node: Node) -> Self {
        if node.as_str() == "public" { Self::Public } else { Self::Private }
    }
}

#[derive(Debug)]
#[enum_dispatch(ASTNode)]
pub enum NodeType {
    Expression(Expression),
    UnaryOperation(UnaryOperation),
    BinaryOperation(BinaryOperation),

    Reference(Reference),
    Boolean(Boolean),
    Integer(Integer),
    Float(Float),
    String(Str),

    Array(Array),
    Tuple(Tuple),
    StructConstructor(StructConstructor),
    ClassConstructor(ClassConstructor),

    TypeSpecifier(TypeSpecifier),
    Local(Local),
    Identifier(Identifier),
    Dotted(Dotted),
    ScopeResolution(ScopeResolution),
    Index(Index),
    Call(Call),

    Return(Return),
    Break(Break),
    Continue(Continue),
    Function(Function),
    TypeDefinition(TypeDefinition),

    Import(Import),
    Use(Use),

    Struct(Struct),
    Class(Class),
    ClassConstructMethod(ClassConstructMethod),
    Namespace(Namespace),
    Declaration(Declaration),
    Initialisation(Initialisation),

    If(If)
}

pub fn get_statements_from_node(input: Node) -> Vec<NodeType> {
    let mut statements: Vec<NodeType> = vec![];
    for node in input.into_children() {
        match node.as_rule() {
            Rule::EOI => {},
            Rule::expr_statement => 
                statements.push(NodeType::Expression(Grammar::expr_statement(node).unwrap())),
            Rule::if_stmt =>
                statements.push(NodeType::If(Grammar::if_stmt(node).unwrap())),
            Rule::return_stmt =>
                statements.push(NodeType::Return(Grammar::return_stmt(node).unwrap())),
            Rule::break_stmt =>
                statements.push(NodeType::Break(Grammar::break_stmt(node).unwrap())),
            Rule::continue_stmt =>
                statements.push(NodeType::Continue(Grammar::continue_stmt(node).unwrap())),
            Rule::function =>
                statements.push(NodeType::Function(Grammar::function(node).unwrap())),
            Rule::type_def =>
                statements.push(NodeType::TypeDefinition(Grammar::type_def(node).unwrap())),
            Rule::import_stmt =>
                statements.push(NodeType::Import(Grammar::import_stmt(node).unwrap())),
            Rule::use_stmt =>
                statements.push(NodeType::Use(Grammar::use_stmt(node).unwrap())),
            Rule::struct_def =>
                statements.push(NodeType::Struct(Grammar::struct_def(node).unwrap())),
            Rule::class_def =>
                statements.push(NodeType::Class(Grammar::class_def(node).unwrap())),
            Rule::class_construct_method =>
                statements.push(NodeType::ClassConstructMethod(Grammar::class_construct_method(node).unwrap())),
            Rule::namespace =>
                statements.push(NodeType::Namespace(Grammar::namespace(node).unwrap())),
            Rule::declaration =>
                statements.push(Grammar::declaration(node).unwrap()),
            _ => {}
        }
    }
    statements
}

#[enum_dispatch]
trait ASTNode {
    fn get_span(&self) -> Span;
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
    fn tuple(input: Node) -> Result<Tuple> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(exprs)..] => {
                let mut elements: Vec<NodeType> = vec![];
                for expr in exprs {
                    elements.push(expr);
                }
                Tuple::new(elements, span)
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
                StructConstructor::new(scope, None, members, span),
            [scope_resolution(scope), generic_call_args(args), struct_map(members)] =>
                StructConstructor::new(scope, Some(args), members, span)
        ))
    }

    fn class_constructor(input: Node) -> Result<ClassConstructor> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [scope_resolution(scope), call_arguments(args)] =>
                ClassConstructor::new(scope, None, args, span),
            [scope_resolution(scope), generic_call_args(generic_args), call_arguments(args)] =>
                ClassConstructor::new(scope, Some(generic_args), args, span)
        ))
    }

    fn call_arguments(input: Node) -> Result<CallArguments> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(exprs)..] => {
                let mut args: Vec<CallArgument> = vec![];
                for expr in exprs {
                    let expr_span: Span = expr.get_span();
                    args.push((Box::new(expr), expr_span));
                }
                (args, span)
            }
        ))
    }

    fn defarg(input: Node) -> Result<DefineArgument> {
        Ok(match_nodes!(input.into_children();
            [ident(ident), typespec(typ)] => {
                let typ_span: Span = typ.get_span();
                ((ident.get_name(), ident.get_span()), (typ, typ_span))
            }
        ))
    }
    fn defargs(input: Node) -> Result<DefineArguments> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [defarg(defargs)..] => {
                let mut args: Vec<DefineArgument> = vec![];
                for arg in defargs {
                    args.push(arg);
                }
                (args, span)
            }
        ))
    }

    fn type_array(input: Node) -> Result<(u32, Span)> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [integer_decimal(size)] => (size as u32, span),
            [] => (0, span)
        ))
    }
    fn type_tuple(input: Node) -> Result<Vec<TypeSpecifier>> {
        Ok(match_nodes!(input.into_children();
            [typespec(types)..] => {
                let mut tuple_types: Vec<TypeSpecifier> = vec![];
                for typ in types {
                    tuple_types.push(typ);
                }
                tuple_types
            }
        ))
    }
    fn typename(input: Node) -> Result<Vec<(String, Span)>> {
        Ok(match_nodes!(input.into_children();
            [scope_resolution(scope)] => scope
        ))
    }
    fn typespec(input: Node) -> Result<TypeSpecifier> {
        let span: Span = Span::from_span(input.as_span());
        let mut scope: Option<Vec<(String, Span)>> = Some(vec![]);
        let mut tuple: Option<Vec<TypeSpecifier>> = Some(vec![]);
        let mut pointer_level: u8 = 0;
        let mut array_dimensions: Vec<(u32, Span)> = vec![];
        let mut generic_args: Option<GenericCallArguments> = None;
        for node in input.into_children() {
            match node.as_rule() {
                Rule::typename => { scope = Some(Self::typename(node)?); },
                Rule::type_tuple => { tuple = Some(Self::type_tuple(node)?); }
                Rule::pointer => { pointer_level += 1; },
                Rule::type_array => { 
                    let dimension = Self::type_array(node)?;
                    array_dimensions.push(dimension)
                },
                Rule::generic_call_args => { generic_args = Some(Self::generic_call_args(node)?); },
                _ => { panic!("Invalid Type Specifier"); }
            }
        }
        Ok(TypeSpecifier::new(scope, tuple, generic_args, pointer_level, array_dimensions, span))
    }
    fn generic_call_args(input: Node) -> Result<GenericCallArguments> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [typespec(types)..] => {
                let mut args: GenericCallArguments = vec![];
                for typ in types {
                    args.push((Box::new(typ), span));
                }
                args
            }
        ))
    }

    fn index(input: Node) -> Result<NodeType> {
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => expr
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
    fn mut_ref(input: Node) -> Result<()> {
        Ok(())
    }
    fn immut_ref(input: Node) -> Result<()> {
        Ok(())
    }
    fn reference(input: Node) -> Result<Reference> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [mut_ref(_ref), scope_resolution(path)] => Reference::new(path, true, span),
            [immut_ref(_ref), scope_resolution(path)] => Reference::new(path, false, span)
        ))
    }
    fn ident(input: Node) -> Result<Identifier> {
        let span: Span = Span::from_span(input.as_span());
        Ok(Identifier::new(input.as_str().to_owned(), span))
    }
    fn access(input: Node) -> Result<NodeType> {
        access::build_access(input)
    }

    fn term(input: Node) -> Result<NodeType> {
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => expr,
            [unary(unary)] => NodeType::UnaryOperation(unary),
            [reference(reference)] => NodeType::Reference(reference),
            [boolean(boolean)] => NodeType::Boolean(boolean),
            [integer(int)] => NodeType::Integer(int),
            [float(float)] => NodeType::Float(float),
            [string(string)] => NodeType::String(string),
            [array(array)] => NodeType::Array(array),
            [tuple(tuple)] => NodeType::Tuple(tuple),
            [struct_constructor(struc)] => NodeType::StructConstructor(struc),
            [class_constructor(clas)] => NodeType::ClassConstructor(clas),
            [access(access)] => access
        ))
    }
    
    fn plus(input: Node) -> Result<UnaryOperator> {
        Ok(UnaryOperator::Positive)
    }
    fn minus(input: Node) -> Result<UnaryOperator> {
        Ok(UnaryOperator::Negative)
    }
    fn logical_not(input: Node) -> Result<UnaryOperator> {
        Ok(UnaryOperator::LogicalNot)
    }
    fn unary(input: Node) -> Result<UnaryOperation> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [operator, term(operand)] =>
                UnaryOperation::new(UnaryOperator::from_node(operator), operand, span)
        ))
    }

    #[prec_climb(term, PRECCLIMBER)]
    fn expression(left: NodeType, op: Node, right: NodeType) -> Result<NodeType> {
        expression::expression(left, op, right)
    }

    // Statements

    fn block(input: Node) -> Result<Vec<NodeType>> {
        Ok(get_statements_from_node(input))
    }
    fn expr_statement(input: Node) -> Result<Expression> { 
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => Expression::new(expr, span)
        ))
    }

    fn else_stmt(input: Node) -> Result<Vec<NodeType>> {
        Ok(match_nodes!(input.into_children();
            [block(stmts)] => stmts
        ))
    }
    fn else_if_stmt(input: Node) -> Result<Box<If>> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(expr), block(block)] =>
                Box::new(If::new(expr, block, vec![], vec![], span)),
        ))
    }
    fn else_if_stmts(input: Node) -> Result<Vec<Box<If>>> {
        Ok(match_nodes!(input.into_children();
            [else_if_stmt(stmts)..] => {
                let mut elif_stmts: Vec<Box<If>> = vec![];
                for stmt in stmts {
                    elif_stmts.push(stmt);
                }
                elif_stmts
            }
        ))
    }
    fn if_stmt(input: Node) -> Result<If> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(expr), block(block)] =>
                If::new(expr, block, vec![], vec![], span),
            [expression(expr), block(block), else_if_stmts(elif_stmts)] =>
                If::new(expr, block, elif_stmts, vec![], span),
            [expression(expr), block(block), else_stmt(else_stmt)] =>
                If::new(expr, block, vec![], else_stmt, span),
            [expression(expr), block(block), else_if_stmts(elif_stmts), else_stmt(else_stmt)] =>
                If::new(expr, block, elif_stmts, else_stmt, span),
        ))
    }

    fn return_stmt(input: Node) -> Result<Return> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [expression(expr)] => Return::new(expr, span)
        ))
    }
    fn break_stmt(input: Node) -> Result<Break> {
        let span: Span = Span::from_span(input.as_span());
        Ok(Break::new(span))
    }
    fn continue_stmt(input: Node) -> Result<Continue> {
        let span: Span = Span::from_span(input.as_span());
        Ok(Continue::new(span))
    }

    fn function_block(input: Node) -> Result<Vec<NodeType>> {
        Ok(get_statements_from_node(input))
    }
    fn function_output(input: Node) -> Result<TypeSpecifier> {
        Ok(match_nodes!(input.into_children();
            [typespec(output)] => output

        ))
    }
    fn function_signature(input: Node) -> Result<FunctionSignature> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [ident(ident), defargs(params), function_output(output)] =>
                FunctionSignature::new(ident, None, params, output, span),
            [ident(ident), generic_def_args(generic_args), defargs(params), function_output(output)] =>
                FunctionSignature::new(ident, Some(generic_args), params, output, span)
        ))
    }
    fn function(input: Node) -> Result<Function> {
        function::build_function(input)
    }

    fn type_def(input: Node) -> Result<TypeDefinition> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [ident(name), typespec(typ)] =>
                TypeDefinition::new(name, None, typ, span),
            [ident(name), generic_def_args(generic_args), typespec(typ)] =>
                TypeDefinition::new(name, Some(generic_args), typ, span)
        ))
    }

    fn import_stmt(input: Node) -> Result<Import> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [string(path)] =>
                Import::new(path, None, span),
            [string(path), ident(name)] =>
                Import::new(path, Some(name), span)
        ))
    }
    fn use_all(input: Node) -> Result<bool> {
        Ok(true)
    }
    fn use_stmt(input: Node) -> Result<Use> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [scope_resolution(scope), use_all(use_all)] =>
                Use::new(scope, use_all, span),
            [scope_resolution(scope)] =>
                Use::new(scope, false, span)
        ))
    }

    fn mutable(input: Node) -> Result<DeclarationType> {
        Ok(DeclarationType::Mutable)
    }
    fn immutable(input: Node) -> Result<DeclarationType> {
        Ok(DeclarationType::Immutable)
    }
    fn declaration(input: Node) -> Result<NodeType> {
        declaration::build_declaration(input)
    }

    fn struct_declaration(input: Node) -> Result<StructDeclaration> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [visibility(visibility), ident(ident), typespec(typ)] =>
                StructDeclaration::new(visibility, ident, typ, span)
        ))
    }
    fn struct_block(input: Node) -> Result<Vec<StructDeclaration>> {
        Ok(match_nodes!(input.into_children();
            [struct_declaration(decls)..] => {
                let mut members: Vec<StructDeclaration> = vec![];
                for decl in decls {
                    members.push(decl);
                }
                members
            }
        ))
    }
    fn struct_def(input: Node) -> Result<Struct> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [ident(ident), generic_def_args(generic_args), struct_block(decls)] =>
                Struct::new(Visibility::Public, ident, Some(generic_args), decls, span),
            [visibility(visibility), ident(ident), generic_def_args(generic_args), struct_block(decls)] =>
                Struct::new(visibility, ident, Some(generic_args), decls, span)
        ))
    }

    fn class_implements(_input: Node) -> Result<()> {
        Ok(())
    }
    fn class_extends(_input: Node) -> Result<()> {
        Ok(())
    }
    fn class_block(input: Node) -> Result<Vec<NodeType>> {
        Ok(get_statements_from_node(input))
    }
    fn class_construct_method(input: Node) -> Result<ClassConstructMethod> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [defargs(args), function_block(stmts)] =>
                ClassConstructMethod::new(args, stmts, span)
        ))
    }
    fn class_def(input: Node) -> Result<Class> {
        class::build_class(input)
    }

    fn namespace(input: Node) -> Result<Namespace> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [scope_resolution(scope), block(body)] =>
                Namespace::new(scope, body, span)
        ))
    }

    fn generic_def_arg(input: Node) -> Result<GenericDefArgument> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [ident(ident)] => GenericDefArgument::new(ident, false, false, None, span),
            [ident(ident), class_implements(_), ident(constraint)] =>
                GenericDefArgument::new(ident, true, false, Some(constraint), span),
            [ident(ident), class_extends(_), ident(constraint)] =>
                GenericDefArgument::new(ident, false, true, Some(constraint), span),
        ))
    }
    fn generic_def_args(input: Node) -> Result<GenericDefArguments> {
        let span: Span = Span::from_span(input.as_span());
        Ok(match_nodes!(input.into_children();
            [generic_def_arg(args)..] => {
                let mut generic_args: Vec<GenericDefArgument> = vec![];
                for arg in args {
                    generic_args.push(arg);
                }
                (generic_args, span)
            }
        ))
    }

    fn visibility(input: Node) -> Result<Visibility> {
        if input.as_str() == "public" {
            return Ok(Visibility::Public);
        }
        Ok(Visibility::Private)
    }

    fn file(input: Node) -> Result<AST> {
        let span: Span = Span::from_span(input.as_span());
        let statements: Vec<NodeType> = get_statements_from_node(input);
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