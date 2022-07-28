
#[derive(Debug)]
enum PrimitiveType {
    Boolean,
    Character,
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    Unsigned8,
    Unsigned16,
    Unsigned32,
    Unsigned64,
    Float32,
    Float64,
}

#[derive(Debug)]
enum ObjectType {
    Class,
    Struct,
    Enum,
}

#[derive(Debug)]
pub struct Type {
    pointer_level: u8,
    prim_type: Option<PrimitiveType>,
    object_type: Option<ObjectType>,
    object_type_scope: Option<Vec<String>>,
    is_array: bool,
    array_dimensions: u8
}