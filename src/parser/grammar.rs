use pest_consume::Parser;

#[derive(Parser)]
#[grammar = "./parser/grammar.pest"]
pub struct Grammar;