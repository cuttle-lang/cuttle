#![allow(dead_code)]

#[macro_use]
extern crate lazy_static;

#[allow(unused_imports)]
#[macro_use]
extern crate pest_derive;
extern crate pest;

#[allow(unused_imports)]
#[macro_use]
extern crate enum_dispatch;

extern crate colored;

pub mod parser;
pub mod types;

use std::{env, fs};
use parser::parse_cuttle;

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc = args.len();
    if argc < 2 {
        println!("Please specify source file path.");
        return;
    }

    let path = &args[1];
    if !path.ends_with(".ct") {
        println!("File specified was not a Cuttle (.ct) source file.");
        return;
    }

    let source = fs::read_to_string(path)
        .expect("Unable to read source file.");
    let file = parse_cuttle(&source);
    match file {
        Ok(_) => {
            println!("Successfully constructed AST.");

            let file = file.unwrap();
            println!("AST: {:#?}", file);
        },
        Err(e) => println!("Parsing Unsuccessful: \n {:?}", e)
    }
}