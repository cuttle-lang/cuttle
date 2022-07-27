use crate::grammar::Rule;

use pest::{
    prec_climber::{Assoc, Operator, PrecClimber},
};

lazy_static! {
    pub static ref PRECCLIMBER: PrecClimber<Rule> = build_precedence_climber();
}

fn build_precedence_climber() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
        Operator::new(Rule::assignment, Assoc::Left),
        Operator::new(Rule::logical_or, Assoc::Left),
        Operator::new(Rule::logical_and, Assoc::Left),
        Operator::new(Rule::equal, Assoc::Right)
            | Operator::new(Rule::not_equal, Assoc::Right),
        Operator::new(Rule::greater_than_or_equal, Assoc::Left)
            | Operator::new(Rule::less_than_or_equal, Assoc::Left)
            | Operator::new(Rule::greater_than, Assoc::Left)
            | Operator::new(Rule::less_than, Assoc::Left),
        Operator::new(Rule::bitwise_xor, Assoc::Left)
            | Operator::new(Rule::bitwise_or, Assoc::Left),
        Operator::new(Rule::bitwise_and, Assoc::Left),
        Operator::new(Rule::shift_right, Assoc::Left)
            | Operator::new(Rule::shift_left, Assoc::Left),
        Operator::new(Rule::plus, Assoc::Left)
            | Operator::new(Rule::minus, Assoc::Left),
        Operator::new(Rule::modulus, Assoc::Left)
            | Operator::new(Rule::divide, Assoc::Left)
            | Operator::new(Rule::multiply, Assoc::Left),
        Operator::new(Rule::exponent, Assoc::Right),
        Operator::new(Rule::cast, Assoc::Left)
    ])
}
