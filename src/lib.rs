mod eval;
mod parse;
mod solve;
mod util;
pub use eval::{eval, evaluate_ast};
pub use parse::{Node, Parser, Token};
