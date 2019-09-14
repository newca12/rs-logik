mod eval;
mod parse;
mod util;
pub use eval::{eval, evaluate_ast};
pub use parse::{Node, Parser, Token};
