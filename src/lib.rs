mod eval;
mod parse;
mod solve;
mod util;
pub use eval::{eval, TruthTable};
pub use parse::{Node, Parser, Token};
pub use solve::{distribute_or};

