use crate::Node;
use std::fmt::{self, Display, Formatter};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Litteral<'s> {
    identifier: &'s str,
    negated: bool,
}
pub type Clause<'s> = Vec<Litteral<'s>>;

impl<'s> From<&'s str> for Litteral<'s> {
    fn from(identifier: &'s str) -> Self {
        Self {
            identifier,
            negated: false,
        }
    }
}

impl<'s> Into<Node<'s>> for Litteral<'s> {
    fn into(self) -> Node<'s> {
        if self.negated {
            Node::UnopNode("non", Box::new(Node::IdentNode(self.identifier)))
        } else {
            Node::IdentNode(self.identifier)
        }
    }
}

impl<'s> Display for Litteral<'s> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.negated {
            write!(f, "{}", self.identifier)?;
        } else {
            write!(f, "non {}", self.identifier)?;
        }
        Ok(())
    }
}

pub fn distribute_or<'s>(ast: Box<Node<'s>>) -> Option<Node<'s>> {
    match *ast {
        Node::BinOpNode("->", _, _) => None,
        Node::BinOpNode("ou", oexpr_a, oexpr_b) => {
            let expr_a = distribute_or(oexpr_a.clone())?;
            let expr_b = distribute_or(oexpr_b.clone())?;
            if let Node::BinOpNode("et", expr_c, expr_d) = expr_b {
                let expr_c = distribute_or(expr_c)?;
                let expr_d = distribute_or(expr_d)?;

                let left = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(expr_a.clone()),
                    Box::new(expr_c),
                )))?;
                let right = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(expr_a),
                    Box::new(expr_d),
                )))?;
                Some(Node::BinOpNode("et", Box::new(left), Box::new(right)))
            } else if let Node::BinOpNode("et", expr_c, expr_d) = expr_a {
                let expr_c = distribute_or(expr_c)?;
                let expr_d = distribute_or(expr_d)?;
                let left = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(expr_c),
                    Box::new(expr_b.clone()),
                )))?;
                let right = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(expr_d),
                    Box::new(expr_b),
                )))?;

                Some(Node::BinOpNode("et", Box::new(left), Box::new(right)))
            } else {
                Some(Node::BinOpNode(
                    "ou",
                    Box::new(distribute_or(oexpr_a)?),
                    Box::new(distribute_or(oexpr_b)?),
                ))
            }
        }
        Node::BinOpNode(op, left, right) => Some(Node::BinOpNode(
            op,
            Box::new(distribute_or(left)?),
            Box::new(distribute_or(right)?),
        )),
        Node::ExprNode(expr) => distribute_or(expr),
        _ => Some(*ast),
    }
}

pub fn remove_implications<'s>(ast: Box<Node<'s>>) -> Node<'s> {
    match *ast {
        Node::BinOpNode(op, left, right) => {
            if op == "->" {
                let left = remove_implications(left);
                let right = remove_implications(right);
                Node::BinOpNode(
                    "ou",
                    Box::new(Node::UnopNode("non", Box::new(left))),
                    Box::new(right),
                )
            } else {
                // Reforming node as `ast` has moved
                Node::BinOpNode(op, left, right)
            }
        }
        _ => *ast,
    }
}

pub fn remove_negations<'s>(ast: Box<Node<'s>>) -> Option<Node<'s>> {
    match *ast {
        Node::BinOpNode("->", _, _) => None,
        Node::BinOpNode(op, left, right) => {
            let left = Box::new(remove_negations(left)?);
            let right = Box::new(remove_negations(right)?);
            Some(Node::BinOpNode(op, left, right))
        }
        Node::UnopNode("non", expr_a) => match *expr_a {
            Node::BinOpNode("ou", expr_b, expr_c) => {
                let expr_b = Box::new(remove_negations(expr_b)?);
                let expr_c = Box::new(remove_negations(expr_c)?);
                Some(Node::BinOpNode("et", expr_b, expr_c))
            }
            Node::BinOpNode("et", expr_b, expr_c) => {
                let expr_b = Box::new(remove_negations(Box::new(Node::UnopNode("non", expr_b)))?);
                let expr_c = Box::new(remove_negations(Box::new(Node::UnopNode("non", expr_c)))?);
                Some(Node::BinOpNode("ou", expr_b, expr_c))
            }
            Node::UnopNode("non", expr_b) => Some(*expr_b),
            _ => remove_negations(expr_a),
        },
        Node::IdentNode(i) => Some(Node::UnopNode("non", Box::new(Node::IdentNode(i)))),
        Node::ValueNode(v) => Some(Node::UnopNode("non", Box::new(Node::ValueNode(v)))),
        Node::ExprNode(expr) => remove_negations(expr),
        _ => Some(*ast),
    }
}

pub fn extract_clauses<'s>(ast: Box<Node<'s>>) -> Option<Vec<Clause<'s>>> {
    match *ast {
        Node::IdentNode(i) => Some(vec![vec![Litteral::from(i)]]),
        Node::UnopNode("non", right) => {
            if let Node::IdentNode(identifier) = *right {
                Some(vec![vec![Litteral::from(identifier)]])
            } else {
                None
            }
        }
        Node::BinOpNode(_, left, right) => extract_clauses(left).and_then(|l| {
            extract_clauses(right).map(|r| l.into_iter().chain(r.into_iter()).collect())
        }),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::Node;
    use super::{distribute_or, extract_clauses, remove_implications, remove_negations, Litteral};

    #[test]
    fn test_distribute_or() {
        let input_node = Node::BinOpNode(
            "ou",
            Box::new(Node::ExprNode(Box::new(Node::BinOpNode(
                "et",
                Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("a")))),
                Box::new(Node::IdentNode("c")),
            )))),
            Box::new(Node::IdentNode("b")),
        );
        let output_node = distribute_or(Box::new(input_node)).expect("Couldn't distribute or");
        let expected_node = Node::BinOpNode(
            "et",
            Box::new(Node::BinOpNode(
                "ou",
                Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("a")))),
                Box::new(Node::IdentNode("b")),
            )),
            Box::new(Node::BinOpNode(
                "ou",
                Box::new(Node::IdentNode("c")),
                Box::new(Node::IdentNode("b")),
            )),
        );
        println!("Expected: {}\tActual: {}", expected_node, output_node);
        assert_eq!(expected_node, output_node);
    }

    #[test]
    fn test_remove_implications() {
        let expr_a_in = Box::new(Node::BinOpNode(
            "->",
            Box::new(Node::IdentNode("a")),
            Box::new(Node::IdentNode("b")),
        ));
        let expr_a_out = remove_implications(expr_a_in.clone());
        let expr_a_expected = Node::BinOpNode(
            "ou",
            Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("a")))),
            Box::new(Node::IdentNode("b")),
        );

        println!(
            "In: {}\tExpected: {}\tActual: {}",
            expr_a_in.clone(),
            expr_a_expected,
            expr_a_out
        );
        assert_eq!(expr_a_expected, expr_a_out);
        let expr_b_in = Box::new(Node::BinOpNode("->", expr_a_in.clone(), expr_a_in));
        let expr_b_out = remove_implications(expr_b_in.clone());
        let expr_b_expected = Node::BinOpNode(
            "ou",
            Box::new(Node::UnopNode("non", Box::new(expr_a_expected.clone()))),
            Box::new(expr_a_expected),
        );

        println!(
            "In: {}\tExpected: {}\tActual: {}",
            expr_b_in, expr_b_expected, expr_b_out
        );
        assert_eq!(expr_b_expected, expr_b_out);
    }

    #[test]
    fn test_remove_negations() {
        let expr_a_in = Box::new(Node::UnopNode(
            "non",
            Box::new(Node::BinOpNode(
                "et",
                Box::new(Node::IdentNode("a")),
                Box::new(Node::IdentNode("b")),
            )),
        ));
        let expr_a_out = remove_negations(expr_a_in.clone()).expect("Couldn't remove negations");
        let expr_a_expected = Node::BinOpNode(
            "ou",
            Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("a")))),
            Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("b")))),
        );
        println!(
            "In: {}\tExpected: {}\tActual: {}",
            expr_a_in, expr_a_expected, expr_a_out
        );
        assert_eq!(expr_a_expected, expr_a_out);

        let expr_b1 = remove_negations(Box::new(Node::UnopNode(
            "non",
            Box::new(Node::BinOpNode(
                "ou",
                Box::new(Node::IdentNode("a")),
                Box::new(Node::IdentNode("b")),
            )),
        )))
        .expect("Couldn't remove negations");
        let expr_b2 = remove_negations(Box::new(Node::BinOpNode(
            "et",
            Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("a")))),
            Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("b")))),
        )))
        .expect("Couldn't remove negations");
        println!("A: {}\tB: {}", expr_b1, expr_b2);
        assert_eq!(expr_b1, expr_b2);

        let expr_c_in = Box::new(Node::UnopNode(
            "non",
            Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("a")))),
        ));
        let expr_c_out = remove_negations(expr_c_in.clone()).expect("Couldn't remove negations");
        let expr_c_expected = Node::IdentNode("a");
        println!(
            "In: {}\tExpected: {}\tActual: {}",
            expr_c_in, expr_c_expected, expr_c_out
        );
        assert_eq!(expr_c_expected, expr_c_out);
    }

    #[test]
    fn test_extract_clauses() {
        let expr_a_in = Box::new(Node::BinOpNode(
            "ou",
            Box::new(Node::IdentNode("a")),
            Box::new(Node::IdentNode("b")),
        ));
        let clauses_a_out = extract_clauses(expr_a_in.clone());
        let clauses_a_expected = vec![vec![Litteral::from("a"), Litteral::from("b")]];

        println!(
            "In: {}\tExpected: {:?}\nActual: {:?}",
            expr_a_in, clauses_a_expected, clauses_a_out
        );
    }
}
