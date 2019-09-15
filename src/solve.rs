use crate::Node;

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
                Node::BinOpNode("ou", Box::new(Node::UnopNode("non", Box::new(left))), Box::new(right))
            } else {
                // Reforming node as `ast` has moved
                Node::BinOpNode(op, left, right)
            }
        }
        _ => *ast,
    }
}

#[cfg(test)]
mod test {
    use super::Node;
    use super::{distribute_or, remove_implications};

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
        let expr_a_in = Node::BinOpNode(
            "->",
            Box::new(Node::IdentNode("a")),
            Box::new(Node::IdentNode("b")),
        );
        let expr_a_out = remove_implications(Box::from(expr_a_in.clone()));
        let expr_a_expected = Node::BinOpNode(
            "ou",
            Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("a")))),
            Box::new(Node::IdentNode("b")),
        );

        println!(
            "In: {}\tExpected: {}\tActual: {}",
            expr_a_in, expr_a_expected, expr_a_out
        );
        assert_eq!(expr_a_expected, expr_a_out);
        let expr_b_in = Node::BinOpNode(
            "->",
            Box::new(expr_a_in.clone()),
            Box::new(expr_a_in.clone()),
        );
        let expr_b_out = remove_implications(Box::new(expr_b_in.clone()));
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
}
