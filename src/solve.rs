use crate::Node;

pub fn distribute_or<'s>(ast: Box<Node<'s>>) -> Option<Node<'s>> {
    match *ast {
        Node::BinOpNode("->", _, _) => None,
        Node::BinOpNode("ou", expr_a, expr_b) => {
            if let Node::BinOpNode("et", expr_c, expr_d) = *expr_b {
                let left = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(distribute_or(expr_a.clone())?),
                    Box::new(distribute_or(expr_c)?),
                )))?;
                let right = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(distribute_or(expr_a)?),
                    Box::new(distribute_or(expr_d)?),
                )))?;
                Some(Node::BinOpNode("et", Box::new(left), Box::new(right)))
            } else if let Node::BinOpNode("et", expr_c, expr_d) = *expr_a {
                let left = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(distribute_or(expr_c)?),
                    Box::new(distribute_or(expr_b.clone())?),
                )))?;
                let right = distribute_or(Box::new(Node::BinOpNode(
                    "ou",
                    Box::new(distribute_or(expr_d)?),
                    Box::new(distribute_or(expr_b)?),
                )))?;
                Some(Node::BinOpNode("et", Box::new(left), Box::new(right)))
            } else {
                Some(Node::BinOpNode(
                    "ou",
                    Box::new(distribute_or(expr_a)?),
                    Box::new(distribute_or(expr_b)?),
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

#[cfg(test)]
mod test {
    use super::distribute_or;
    use super::Node;

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
}
