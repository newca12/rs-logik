use crate::eval::pprint;
use logos::{Lexer, Logos};
use std::ops::Range;

#[derive(Copy, Clone, Debug, Logos, PartialEq)]
pub enum Token {
    #[token = "0"]
    #[token = "1"]
    Value,
    #[token = "->"]
    OpInduces,
    #[token = "et"]
    OpAnd,
    #[token = "ou"]
    OpOr,
    #[token = "non"]
    OpNeg,
    #[regex = "[a-zA-Z]+"]
    Ident,
    #[token = "("]
    #[token = ")"]
    Parent,
    #[token = ":"]
    Colon,
    #[end]
    End,
    #[error]
    Error,
}

pub struct Parser<'s> {
    lexer: Lexer<Token, &'s str>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node<'s> {
    IdentNode(&'s str),
    ValueNode(bool),
    UnopNode(&'s str, Box<Node<'s>>),
    BinOpNode(&'s str, Box<Node<'s>>, Box<Node<'s>>),
    ExprNode(Box<Node<'s>>),
}

impl<'s> std::fmt::Display for Node<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", pprint(self))
    }
}

impl<'s> Parser<'s> {
    pub fn new(expr: &'s str) -> Self {
        Self {
            lexer: Token::lexer(expr),
        }
    }

    pub fn parse(&mut self) -> Result<Node<'s>, (Range<usize>, &'s str)> {
        self.expr()
            .ok_or_else(|| (self.lexer.range(), self.lexer.slice()))
    }

    pub fn parse_command(&mut self) -> Result<(&'s str, Node<'s>), (Range<usize>, &'s str)> {
        self.command()
            .ok_or_else(|| (self.lexer.range(), self.lexer.slice()))
    }

    fn expect(&self, t: Token) -> bool {
        self.lexer.token == t
    }

    fn accept(&mut self, t: Token) -> Option<(Range<usize>, &'s str)> {
        if self.expect(t) {
            let res = Some((self.lexer.range(), self.lexer.slice()));
            self.lexer.advance();
            return res;
        }
        return None;
    }

    fn command(&mut self) -> Option<(&'s str, Node<'s>)> {
        let node = self.eident()?;
        self.accept(Token::Colon)?;
        let expr = self.expr()?;
        match node {
            Node::IdentNode(i) => Some((i, expr)),
            _ => None,
        }
    }

    fn expr(&mut self) -> Option<Node<'s>> {
        if self.expect(Token::Parent) {
            self.accept(Token::Parent)?;
            let expr = self.expr()?;
            self.accept(Token::Parent)?;
            return Some(expr);
        }
        self.e4()
    }

    fn e4(&mut self) -> Option<Node<'s>> {
        let left = self.e3()?;
        match self.accept(Token::OpInduces) {
            Some((_, s)) => {
                let right = self.e4()?;
                Some(Node::BinOpNode(s, Box::new(left), Box::new(right)))
            }
            None => Some(left),
        }
    }

    fn e3(&mut self) -> Option<Node<'s>> {
        let left = self.e2()?;
        match self.accept(Token::OpOr) {
            Some((_, s)) => {
                let right = self.e3()?;
                Some(Node::BinOpNode(s, Box::new(left), Box::new(right)))
            }
            None => Some(left),
        }
    }

    fn e2(&mut self) -> Option<Node<'s>> {
        let left = self.e1()?;
        match self.accept(Token::OpAnd) {
            Some((_, s)) => {
                let right = self.e2()?;
                Some(Node::BinOpNode(s, Box::new(left), Box::new(right)))
            }
            None => Some(left),
        }
    }

    fn e1(&mut self) -> Option<Node<'s>> {
        match self.accept(Token::OpNeg) {
            Some((_, s)) => {
                let right = self.e1()?;
                Some(Node::UnopNode(s, Box::new(right)))
            }
            None => self.e0(),
        }
    }

    fn e0(&mut self) -> Option<Node<'s>> {
        match self.accept(Token::Parent) {
            Some((_, _)) => {
                let e = self.expr()?;
                self.accept(Token::Parent)?;
                Some(Node::ExprNode(Box::new(e)))
            }
            None => self.eident().or_else(|| self.evalue()),
        }
    }

    fn eident(&mut self) -> Option<Node<'s>> {
        match self.accept(Token::Ident) {
            Some((_, s)) => Some(Node::IdentNode(s)),
            None => None,
        }
    }

    fn evalue(&mut self) -> Option<Node<'s>> {
        match self.accept(Token::Value) {
            Some((_, s)) => match s {
                "0" => Some(Node::ValueNode(false)),
                "1" => Some(Node::ValueNode(true)),
                _ => None,
            },
            None => None,
        }
    }
}

impl<'s> Node<'s> {
    pub fn is_op(&self, op: &'s str) -> bool {
        match self {
            Node::BinOpNode(ast_op, _, _) => &op == ast_op,
            Node::UnopNode(ast_op, _) => &op == ast_op,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Node, Parser};

    #[test]
    fn node_is_op_binop() {
        let node = Node::BinOpNode(
            "ou",
            Box::new(Node::ValueNode(false)),
            Box::new(Node::ValueNode(true)),
        );
        assert!(node.is_op("ou"));
        assert!(!node.is_op("et"));
    }

    #[test]
    fn node_is_op_unop() {
        let node = Node::UnopNode("non", Box::new(Node::ValueNode(false)));
        assert!(node.is_op("non"));
    }

    #[test]
    fn node_is_op_other() {
        let node = Node::IdentNode("a");
        assert!(!node.is_op("any"));
    }

    #[test]
    fn parse_binop_1() {
        let mut parser = Parser::new("a -> b");
        assert_eq!(
            parser.parse().expect("Couldn't parse input"),
            Node::BinOpNode(
                "->",
                Box::new(Node::IdentNode("a")),
                Box::new(Node::IdentNode("b"))
            )
        );
    }

    #[test]
    fn parse_binop_2() {
        let mut parser = Parser::new("0 ou 1");
        assert_eq!(
            parser.parse().expect("Couldn't parse input"),
            Node::BinOpNode(
                "ou",
                Box::new(Node::ValueNode(false)),
                Box::new(Node::ValueNode(true))
            )
        );
    }

    #[test]
    fn parse_unop() {
        let mut parser = Parser::new("non a");
        assert_eq!(
            parser.parse().expect("Couldn't parse input"),
            Node::UnopNode("non", Box::new(Node::IdentNode("a")))
        );
    }

    #[test]
    fn parse_complex() {
        let mut parser = Parser::new("a et (non b ou c) -> 1");
        assert_eq!(
            parser.parse().expect("Couldn't parse input"),
            Node::BinOpNode(
                "->",
                Box::new(Node::BinOpNode(
                    "et",
                    Box::new(Node::IdentNode("a")),
                    Box::new(Node::ExprNode(Box::new(Node::BinOpNode(
                        "ou",
                        Box::new(Node::UnopNode("non", Box::new(Node::IdentNode("b")))),
                        Box::new(Node::IdentNode("c"))
                    ))))
                )),
                Box::new(Node::ValueNode(true))
            )
        );
    }
}
