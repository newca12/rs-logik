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
    #[end]
    End,
    #[error]
    Error,
}

pub struct Parser<'s> {
    lexer: Lexer<Token, &'s str>,
}

#[derive(Debug)]
pub enum Node<'s> {
    IdentNode(&'s str),
    UnopNode(&'s str, Box<Node<'s>>),
    BinOpNode(&'s str, Box<Node<'s>>, Box<Node<'s>>),
    ExprNode(Box<Node<'s>>),
}

impl<'s> Parser<'s> {
    pub fn new(expr: &'s str) -> Self {
        Self {
            lexer: Token::lexer(expr),
        }
    }

    pub fn parse(&mut self) -> Result<Node<'s>, Range<usize>> {
        match self.expr() {
            Some(res) => {
                if self.expect(Token::End) {
                    Ok(res)
                } else {
                    Err(self.lexer.range())
                }
            }
            None => Err(self.lexer.range()),
        }
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

    fn expr(&mut self) -> Option<Node<'s>> {
        if self.expect(Token::Parent) {
            self.accept(Token::Parent);
            let expr = self.expr()?;
            self.accept(Token::Parent);
            return Some(expr);
        }
        self.e3()
    }

    fn e3(&mut self) -> Option<Node<'s>> {
        let left = self.e2()?;
        match self.accept(Token::OpInduces) {
            Some((_, s)) => {
                let right = self.e3()?;
                Some(Node::BinOpNode(s, Box::new(left), Box::new(right)))
            }
            None => Some(left),
        }
    }

    fn e2(&mut self) -> Option<Node<'s>> {
        let left = self.e1()?;
        match self.accept(Token::OpOr) {
            Some((_, s)) => {
                let right = self.e2()?;
                Some(Node::BinOpNode(s, Box::new(left), Box::new(right)))
            }
            None => Some(left),
        }
    }

    fn e1(&mut self) -> Option<Node<'s>> {
        let left = self.e0()?;
        match self.accept(Token::OpAnd) {
            Some((_, s)) => {
                let right = self.e1()?;
                Some(Node::BinOpNode(s, Box::new(left), Box::new(right)))
            }
            None => Some(left),
        }
    }

    fn e0(&mut self) -> Option<Node<'s>> {
        match self.accept(Token::Ident) {
            Some((_, s)) => Some(Node::IdentNode(s)),
            None => {
                let (_, s) = self.accept(Token::OpNeg)?;
                let right = self.expr()?;
                Some(Node::UnopNode(s, Box::new(right)))
            }
        }
    }
}
