use logos::{Lexer, Logos};
use std::collections::{HashMap, HashSet};
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

#[derive(Clone, Debug)]
pub enum Node<'s> {
    IdentNode(&'s str),
    ValueNode(bool),
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

    pub fn parse(&mut self) -> Result<Node<'s>, (Range<usize>, &'s str)> {
        match self.expr() {
            Some(res) => {
                if self.expect(Token::End) {
                    Ok(res)
                } else {
                    Err((self.lexer.range(), self.lexer.slice()))
                }
            }
            None => Err((self.lexer.range(), self.lexer.slice())),
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
            None => self.eident(),
        }
    }

    fn eident(&mut self) -> Option<Node<'s>> {
        match self.accept(Token::Ident) {
            Some((_, s)) => Some(Node::IdentNode(s)),
            None => {
                let (_, s) = self.accept(Token::Value)?;
                match s {
                    "0" => Some(Node::ValueNode(false)),
                    "1" => Some(Node::ValueNode(true)),
                    _ => None,
                }
            }
        }
    }
}

pub fn evaluate_all<'s>(ast: &Node<'s>) {
    let mut vars = HashSet::new();
    get_vars(ast, &mut vars);
    let (envs, tab) = make_env(&vars);
    if vars.len() > 0 {
        println!("Table de vérité:\n");
        for var in vars.iter() {
            print!("{}\t", var);
        }
        println!("({} entrées)", vars.len());
        for (i, env) in envs.iter().enumerate() {
            for t in tab[i].iter() {
                print!("{}\t", *t as u8);
            }
            println!("{}", evaluate(ast, env) as u8);
        }
    } else {
        println!("Valeur: {}", evaluate(ast, &HashMap::new()) as u8);
    }
}

fn evaluate<'s>(ast: &Node<'s>, env: &HashMap<&'s str, bool>) -> bool {
    match ast {
        Node::ValueNode(v) => *v,
        Node::IdentNode(i) => env[i],
        Node::ExprNode(expr) => evaluate(expr, env),
        Node::UnopNode(_, e) => !evaluate(e, env),
        Node::BinOpNode(op, l, r) => match op {
            &"ou" => evaluate(l, env) || evaluate(r, env),
            &"et" => evaluate(l, env) && evaluate(r, env),
            _ => (!evaluate(l, env)) || evaluate(r, env),
        },
    }
}

fn get_vars<'s>(ast: &Node<'s>, vars: &mut HashSet<&'s str>) {
    match ast {
        Node::ValueNode(_) => (),
        Node::IdentNode(i) => {
            if !vars.contains(i) {
                vars.insert(i);
            }
        }
        Node::UnopNode(_, e) => get_vars(e, vars),
        Node::BinOpNode(_, l, r) => {
            get_vars(l, vars);
            get_vars(r, vars);
        }
        Node::ExprNode(e) => get_vars(e, vars),
    }
}

fn make_env<'s>(vars: &HashSet<&'s str>) -> (Vec<HashMap<&'s str, bool>>, Vec<Vec<bool>>) {
    let tab = cartesian_product(vec![vec![false, true]; vars.len()]);
    let mut env_list = vec![];
    for lines in tab.iter() {
        let mut env = HashMap::new();
        for (i, v) in vars.iter().enumerate() {
            env.insert(*v, lines[i]);
        }
        env_list.push(env);
    }
    return (env_list, tab);
}

fn cartesian_product<T: Clone>(lists: Vec<Vec<T>>) -> Vec<Vec<T>> {
    match lists.split_first() {
        Some((first, rest)) => {
            let init: Vec<Vec<T>> = first.iter().cloned().map(|n| vec![n]).collect();
            rest.iter()
                .cloned()
                .fold(init, |vec, list| partial_cartesian(vec, list))
        }
        None => vec![],
    }
}

fn partial_cartesian<T: Clone>(a: Vec<Vec<T>>, b: Vec<T>) -> Vec<Vec<T>> {
    a.into_iter()
        .flat_map(|xs| {
            b.iter()
                .cloned()
                .map(|y| {
                    let mut vec = xs.clone();
                    vec.push(y);
                    vec
                })
                .collect::<Vec<_>>()
        })
        .collect()
}
