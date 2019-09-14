use crate::parse::{Node, Parser};
use crate::util::cartesian_product;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display};

pub struct TruthTable<'s> {
    variables: HashSet<&'s str>,
    table: HashMap<Vec<bool>, bool>,
}

impl<'s> TruthTable<'s> {
    pub fn create<F: Fn(&HashMap<&'s str, bool>) -> bool>(
        variables: HashSet<&'s str>,
        pred: F,
    ) -> Self {
        let (envs, tab) = make_env(&variables);
        let mut table = HashMap::new();
        if variables.len() > 0 {
            for (i, env) in envs.iter().enumerate() {
                table.insert(tab[i].clone(), pred(&env));
            }
        } else {
            table.insert(vec![], pred(&HashMap::new()));
        }
        Self { variables, table }
    }
}

impl<'s> From<Node<'s>> for TruthTable<'s> {
    fn from(ast: Node<'s>) -> Self {
        let mut vars = HashSet::new();
        get_vars(&ast, &mut vars);
        Self::create(vars, |env| evaluate(&ast, env))
    }
}

impl<'s> Display for TruthTable<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for var in self.variables.iter() {
            write!(f, "{}\t", var)?;
        }
        writeln!(f, "({} entrées)", self.variables.len())?;
        for (tab, v) in self.table.iter() {
            for t in tab.iter() {
                write!(f, "{}\t", if *t { "V" } else { "F" })?;
            }
            writeln!(f, "{}", if *v { "V" } else { "F" })?;
        }
        Ok(())
    }
}

pub fn eval<'s>(input: String) -> Result<String, String> {
    let mut parser = Parser::new(&input);
    match parser.parse() {
        Ok(node) => Ok(format!("{}", TruthTable::from(node))),
        Err(err) => Err(format!("Couldn't parse input {:?}", err)),
    }
}

pub fn pprint<'s>(ast: &Node<'s>) -> String {
    match ast {
        Node::ValueNode(v) => format!("{}", *v as u8),
        Node::IdentNode(i) => format!("{}", i),
        Node::ExprNode(expr) => format!("({})", pprint(expr.as_ref())),
        Node::BinOpNode(op, left, right) => format!(
            "{} {} {}",
            pprint(left.as_ref()),
            op,
            pprint(right.as_ref())
        ),
        Node::UnopNode(op, right) => format!("{} {}", op, pprint(right.as_ref())),
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
