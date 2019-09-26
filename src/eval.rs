use crate::parse::{Node, Parser};
use crate::solve::{distribute_or, extract_clauses, remove_implications, remove_negations, Clause};
use crate::util::cartesian_product;
use atty::Stream;
use colored::*;
use lazy_static::lazy_static;
use regex::Regex;
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
                write!(f, "{}\t", print_bool(*t))?;
            }
            writeln!(f, "{}", print_bool(*v))?;
        }
        Ok(())
    }
}

pub fn eval<'s>(input: String) -> Result<String, String> {
    let input = input.trim();
    if input.starts_with(":") {
        lazy_static! {
            static ref input_re: Regex = Regex::new(r"^:([a-z]+) (.*)$").unwrap();
        }
        if !input_re.is_match(input) {
            Err("Malformed command input".to_string())
        } else {
            let matches = input_re.captures(input).ok_or("Malformed command input")?;
            match Parser::new(
                matches
                    .get(2)
                    .ok_or("Couldn't expression from input")?
                    .as_str(),
            )
            .parse()
            {
                Ok(ast) => match matches
                    .get(1)
                    .ok_or("Couldn't get command from input")?
                    .as_str()
                {
                    "ast" => Ok(format!("{:?}", ast)),
                    "table" => Ok(format!("{}", TruthTable::from(ast))),
                    "cnf" => {
                        let ast = distribute_or(Box::new(ast)).ok_or("Couldn't distribute or")?;
                        let ast = remove_implications(Box::new(ast));
                        let ast = remove_negations(Box::new(ast.clone()))
                            .ok_or(format!("Couldn't remove negations: {}", ast.clone()))?;
                        Ok(format!("{}", ast))
                    }
                    "clauses" => {
                        let ast = distribute_or(Box::new(ast)).ok_or("Couldn't distribute or")?;
                        let ast = remove_implications(Box::new(ast));
                        let ast = remove_negations(Box::new(ast.clone()))
                            .ok_or(format!("Couldn't remove negations: {}", ast.clone()))?;
                        let clauses = extract_clauses(Box::new(ast.clone()))
                            .ok_or(format!("Couldn't extract clauses: {}", ast))?;
                        Ok(format!(
                            "[{}]",
                            clauses
                                .iter()
                                .map(print_clause)
                                .collect::<Vec<String>>()
                                .join(", ")
                        ))
                    }
                    x => Err(format!("Command not found: {}", x)),
                },
                Err(err) => Err(format!("Error parsing input {:?}", err)),
            }
        }
    } else {
        match Parser::new(input).parse() {
            Ok(ast) => Ok(format!("{}", ast)),
            Err(err) => Err(format!("Error parsing input {:?}", err)),
        }
    }
}

pub fn pprint<'s>(ast: &Node<'s>) -> String {
    match ast {
        Node::ValueNode(v) => format!("{}", *v as u8),
        Node::IdentNode(i) => format!("{}", i),
        Node::ExprNode(expr) => format!("({})", pprint(expr.as_ref())),
        Node::BinOpNode(op, left, right) => format!(
            "({} {} {})",
            pprint(left.as_ref()),
            op,
            pprint(right.as_ref())
        ),
        Node::UnopNode(op, right) => format!("{} {}", op, pprint(right.as_ref())),
    }
}

fn print_clause<'s>(clause: &Clause<'s>) -> String {
    format!(
        "[{}]",
        clause
            .iter()
            .map(|l| format!("{}", l))
            .collect::<Vec<String>>()
            .join(",")
    )
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

fn print_bool(v: bool) -> String {
    if atty::is(Stream::Stdout) {
        format!("{}", if v { "V".clear() } else { "F".dimmed() })
    } else {
        format!("{}", if v { "V" } else { "F" })
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
