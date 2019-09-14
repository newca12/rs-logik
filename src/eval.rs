use crate::parse::{Node, Parser};
use crate::util::cartesian_product;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write};

pub fn eval<'s>(input: String) -> Result<String, String> {
    let mut parser = Parser::new(&input);
    match parser.parse() {
        Ok(node) => evaluate_ast(&node).map_err(|_| String::from("Couldn't evaluate AST")),
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

pub fn evaluate_ast<'s>(ast: &Node<'s>) -> Result<String, fmt::Error> {
    let mut s = String::new();
    let mut vars = HashSet::new();
    get_vars(ast, &mut vars);
    let (envs, tab) = make_env(&vars);
    if vars.len() > 0 {
        writeln!(&mut s, "Table de vérité:\n")?;
        for var in vars.iter() {
            write!(&mut s, "{}\t", var)?;
        }
        writeln!(&mut s, "({} entrées)", vars.len())?;
        for (i, env) in envs.iter().enumerate() {
            for t in tab[i].iter() {
                write!(&mut s, "{}\t", *t as u8)?;
            }
            writeln!(&mut s, "{}", evaluate(ast, env) as u8)?;
        }
    } else {
        writeln!(&mut s, "Valeur: {}", evaluate(ast, &HashMap::new()) as u8)?;
    }

    Ok(s.clone())
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
