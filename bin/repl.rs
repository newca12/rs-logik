use clap::App;
use logik::eval;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let matches = App::new("Évaluateur Logik")
        .version("0.1")
        .author("Arthur & Nathan G.")
        .about("Évalue des propositions logiques")
        .arg_from_usage(
            "-e, --eval [EXPR] 'Évalue une expression logique, ou si '-' lis depuis stdin'",
        )
        .get_matches();

    if let Some(expr) = matches.value_of("eval") {
        if expr == "-" {
            loop {
                if let Err(_) = repl_once() {
                    return Ok(());
                }
            }
        } else {
            eval_one(expr.to_string())?;
            Ok(())
        }
    } else {
        loop {
            print!(">>> ");
            io::stdout().flush()?;
            repl_once()?
        }
    }
}

fn repl_once() -> io::Result<()> {
    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(size) => {
            if size == 0 {
                Err(io::Error::from(io::ErrorKind::BrokenPipe))
            } else {
                eval_one(buffer)
            }
        }
        Err(err) => Err(err),
    }
}

fn eval_one(buffer: String) -> io::Result<()> {
    match eval(buffer) {
        Ok(data) => writeln!(io::stdout(), "{}", data)?,
        Err(err) => writeln!(io::stderr(), "{}", err)?,
    };
    Ok(())
}
