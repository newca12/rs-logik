use logik::eval;

use std::io::{self, Write};

fn main() -> io::Result<()> {
    loop {
        print!(">>> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => match eval(buffer) {
                Ok(data) => writeln!(io::stdout(), "{}", data)?,
                Err(err) => writeln!(io::stderr(), "{}", err)?,
            },
            Err(err) => writeln!(io::stderr(), "Error reading stdin: {}", err)?,
        };
    }
}
