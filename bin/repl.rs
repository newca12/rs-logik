use logik::Parser;

use std::io::{self, Write};

fn main() -> io::Result<()> {
    loop {
        print!(">>> ");
        io::stdout().flush()?;
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                let mut parser = Parser::new(buffer.as_str());
                match parser.parse() {
                    Ok(node) => println!("{:?}", node),
                    Err(range) => println!("Error parsing input {:?}", range),
                }
            }
            Err(err) => println!("Error: {}", err),
        }
    }
}
