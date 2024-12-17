use std::io;
use std::path::PathBuf;
use tardigrade::Tardigrade;

/// Tardigrade: an experimental programming language
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct CommandLineArgs {
    /// The source file to run. If not provided, start a REPL.
    path: Option<PathBuf>,
    /// Whether to format the source file instead of running it.
    #[arg(short, long)]
    format: bool,
}

fn prompt(buffer: &mut String) -> Result<&str, io::Error> {
    use std::io::Write;

    // Write prompt
    print!("> ");
    io::stdout().flush()?;

    // Read line
    buffer.clear();
    io::stdin().read_line(buffer)?;
    Ok(buffer.trim())
}

fn run(tardigrade: &Tardigrade) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(ast) => match ast.type_check() {
            Err(type_err) => println!("{}", type_err),
            Ok(_) => match ast.interpret() {
                Err(runtime_err) => println!("{}", runtime_err),
                Ok(value) => println!("{}", value),
            },
        },
    }
}

fn fmt(tardigrade: &Tardigrade) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(ast) => println!("{}", ast.format()),
    }
}

fn repl() {
    let mut input_buffer = String::new();
    loop {
        let source = prompt(&mut input_buffer).unwrap();
        if source.is_empty() {
            break;
        }
        let tardigrade = Tardigrade::new("[stdin]", source.to_owned());
        run(&tardigrade);
    }
    println!("Goodbye!");
}

fn main() {
    let args = <CommandLineArgs as clap::Parser>::parse();

    if let Some(path) = args.path {
        let tardigrade = Tardigrade::open(path).unwrap();
        if args.format {
            fmt(&tardigrade);
        } else {
            run(&tardigrade);
        }
    } else {
        repl();
    }
}
