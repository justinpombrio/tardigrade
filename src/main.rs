use std::io;
use std::path::PathBuf;
use tardigrade::Tardigrade;

/// Tardigrade: an experimental programming language
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct CommandLineArgs {
    /// The source file to run. If not provided, start a REPL.
    path: Option<PathBuf>,
    /// Mode: format the source file instead of running it.
    #[arg(short, long)]
    format: bool,
    /// Mode: scope check the source file instead of running it.
    #[arg(short, long)]
    scope_check: bool,
    /// Mode: type check the source file instead of running it.
    #[arg(short, long)]
    type_check: bool,
    /// Mode: compile the source file instead of running it.
    #[arg(short, long)]
    compile: bool,
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

fn format(tardigrade: &Tardigrade) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(ast) => println!("{}", ast.format()),
    }
}

fn scope_check(tardigrade: &Tardigrade) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(mut ast) => match ast.scope_check() {
            Err(err) => println!("{}", err),
            Ok(()) => println!("ok"),
        },
    }
}

fn type_check(tardigrade: &Tardigrade) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(mut ast) => match ast.type_check() {
            Err(err) => println!("{}", err),
            Ok(ty) => println!("{}", ty),
        },
    }
}

fn compile(tardigrade: &Tardigrade) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(mut ast) => match ast.compile() {
            Err(err) => println!("{}", err),
            Ok(compiled_ast) => println!("{}", compiled_ast.format()),
        },
    }
}

fn run(tardigrade: &Tardigrade) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(mut ast) => match ast.compile() {
            Err(err) => println!("{}", err),
            Ok(compiled_ast) => match compiled_ast.interpret() {
                Err(err) => println!("{}", err),
                Ok(value) => println!("{}", value),
            },
        },
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
        if args.compile {
            compile(&tardigrade);
        } else if args.type_check {
            type_check(&tardigrade);
        } else if args.scope_check {
            scope_check(&tardigrade);
        } else if args.format {
            format(&tardigrade);
        } else {
            run(&tardigrade);
        }
    } else {
        repl();
    }
}
