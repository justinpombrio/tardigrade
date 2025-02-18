use std::io;
use std::path::PathBuf;
use tardigrade::{log, Logger, Tardigrade, Verbosity};

/// Tardigrade: an experimental programming language
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct CommandLineArgs {
    /// The source file to run. If not provided, start a REPL.
    path: Option<PathBuf>,
    /// Whether to format the source file instead of running it.
    #[arg(short, long)]
    format: bool,
    /// Whether to type check the source file instead of running it.
    #[arg(short, long)]
    type_check: bool,
    /// Whether to compile and print the source file instead of running it.
    #[arg(short, long)]
    compile: bool,
    /// Whether to be extra verbose when logging. Takes priority over --quiet.
    #[arg(short, long)]
    verbose: bool,
    /// Whether to be extra quiet when logging.
    #[arg(short, long)]
    quiet: bool,
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

fn fmt(tardigrade: &Tardigrade, logger: &mut Logger) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(ast) => log!(logger, Required, &ast),
    }
}

fn type_check(tardigrade: &Tardigrade, logger: &mut Logger) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(ast) => match ast.type_check(logger) {
            Err(type_err) => println!("{}", type_err),
            Ok((ty, _)) => println!("{}", ty),
        },
    }
}

fn compile(tardigrade: &Tardigrade, logger: &mut Logger) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(ast) => match ast.type_check(logger) {
            Err(type_err) => println!("{}", type_err),
            Ok((_, ast)) => match ast.compile(logger) {
                Err(compilation_err) => println!("{}", compilation_err),
                Ok(ast) => log!(logger, Required, &ast),
            },
        },
    }
}

fn run(tardigrade: &Tardigrade, logger: &mut Logger) {
    match tardigrade.parse() {
        Err(parse_err) => println!("{}", parse_err),
        Ok(ast) => match ast.type_check(logger) {
            Err(type_err) => println!("{}", type_err),
            Ok((_, ast)) => match ast.compile(logger) {
                Err(compilation_err) => println!("{}", compilation_err),
                Ok(ast) => match ast.evaluate(logger) {
                    Err(runtime_err) => println!("{}", runtime_err),
                    Ok(value) => println!("{}", value),
                },
            },
        },
    }
}

fn repl(logger: &mut Logger) {
    let mut input_buffer = String::new();
    loop {
        let source = prompt(&mut input_buffer).unwrap();
        if source.is_empty() {
            break;
        }
        let tardigrade = Tardigrade::new("[stdin]", source.to_owned());
        run(&tardigrade, logger);
    }
    println!("Goodbye!");
}

fn main() {
    let args = <CommandLineArgs as clap::Parser>::parse();
    let verbosity = if args.verbose {
        Verbosity::Trace
    } else if args.quiet {
        Verbosity::Required
    } else {
        Verbosity::Info
    };
    let mut logger = Logger::new(verbosity);

    if let Some(path) = args.path {
        let tardigrade = Tardigrade::open(path).unwrap();
        if args.format {
            fmt(&tardigrade, &mut logger);
        } else if args.type_check {
            type_check(&tardigrade, &mut logger);
        } else if args.compile {
            compile(&tardigrade, &mut logger);
        } else {
            run(&tardigrade, &mut logger);
        }
    } else {
        repl(&mut logger);
    }
}
