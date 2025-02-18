use std::fs;
use std::io;
use std::mem;
use tardigrade::{log, span, Format, Logger, Prec, Tardigrade, Verbosity};

const VERBOSITY: Verbosity = Verbosity::Info;

const TEST_DIR: &str = "tests/";
const TEST_EXT: &str = "trd";
const INDENT: &str = "    ";

/// The test cases in one file.
struct TestCaseGroup {
    filename: String,
    test_cases: Vec<TestCase>,
}

/// A single test case (TEST/EXPECT/END in the test files).
struct TestCase {
    name: String,
    source: String,
    expectations: Vec<(Operation, String)>,
}

/// What to do with the test case's source code.
#[derive(Debug, Clone, Copy)]
enum Operation {
    Format,
    TypeCheck,
    Compile,
    Run,
}

#[test]
fn run_tests() {
    let mut logger = Logger::new(VERBOSITY);

    let test_files = read_test_files().unwrap();
    let mut test_case_groups = Vec::new();
    for (filename, contents) in test_files {
        test_case_groups.push(parse_test_cases(filename, contents));
    }

    for group in test_case_groups {
        span!(logger, Info, "file", ("{}", group.filename), {
            for test in group.test_cases {
                span!(logger, Info, "test", ("{}", test.name), {
                    for (operation, expected_output) in &test.expectations {
                        run_test_expectation(
                            &group.filename,
                            &test,
                            *operation,
                            expected_output,
                            &mut logger,
                        );
                    }
                })
            }
        })
    }
}

fn run_test_expectation(
    filename: &str,
    test: &TestCase,
    operation: Operation,
    expected_output: &str,
    logger: &mut Logger,
) {
    let tardigrade = Tardigrade::new(&test.name, test.source.clone());
    let mut actual_output = match operation {
        Operation::Format => fmt(&tardigrade),
        Operation::TypeCheck => type_check(&tardigrade, logger),
        Operation::Compile => compile(&tardigrade, logger),
        Operation::Run => run(&tardigrade, logger),
    };
    if actual_output.ends_with("\n") {
        actual_output.pop();
    }
    if expected_output == actual_output {
        log!(logger, Info, "pass");
    } else {
        log!(logger, Required, "TEST", ("{} in {}", test.name, filename));
        logger.start_span();
        log!(logger, Required, &test.source);
        logger.end_span();
        match operation {
            Operation::Format => log!(logger, Required, "EXPECT format"),
            Operation::TypeCheck => log!(logger, Required, "EXPECT type"),
            Operation::Compile => log!(logger, Required, "EXPECT compile"),
            Operation::Run => log!(logger, Required, "EXPECT"),
        }
        logger.start_span();
        log!(logger, Required, &expected_output);
        logger.end_span();
        log!(logger, Required, "ACTUAL");
        logger.start_span();
        log!(logger, Required, &actual_output);
        logger.end_span();
        log!(logger, Required, "END");

        panic!("Test case failed.");
    }
}

/// Read all test case files (files in `TEST_DIR` that end in `TEST_EXT`).
/// Produces a list of `(filename, file_contents)`.
fn read_test_files() -> io::Result<Vec<(String, String)>> {
    let mut tests = Vec::new();
    for entry in fs::read_dir(TEST_DIR)? {
        let entry = entry?;
        if entry
            .path()
            .extension()
            .map(|ext| ext.to_str() == Some(TEST_EXT))
            .unwrap_or(false)
        {
            let filename = entry.path().to_string_lossy().to_string();
            let file_contents = fs::read_to_string(entry.path())?;
            tests.push((filename, file_contents));
        }
    }
    Ok(tests)
}

/// Parse the test cases from a single file. Panic on errors.
fn parse_test_cases(filename: String, input: String) -> TestCaseGroup {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum ParseState {
        Initial,
        ReadingSource,
        ReadingOutput,
    }

    use ParseState::*;

    let mut parse_state = ParseState::Initial;
    let mut test_name = String::new();
    let mut source = String::new();
    let mut operation = Operation::Format;
    let mut output = String::new();
    let mut expectations = Vec::new();
    let mut test_cases = Vec::new();
    for line in input.lines() {
        if line.is_empty() || line.starts_with("//") {
            continue;
        }
        match parse_state {
            Initial => {
                if let Some(stripped) = line.strip_prefix("TEST") {
                    test_name = stripped.trim().to_owned();
                    parse_state = ReadingSource;
                } else {
                    panic!("Test cases: expected 'TEST', found '{}'", line);
                }
            }
            ReadingSource => {
                if line.starts_with("EXPECT") {
                    operation = parse_operation(line);
                    parse_state = ReadingOutput;
                } else if line.starts_with("TEST") || line.starts_with("END") {
                    panic!("Test cases: expected 'EXPECT', found '{}'", line);
                } else {
                    if !source.is_empty() {
                        source += "\n";
                    }
                    if let Some(stripped_line) = line.strip_prefix(INDENT) {
                        source += stripped_line;
                    } else {
                        panic!("Expected test case source line to be indented: '{}'", line);
                    }
                }
            }
            ReadingOutput => {
                if line.starts_with("EXPECT") {
                    expectations.push((operation, mem::take(&mut output)));
                    operation = parse_operation(line);
                } else if line.starts_with("END") {
                    expectations.push((operation, mem::take(&mut output)));
                    test_cases.push(TestCase {
                        name: mem::take(&mut test_name),
                        source: mem::take(&mut source),
                        expectations: mem::take(&mut expectations),
                    });
                    parse_state = Initial;
                } else if line.starts_with("TEST") {
                    panic!("Test cases: expected 'END', found '{}'", line);
                } else {
                    if !output.is_empty() {
                        output += "\n";
                    }
                    output += line.strip_prefix(INDENT).expect("indentation required");
                }
            }
        }
    }

    if parse_state != Initial {
        panic!("Test cases: missing final 'END'");
    }

    TestCaseGroup {
        filename,
        test_cases,
    }
}

fn parse_operation(line: &str) -> Operation {
    match line {
        "EXPECT format" => Operation::Format,
        "EXPECT type" => Operation::TypeCheck,
        "EXPECT compile" => Operation::Compile,
        "EXPECT" | "EXPECT run" => Operation::Run,
        _ => panic!(
            "Test cases: expected 'EXPECT' or 'EXPECT format', found '{}'",
            line
        ),
    }
}

/// Format Tardigrade source code
fn fmt(tardigrade: &Tardigrade) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err.display_with_color_override(false)),
        Ok(ast) => {
            let mut buffer = String::new();
            ast.format(&mut buffer, 0, Prec::MAX).unwrap();
            buffer
        }
    }
}

fn type_check(tardigrade: &Tardigrade, logger: &mut Logger) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err.display_with_color_override(false)),
        Ok(ast) => match ast.type_check(logger) {
            Err(type_err) => format!("{}", type_err.display_with_color_override(false)),
            Ok((ty, _)) => ty.to_string(),
        },
    }
}

fn compile(tardigrade: &Tardigrade, logger: &mut Logger) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err.display_with_color_override(false)),
        Ok(ast) => match ast.type_check(logger) {
            Err(type_err) => format!("{}", type_err.display_with_color_override(false)),
            Ok((_, ast)) => match ast.compile(logger) {
                Err(compilation_err) => {
                    format!("{}", compilation_err.display_with_color_override(false))
                }
                Ok(ast) => {
                    let mut buffer = String::new();
                    ast.format(&mut buffer, 0, Prec::MAX).unwrap();
                    buffer
                }
            },
        },
    }
}

/// Run Tardigrade source code, and print the return value if it ran successfully, or the error
/// message if it didn't.
fn run(tardigrade: &Tardigrade, logger: &mut Logger) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err.display_with_color_override(false)),
        Ok(ast) => match ast.type_check(logger) {
            Err(type_err) => format!("{}", type_err.display_with_color_override(false)),
            Ok((_, ast)) => match ast.compile(logger) {
                Err(compilation_err) => {
                    format!("{}", compilation_err.display_with_color_override(false))
                }
                Ok(ast) => match ast.evaluate(logger) {
                    Err(runtime_err) => {
                        format!("{}", runtime_err.display_with_color_override(false))
                    }
                    Ok(value) => format!("{}", value),
                },
            },
        },
    }
}
