use std::fs;
use std::io;
use std::mem;
use tardigrade::{Logger, log};

pub trait RunTests {
    type Operation: Default + Clone;
    fn parse_op(s: &str) -> Option<Self::Operation>;
    fn show_op(op: &Self::Operation) -> &str;
    fn run_test(&mut self, test_name: &str, source: &str, logger: &mut Logger) -> String;
}

/// The test cases in one file.
struct TestCaseGroup<R: RunTests> {
    filename: String,
    test_cases: Vec<TestCase<R>>,
}

/// A single test case.
struct TestCase<R: RunTests> {
    name: String,
    source: String,
    expectations: Vec<(R::Operation, String)>,
}

pub fn run_tests<R: RunTests>(
    runner: &mut R,
    test_dir: &str,
    test_ext: &str,
    indent: &str,
    logger: &mut Logger,
) {
    let test_files = read_test_files(test_dir, test_ext).unwrap();
    let test_case_groups = test_files
        .into_iter()
        .map(|(filename, contents)| parse_test_cases::<R>(filename, contents, indent))
        .collect::<Vec<_>>();

    for group in test_case_groups {
        log!(logger, Info, "file", ("{}", group.filename), {
            for test in group.test_cases {
                log!(logger, Info, "test", ("{}", test.name), {
                    for (operation, expected_output) in &test.expectations {
                        run_test(
                            runner,
                            &group.filename,
                            &test,
                            operation.clone(),
                            expected_output,
                            logger,
                        );
                    }
                })
            }
        })
    }
}

fn run_test<R: RunTests>(
    runner: &mut R,
    filename: &str,
    test: &TestCase<R>,
    op: R::Operation,
    expected_output: &str,
    logger: &mut Logger,
) {
    let mut actual_output = runner.run_test(&test.name, &test.source, logger);
    if actual_output.ends_with("\n") {
        actual_output.pop();
    }
    if expected_output == actual_output {
        log!(logger, Info, "pass");
    } else {
        log!(logger, Notice, "TEST", ("{} in {}", test.name, filename));
        logger.indent();
        log!(logger, Notice, &test.source as &str);
        logger.dedent();
        log!(logger, Notice, "EXPECT", ("{}", R::show_op(&op)));
        logger.indent();
        log!(logger, Notice, expected_output);
        logger.dedent();
        log!(logger, Notice, "ACTUAL");
        logger.indent();
        log!(logger, Notice, &actual_output);
        logger.dedent();
        log!(logger, Notice, "END");

        panic!("Test case failed.");
    }
}

/// Read all files in `test_dir` that end in `test_ext`.
/// Produces a list of `(filename, file_contents)`.
fn read_test_files(test_dir: &str, test_ext: &str) -> io::Result<Vec<(String, String)>> {
    let mut tests = Vec::new();
    for entry in fs::read_dir(test_dir)? {
        let entry = entry?;
        if entry
            .path()
            .extension()
            .map(|ext| ext.to_str() == Some(test_ext))
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
fn parse_test_cases<R: RunTests>(
    filename: String,
    input: String,
    indent: &str,
) -> TestCaseGroup<R> {
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
    let mut operation = R::Operation::default();
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
                if let Some(op_str) = line.strip_prefix("EXPECT") {
                    operation = parse_operation::<R>(op_str);
                    parse_state = ReadingOutput;
                } else if line.starts_with("TEST") || line.starts_with("END") {
                    panic!("Test cases: expected 'EXPECT', found '{}'", line);
                } else {
                    if !source.is_empty() {
                        source += "\n";
                    }
                    source += parse_line(line, indent);
                }
            }
            ReadingOutput => {
                if let Some(op_str) = line.strip_prefix("EXPECT") {
                    operation = parse_operation::<R>(op_str);
                    expectations.push((operation.clone(), mem::take(&mut output)));
                } else if line.starts_with("END") {
                    expectations.push((operation.clone(), mem::take(&mut output)));
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
                    output += parse_line(line, indent);
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

fn parse_line<'a>(line: &'a str, indent: &str) -> &'a str {
    line.strip_prefix(indent).unwrap_or_else(|| {
        panic!("Expected line to be indented: '{}'", line);
    })
}

fn parse_operation<R: RunTests>(op_str: &str) -> R::Operation {
    match R::parse_op(op_str.trim()) {
        Some(op) => op,
        None => panic!("Test cases: unknown operation '{}' after 'EXPECT'", op_str),
    }
}
