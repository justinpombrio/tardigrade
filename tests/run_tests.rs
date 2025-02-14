use std::fs;
use std::io;
use std::mem;
use tardigrade::Tardigrade;

const TEST_DIR: &str = "tests/";
const TEST_EXT: &str = "trd";
const INDENT: &str = "    ";

/// A single test case (TEST/EXPECT/END in the test files).
struct TestCase {
    filename: String,
    name: String,
    source: String,
    operation: Operation,
    expected_output: String,
}

/// What to do with the test case's source code.
#[derive(Debug, Clone, Copy)]
enum Operation {
    Format,
    Run,
}

#[test]
fn run_tests() {
    let test_files = read_test_files().unwrap();
    let mut test_cases = Vec::new();
    for (filename, contents) in test_files {
        test_cases.append(&mut parse_test_cases(filename, contents));
    }

    for test in test_cases {
        eprintln!("TEST {} in {} ...", test.name, test.filename);
        let tardigrade = Tardigrade::new(&test.name, test.source.clone());
        let actual_output = match test.operation {
            Operation::Format => indent(fmt(&tardigrade)),
            Operation::Run => indent(run(&tardigrade)),
        };
        if test.expected_output == actual_output {
            eprintln!("  ok");
        } else {
            eprintln!("In {}:", test.filename);
            eprintln!("TEST {}", test.name);
            eprintln!("{}", test.source);
            match test.operation {
                Operation::Format => eprintln!("EXPECT format"),
                Operation::Run => eprintln!("EXPECT"),
            }
            eprintln!("{}", test.expected_output);
            eprintln!("ACTUAL");
            eprintln!("{}", actual_output);
            eprintln!("END");
            panic!("Test case failed.");
        }
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
fn parse_test_cases(filename: String, input: String) -> Vec<TestCase> {
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
                    operation = match line {
                        "EXPECT format" => Operation::Format,
                        "EXPECT" => Operation::Run,
                        _ => panic!(
                            "Test cases: expected 'EXPECT' or 'EXPECT format', found '{}'",
                            line
                        ),
                    };
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
                if line.starts_with("END") {
                    test_cases.push(TestCase {
                        filename: filename.clone(),
                        name: mem::take(&mut test_name),
                        source: mem::take(&mut source),
                        operation,
                        expected_output: mem::take(&mut output),
                    });
                    parse_state = Initial;
                } else if line.starts_with("TEST") || line.starts_with("EXPECT") {
                    panic!("Test cases: expected 'END', found '{}'", line);
                } else {
                    if !output.is_empty() {
                        output += "\n";
                    }
                    output += line;
                }
            }
        }
    }

    if parse_state != Initial {
        panic!("Test cases: missing final 'END'");
    }

    test_cases
}

/// Prefix all lines of `text` with `INDENT`.
fn indent(text: String) -> String {
    let mut output = String::new();
    for (i, line) in text.lines().enumerate() {
        if i != 0 {
            output.push('\n');
        }
        output.push_str(INDENT);
        output.push_str(line);
    }
    output
}

/// Run Tardigrade source code, and print the return value if it ran successfully, or the error
/// message if it didn't.
fn run(tardigrade: &Tardigrade) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err.display_with_color_override(false)),
        Ok(ast) => match ast.type_check() {
            Err(type_err) => format!("{}", type_err.display_with_color_override(false)),
            Ok((_, ast)) => match ast.interpret() {
                Err(runtime_err) => {
                    format!("{}", runtime_err.display_with_color_override(false))
                }
                Ok(value) => format!("{}", value),
            },
        },
    }
}

/// Format Tardigrade source code
fn fmt(tardigrade: &Tardigrade) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err.display_with_color_override(false)),
        Ok(ast) => ast.format(),
    }
}
