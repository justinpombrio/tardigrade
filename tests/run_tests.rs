use std::fs;
use std::io;
use std::mem;
use tardigrade::Tardigrade;

const TEST_DIR: &str = "tests/";
const TEST_EXT: &str = "trd";

struct TestCase {
    name: String,
    source: String,
    operation: Operation,
    expected_output: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseState {
    Initial,
    ReadingSource,
    ReadingOutput,
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Format,
    Run,
}

fn test_files() -> io::Result<Vec<String>> {
    let mut tests = Vec::new();
    for entry in fs::read_dir(TEST_DIR)? {
        let entry = entry?;
        if entry
            .path()
            .extension()
            .map(|ext| ext.to_str() == Some(TEST_EXT))
            .unwrap_or(false)
        {
            tests.push(fs::read_to_string(entry.path())?);
        }
    }
    Ok(tests)
}

#[test]
fn run_tests() {
    use ParseState::*;

    let mut parse_state = ParseState::Initial;
    let mut test_name = String::new();
    let mut source = String::new();
    let mut operation = Operation::Format;
    let mut output = String::new();
    let mut test_cases = Vec::new();
    for input in test_files().unwrap() {
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
                        source += line;
                    }
                }
                ReadingOutput => {
                    if line.starts_with("END") {
                        test_cases.push(TestCase {
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
    }

    if parse_state != Initial {
        panic!("Test cases: missing final 'END'");
    }

    for test in test_cases {
        let tardigrade = Tardigrade::new(&test.name, test.source.clone());
        let actual_output = match test.operation {
            Operation::Format => indent(fmt(&tardigrade)),
            Operation::Run => indent(run(&tardigrade)),
        };
        if test.expected_output != actual_output {
            println!("TEST {}", test.name);
            println!("{}", test.source);
            match test.operation {
                Operation::Format => println!("EXPECT format"),
                Operation::Run => println!("EXPECT"),
            }
            println!("{}", test.expected_output);
            println!("ACTUAL");
            println!("{}", actual_output);
            println!("END");
            assert_eq!(test.expected_output, actual_output);
        }
    }
}

fn indent(text: String) -> String {
    let mut output = String::new();
    for line in text.lines() {
        output.push_str("    ");
        output.push_str(line);
    }
    output
}

fn run(tardigrade: &Tardigrade) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err),
        Ok(ast) => match tardigrade.type_check(&ast) {
            Err(type_err) => format!("{}", type_err),
            Ok(_) => match tardigrade.interpret(&ast) {
                Err(runtime_err) => format!("{}", runtime_err),
                Ok(value) => format!("{}", value),
            },
        },
    }
}

fn fmt(tardigrade: &Tardigrade) -> String {
    match tardigrade.parse() {
        Err(parse_err) => format!("{}", parse_err),
        Ok(ast) => format!("{}", tardigrade.format(&ast)),
    }
}
