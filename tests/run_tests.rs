mod common;

use common::{RunTests, run_tests};
use tardigrade::{Logger, RenderOptions, Verbosity};

const VERBOSITY: Verbosity = Verbosity::Info;
const TEST_DIR: &str = "tests/suite/";
const TEST_EXT: &str = "trd";
const INDENT: &str = "  ";

struct TestRunner;

/// What to do with the test case's source code.
#[derive(Clone, Default)]
enum Operation {
    #[default]
    SayHello,
}

impl RunTests for TestRunner {
    type Operation = Operation;

    fn parse_op(s: &str) -> Option<Operation> {
        match s {
            "hello" => Some(Operation::SayHello),
            _ => None,
        }
    }

    fn show_op(op: &Operation) -> &str {
        match op {
            Operation::SayHello => "hello",
        }
    }

    fn run_test(&mut self, _test_name: &str, source: &str, _logger: &mut Logger) -> String {
        format!("Hello, {}", source.trim())
    }
}

#[test]
fn tests() {
    let mut runner = TestRunner;
    let mut logger = Logger::new(VERBOSITY, RenderOptions::default());
    run_tests(&mut runner, TEST_DIR, TEST_EXT, INDENT, &mut logger);
}
