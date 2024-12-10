mod ast;
mod error;
mod format;
mod interpreter;
mod parser;
mod type_checker;

use ast::{Expr, Value, WithSpan};
use error::Error;
use interpreter::Interpreter;
use parser::{ParseError, Parser};
use std::io;
use std::path::Path;
use type_checker::{Type, TypeChecker};

pub use panfix::Source;

pub struct Tardigrade {
    source: Source,
    parser: Parser,
}

impl Tardigrade {
    pub fn new(filename: &str, source: String) -> Tardigrade {
        Tardigrade {
            source: Source::new(filename, source),
            parser: Parser::new().unwrap(),
        }
    }

    pub fn from_stdin() -> Result<Tardigrade, io::Error> {
        Ok(Tardigrade {
            source: Source::from_stdin()?,
            parser: Parser::new().unwrap(),
        })
    }

    pub fn open(filepath: impl AsRef<Path>) -> Result<Tardigrade, io::Error> {
        Ok(Tardigrade {
            source: Source::open(filepath)?,
            parser: Parser::new().unwrap(),
        })
    }

    pub fn parse(&self) -> Result<WithSpan<Expr>, ParseError> {
        self.parser.parse(&self.source)
    }

    pub fn format(&self, ast: &WithSpan<Expr>) -> String {
        format!("{}", ast)
    }

    pub fn type_check(&self, ast: &WithSpan<Expr>) -> Result<Type, Error> {
        let type_checker = TypeChecker::new(&self.source);
        type_checker.check_expr(ast)
    }

    pub fn interpret(&self, ast: &WithSpan<Expr>) -> Result<Value, Error> {
        let interpreter = Interpreter::new(&self.source);
        interpreter.interp_expr(ast)
    }
}
