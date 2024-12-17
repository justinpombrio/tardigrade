mod ast;
mod error;
mod format;
mod interpreter;
mod parser;
mod type_checker;

use ast::{Expr, Span, Value};
use error::Error;
use interpreter::Interpreter;
use panfix::Source;
use parser::Parser;
use std::io;
use std::path::Path;
use type_checker::{Type, TypeChecker};

/// Tardigrade source code.
pub struct Tardigrade(Source);

impl Tardigrade {
    /// Construct directly from the source text. `filename` is used solely for error messages.
    pub fn new(filename: &str, source: String) -> Tardigrade {
        Tardigrade(Source::new(filename, source))
    }

    /// Read Tardigrade source from stdin.
    pub fn from_stdin() -> Result<Tardigrade, io::Error> {
        Ok(Tardigrade(Source::from_stdin()?))
    }

    /// Read Tardigrade source from the given file.
    pub fn open(filepath: impl AsRef<Path>) -> Result<Tardigrade, io::Error> {
        Ok(Tardigrade(Source::open(filepath)?))
    }

    /// Attempt to parse this source code.
    pub fn parse(&self) -> Result<Ast, Error> {
        let parser = Parser::new();
        let (expr, span) = parser.parse(&self.0)?;
        Ok(Ast {
            source: &self.0,
            expr,
            span,
        })
    }
}

/// Successfully parsed Tardigrade source code. Parameterized over the lifetime of its source code,
/// which may be referenced in error messages.
pub struct Ast<'s> {
    source: &'s Source,
    expr: Expr,
    span: Span,
}

impl<'s> Ast<'s> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    pub fn format(&self) -> String {
        format!("{}", self.expr)
    }

    pub fn type_check(&self) -> Result<Type, Error<'s>> {
        let type_checker = TypeChecker::new(self.source);
        type_checker.check_expr(&self.expr)
    }

    pub fn interpret(&self) -> Result<Value, Error<'s>> {
        let interpreter = Interpreter::new(self.source);
        interpreter.interp_expr(&self.expr, self.span)
    }
}
