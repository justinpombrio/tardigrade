mod ast;
mod error;
mod format;
mod grammar;
mod interpreter;
mod logger;
mod parser;
mod stack;
mod type_checker;

use ast::{Block, FuncStmt, Span, Value};
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
        let (block, span) = parser.parse(&self.0)?;
        Ok(Ast {
            source: &self.0,
            block: (block, span),
        })
    }
}

/// Successfully parsed Tardigrade source code. Parameterized over the lifetime of its source code,
/// which may be referenced in error messages.
pub struct Ast<'s> {
    source: &'s Source,
    block: (Block, Span),
}

pub struct TypeCheckedAst<'s> {
    source: &'s Source,
    block: (Block, Span),
    functions: Vec<FuncStmt>,
}

impl<'s> Ast<'s> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    pub fn format(&self) -> String {
        format!("{}", &self.block.0)
    }

    pub fn type_check(mut self) -> Result<(Type, TypeCheckedAst<'s>), Error<'s>> {
        let mut type_checker = TypeChecker::new(self.source);
        let ty = type_checker.check_prog(&mut self.block)?;
        let functions = type_checker.finish();
        let ast = TypeCheckedAst {
            source: self.source,
            block: self.block,
            functions,
        };
        Ok((ty, ast))
    }
}

impl<'s> TypeCheckedAst<'s> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    pub fn format(&self) -> String {
        format!("{}", &self.block.0)
    }

    pub fn interpret(&'s self) -> Result<Value<'s>, Error<'s>> {
        let mut interpreter = Interpreter::new(self.source, &self.functions);
        let value = interpreter.interp_prog(&self.block)?;
        interpreter.finish();
        Ok(value)
    }
}
