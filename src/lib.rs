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
use std::fmt;
use std::io;
use std::path::Path;
use type_checker::{Type, TypeChecker};

pub use ast::Prec;
pub use format::Format;
pub use logger::{Logger, Verbosity};

/// Tardigrade source code.
pub struct Tardigrade {
    source: Source,
}

impl Tardigrade {
    /// Construct directly from the source text. `filename` is used solely for error messages.
    pub fn new(filename: &str, source: String) -> Tardigrade {
        Tardigrade::from_source(Source::new(filename, source))
    }

    /// Read Tardigrade source from stdin.
    pub fn from_stdin() -> Result<Tardigrade, io::Error> {
        Ok(Tardigrade::from_source(Source::from_stdin()?))
    }

    /// Read Tardigrade source from the given file.
    pub fn open(filepath: impl AsRef<Path>) -> Result<Tardigrade, io::Error> {
        Ok(Tardigrade::from_source(Source::open(filepath)?))
    }

    fn from_source(source: Source) -> Tardigrade {
        Tardigrade { source }
    }

    /// Attempt to parse this source code.
    pub fn parse(&self) -> Result<Ast, Error> {
        let parser = Parser::new();
        let (block, span) = parser.parse(&self.source)?;
        Ok(Ast {
            source: &self.source,
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
    pub fn type_check(
        mut self,
        logger: &mut Logger,
    ) -> Result<(Type, TypeCheckedAst<'s>), Error<'s>> {
        span!(logger, Trace, "typecheck", {
            span!(logger, Trace, "program", {
                log!(logger, Trace, self.block.0);
            });
            let mut type_checker = TypeChecker::new(self.source, logger);
            let ty = type_checker.check_prog(&mut self.block)?;
            let functions = type_checker.finish();
            let ast = TypeCheckedAst {
                source: self.source,
                block: self.block,
                functions,
            };
            log!(logger, Trace, "type", ("{}", ty));
            Ok((ty, ast))
        })
    }
}

impl Format for Ast<'_> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    fn format(&self, f: &mut impl fmt::Write, indent: u32, prec: Prec) -> fmt::Result {
        self.block.0.format(f, indent, prec)
    }
}

impl<'s> TypeCheckedAst<'s> {
    pub fn interpret(&'s self, logger: &mut Logger) -> Result<Value, Error<'s>> {
        span!(logger, Trace, "interpret", {
            let mut interpreter = Interpreter::new(self.source, &self.functions, logger);
            let value = interpreter.interp_prog(&self.block)?;
            interpreter.finish();
            Ok(value)
        })
    }
}

impl Format for TypeCheckedAst<'_> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    fn format(&self, f: &mut impl fmt::Write, indent: u32, prec: Prec) -> fmt::Result {
        self.block.0.format(f, indent, prec)
    }
}
