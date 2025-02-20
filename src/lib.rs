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
    pub fn parse(&self, logger: &mut Logger) -> Result<Ast, Error> {
        let parser = Parser::new();
        let (block, span) = parser.parse(&self.source, logger)?;
        Ok(Ast {
            source: &self.source,
            block: (block, span),
        })
    }
}

/*******
 * AST *
 *******/

/// Successfully parsed Tardigrade source code. Parameterized over the lifetime of its source code,
/// which may be referenced in error messages.
pub struct Ast<'s> {
    source: &'s Source,
    block: (Block, Span),
}

impl<'s> Ast<'s> {
    pub fn type_check(
        mut self,
        logger: &mut Logger,
    ) -> Result<(Type, TypeCheckedAst<'s>), Error<'s>> {
        let mut type_checker = TypeChecker::new(self.source, logger);
        let ty = type_checker.check_prog(&mut self.block)?;
        let (ct_funcs, rt_funcs) = type_checker.finish();
        let ast = TypeCheckedAst {
            source: self.source,
            block: self.block,
            ct_funcs,
            rt_funcs,
        };
        log!(logger, Trace, "type", ("{}", ty));
        Ok((ty, ast))
    }
}

impl Format for Ast<'_> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    fn format(&self, f: &mut impl fmt::Write, indent: u32, prec: Prec) -> fmt::Result {
        self.block.0.format(f, indent, prec)
    }
}

/********************
 * Type Checked AST *
 ********************/

pub struct TypeCheckedAst<'s> {
    source: &'s Source,
    block: (Block, Span),
    ct_funcs: Vec<FuncStmt>,
    rt_funcs: Vec<FuncStmt>,
}

impl<'s> TypeCheckedAst<'s> {
    pub fn compile(&'s self, logger: &mut Logger) -> Result<CompiledAst<'s>, Error<'s>> {
        let mut interpreter = Interpreter::new(self.source, &self.ct_funcs, logger);
        let (block, funcs) = interpreter.compile_prog(&self.block, &self.rt_funcs)?;
        Ok(CompiledAst {
            source: self.source,
            block,
            funcs,
        })
    }
}

impl Format for TypeCheckedAst<'_> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    fn format(&self, f: &mut impl fmt::Write, indent: u32, prec: Prec) -> fmt::Result {
        self.block.0.format(f, indent, prec)
    }
}

/****************
 * Compiled AST *
 ****************/

pub struct CompiledAst<'s> {
    source: &'s Source,
    block: (Block, Span),
    funcs: Vec<FuncStmt>,
}

impl<'s> CompiledAst<'s> {
    pub fn evaluate(&'s self, logger: &mut Logger) -> Result<Value, Error<'s>> {
        let mut interpreter = Interpreter::new(self.source, &self.funcs, logger);
        let value = interpreter.eval_prog(&self.block)?;
        interpreter.finish();
        Ok(value)
    }
}

impl Format for CompiledAst<'_> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    fn format(&self, f: &mut impl fmt::Write, indent: u32, prec: Prec) -> fmt::Result {
        self.block.0.format(f, indent, prec)
    }
}
