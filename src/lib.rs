mod ast;
mod error;
mod evaluator;
mod format;
mod grammar;
mod parser;
mod scope_checker;
mod stack;
mod type_checker;

use ast::{Block, Span};
use evaluator::Evaluator;
use panfix::Source;
use parser::Parser;
use scope_checker::ScopeChecker;
use std::io;
use std::path::Path;
use type_checker::{Type, TypeChecker};

pub use ast::Value;
pub use error::Error;

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
            compiled: false,
        })
    }
}

/// Successfully parsed Tardigrade source code. Parameterized over the lifetime of its source code,
/// which may be referenced in error messages.
pub struct Ast<'s> {
    source: &'s Source,
    block: (Block, Span),
    compiled: bool,
}

impl<'s> Ast<'s> {
    /// Very naive pretty printing. Liable to put way too much on one line.
    pub fn format(&self) -> String {
        format!("{}", &self.block.0)
    }

    pub fn scope_check(&mut self) -> Result<(), Error<'s>> {
        let mut scope_checker = ScopeChecker::new(self.source);
        scope_checker.resolve_block(&mut self.block, false)?;
        scope_checker.finish();
        Ok(())
    }

    pub fn type_check(&mut self) -> Result<Type, Error<'s>> {
        self.scope_check()?;
        let mut type_checker = TypeChecker::new(self.source);
        let ty = type_checker.check_block(&self.block, false)?;
        type_checker.finish();
        Ok(ty)
    }

    pub fn compile(&'s mut self) -> Result<Ast<'s>, Error<'s>> {
        self.type_check()?;
        let mut evaluator = Evaluator::new(self.source);
        let compiled = evaluator.compile_block(&self.block)?;
        evaluator.finish();
        let compiled_ast = Ast {
            source: self.source,
            block: compiled,
            compiled: true,
        };
        {
            // Double check that type checking still works!
            let mut type_checker = TypeChecker::new(self.source);
            match type_checker.check_block(&compiled_ast.block, false) {
                Ok(_) => (),
                Err(err) => {
                    eprintln!("{}", err);
                    panic!("Type error in compiled code! It should have been caught before compilation.");
                }
            }
            type_checker.finish();
        }
        Ok(compiled_ast)
    }

    pub fn interpret(&'s self) -> Result<Value<'s>, Error<'s>> {
        if !self.compiled {
            panic!("Ast::interpret -- code needs to be compiled first");
        }
        let mut evaluator = Evaluator::new(self.source);
        let value = evaluator.interp_block(&self.block)?;
        evaluator.finish();
        Ok(value)
    }
}
