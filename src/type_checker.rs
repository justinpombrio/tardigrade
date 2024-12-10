use crate::ast::{Expr, Span, WithSpan};
use crate::error::Error;
use panfix::Source;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Span,
}

pub struct TypeChecker<'s> {
    source: &'s Source,
}

impl<'s> TypeChecker<'s> {
    pub fn new(source: &'s Source) -> TypeChecker<'s> {
        TypeChecker { source }
    }

    pub fn check_expr(&self, expr: &WithSpan<Expr>) -> Result<Type, Error<'s>> {
        use Expr::*;

        match &expr.inner {
            Int(_) => Ok(Type::Int),
            Binop_II_I(_, x, y) => {
                self.expect_expr(x, Type::Int)?;
                self.expect_expr(y, Type::Int)?;
                Ok(Type::Int)
            }
        }
    }

    fn expect_expr(&self, expr: &WithSpan<Expr>, expected: Type) -> Result<Type, Error<'s>> {
        self.expect(self.check_expr(expr)?, expected, expr.span)
    }

    fn expect(&self, actual: Type, expected: Type, span: Span) -> Result<Type, Error<'s>> {
        if actual == expected {
            Ok(expected)
        } else {
            Err(Error::new(
                "Type Error",
                self.source,
                span,
                &format!("expected {}", expected),
                &format!("Expected type {} but found type {}", expected, actual),
            ))
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;

        match self {
            Int => write!(f, "Int"),
            Span => write!(f, "Span"),
        }
    }
}
