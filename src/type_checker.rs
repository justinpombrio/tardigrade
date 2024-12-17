use crate::ast::{Expr, Span};
use crate::error::Error;
use panfix::Source;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
}

pub struct TypeChecker<'s> {
    source: &'s Source,
}

impl<'s> TypeChecker<'s> {
    pub fn new(source: &'s Source) -> TypeChecker<'s> {
        TypeChecker { source }
    }

    /// Type check the given expression, which must be from the `Source` that this `TypeChecker`
    /// was cosntructed from.
    pub fn check_expr(&self, expr: &Expr) -> Result<Type, Error<'s>> {
        use Expr::*;

        match expr {
            Int(_) => Ok(Type::Int),
            Binop_II_I(_, x, y) => {
                self.expect_expr(&x.0, x.1, Type::Int)?;
                self.expect_expr(&y.0, y.1, Type::Int)?;
                Ok(Type::Int)
            }
        }
    }

    fn expect_expr(&self, expr: &Expr, span: Span, expected: Type) -> Result<Type, Error<'s>> {
        self.expect(self.check_expr(expr)?, expected, span)
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
        }
    }
}
