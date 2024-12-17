use crate::ast::{self, Expr, Span};
use crate::error::Error;
use panfix::Source;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
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
    pub fn check_expr(&self, expr: &(Expr, Span)) -> Result<Type, Error<'s>> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        match &expr.0 {
            Bool(_) => Ok(Type::Bool),
            Int(_) => Ok(Type::Int),
            Unop(Not, x) => {
                self.expect_expr(x, Type::Bool)?;
                Ok(Type::Bool)
            }
            Binop(Add | Sub | Mul | Div, x, y) => {
                self.expect_expr(x, Type::Int)?;
                self.expect_expr(y, Type::Int)?;
                Ok(Type::Int)
            }
            Binop(Lt | Le | Gt | Ge, x, y) => {
                self.expect_expr(x, Type::Int)?;
                self.expect_expr(y, Type::Int)?;
                Ok(Type::Bool)
            }
            Binop(And | Or, x, y) => {
                self.expect_expr(x, Type::Bool)?;
                self.expect_expr(y, Type::Bool)?;
                Ok(Type::Bool)
            }
            If(e_if, e_then, e_else) => {
                self.expect_expr(e_if, Type::Bool)?;
                let t_then = self.check_expr(e_then)?;
                let t_else = self.check_expr(e_else)?;
                self.expect_branches(t_then, t_else, expr.1)
            }
        }
    }

    fn expect_expr(&self, expr: &(Expr, Span), expected: Type) -> Result<Type, Error<'s>> {
        self.expect(self.check_expr(expr)?, expected, expr.1)
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
                &format!("Expected type {} but found type {}.", expected, actual),
            ))
        }
    }

    fn expect_branches(&self, type_1: Type, type_2: Type, span: Span) -> Result<Type, Error<'s>> {
        if type_1 == type_2 {
            Ok(type_1)
        } else {
            Err(Error::new(
                "Type Error",
                self.source,
                span,
                "branches have different types",
                &format!(
                    "Expected branches to have the same type, but found type {} and type {}.",
                    type_1, type_2
                ),
            ))
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;

        match self {
            Bool => write!(f, "Bool"),
            Int => write!(f, "Int"),
        }
    }
}
