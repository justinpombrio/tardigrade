use crate::ast::{self, Expr, Span, Value};
use crate::error::Error;
use panfix::Source;

pub struct Interpreter<'s> {
    source: &'s Source,
}

impl<'s> Interpreter<'s> {
    pub fn new(source: &'s Source) -> Interpreter<'s> {
        Interpreter { source }
    }

    /// Interpret the expression, producing either a result or a runtime Error. `expr` and `span`
    /// must be from the `Source` that this interpreter was constructed with.
    pub fn interp_expr(&self, expr: &(Expr, Span)) -> Result<Value, Error<'s>> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        match &expr.0 {
            Bool(b) => Ok(Value::bool(*b)),
            Int(n) => Ok(Value::int(*n)),
            Unop(Not, x) => self.apply_unop_b_b(x, |x| !x),
            Binop(Add, x, y) => self.apply_binop_ii_i(x, y, |x, y| x + y),
            Binop(Sub, x, y) => self.apply_binop_ii_i(x, y, |x, y| x - y),
            Binop(Mul, x, y) => self.apply_binop_ii_i(x, y, |x, y| x * y),
            Binop(Div, x, y) => {
                let span = y.1;
                let x = self.interp_expr(x)?.unwrap_int();
                let y = self.interp_expr(y)?.unwrap_int();
                if y == 0 {
                    Err(self.error_divide_by_zero(span))
                } else {
                    Ok(Value::int(x / y))
                }
            }
            Binop(Lt, x, y) => self.apply_binop_ii_b(x, y, |x, y| x < y),
            Binop(Le, x, y) => self.apply_binop_ii_b(x, y, |x, y| x <= y),
            Binop(Gt, x, y) => self.apply_binop_ii_b(x, y, |x, y| x > y),
            Binop(Ge, x, y) => self.apply_binop_ii_b(x, y, |x, y| x >= y),
            Binop(And, x, y) => self.apply_binop_bb_b(x, y, |x, y| x && y),
            Binop(Or, x, y) => self.apply_binop_bb_b(x, y, |x, y| x || y),
            If(e_if, e_then, e_else) => {
                let b = self.interp_expr(e_if)?.unwrap_bool();
                if b {
                    self.interp_expr(e_then)
                } else {
                    self.interp_expr(e_else)
                }
            }
        }
    }

    fn apply_unop_b_b(
        &self,
        x: &(Expr, Span),
        f: impl Fn(bool) -> bool,
    ) -> Result<Value, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_bool();
        Ok(Value::bool(f(x)))
    }

    fn apply_binop_ii_i(
        &self,
        x: &(Expr, Span),
        y: &(Expr, Span),
        f: impl Fn(i32, i32) -> i32,
    ) -> Result<Value, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_int();
        let y = self.interp_expr(y)?.unwrap_int();
        Ok(Value::int(f(x, y)))
    }

    fn apply_binop_ii_b(
        &self,
        x: &(Expr, Span),
        y: &(Expr, Span),
        f: impl Fn(i32, i32) -> bool,
    ) -> Result<Value, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_int();
        let y = self.interp_expr(y)?.unwrap_int();
        Ok(Value::bool(f(x, y)))
    }

    fn apply_binop_bb_b(
        &self,
        x: &(Expr, Span),
        y: &(Expr, Span),
        f: impl Fn(bool, bool) -> bool,
    ) -> Result<Value, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_bool();
        let y = self.interp_expr(y)?.unwrap_bool();
        Ok(Value::bool(f(x, y)))
    }

    fn error_divide_by_zero(&self, span: Span) -> Error<'s> {
        Error::new(
            "Runtime Error",
            self.source,
            span,
            "was zero",
            "Division by zero.",
        )
    }
}
