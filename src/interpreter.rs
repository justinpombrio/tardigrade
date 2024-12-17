use crate::ast::{Binop_II_I, Expr, Span, Value};
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
    pub fn interp_expr(&self, expr: &Expr, span: Span) -> Result<Value, Error<'s>> {
        use Expr::*;

        match expr {
            Int(n) => Ok(Value::int(*n)),
            Binop_II_I(binop, x, y) => {
                let x = self.interp_expr(&x.0, x.1)?.unwrap_int();
                let y = self.interp_expr(&y.0, y.1)?.unwrap_int();
                let z = self.apply_binop_ii_i(*binop, x, y, span)?;
                Ok(Value::int(z))
            }
        }
    }

    fn apply_binop_ii_i(
        &self,
        binop: Binop_II_I,
        x: i32,
        y: i32,
        span: Span,
    ) -> Result<i32, Error<'s>> {
        use Binop_II_I::*;

        match binop {
            Add => Ok(x + y),
            Sub => Ok(x - y),
            Mul => Ok(x * y),
            Div => {
                if y == 0 {
                    Err(self.error_divide_by_zero(span))
                } else {
                    Ok(x / y)
                }
            }
        }
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
