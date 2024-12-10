use crate::ast::{Binop_II_I, Expr, Prec, WithSpan};
use std::fmt;

trait Format {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result;
}

impl Format for Expr {
    #[allow(clippy::only_used_in_recursion)] // will be used later
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result {
        use Expr::*;

        match self {
            Int(n) => write!(f, "{}", n),
            Binop_II_I(binop, x, y) => {
                if binop.prec() > prec {
                    write!(f, "(")?;
                }
                x.inner.format(f, indent, binop.prec())?;
                write!(f, " {} ", binop)?;
                y.inner.format(f, indent, binop.prec())?;
                if binop.prec() > prec {
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Binop_II_I {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Binop_II_I::*;

        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format(f, 0, Prec::MAX)
    }
}

impl<T: fmt::Display> fmt::Display for WithSpan<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}
