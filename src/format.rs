use crate::ast::{Binop_II_I, Expr, Prec};
use std::fmt;

/// Naive pretty printing.
trait Format {
    /// Formats `self`. `indent` is the number of spaces to prefix each new line with, and `prec`
    /// is the precedence level of the containing expression, used for inserting parentheses.
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
                x.0.format(f, indent, binop.prec())?;
                write!(f, " {} ", binop)?;
                y.0.format(f, indent, binop.prec())?;
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
