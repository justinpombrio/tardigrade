use crate::ast::{Binop, Expr, Prec, Unop};
use std::fmt;

const INDENT: u32 = 4;

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
            Bool(b) => write!(f, "{}", b),
            Int(n) => write!(f, "{}", n),
            Unop(unop, x) => {
                if unop.prec() > prec {
                    write!(f, "(")?;
                }
                write!(f, " {} ", unop)?;
                x.0.format(f, indent, unop.prec())?;
                if unop.prec() > prec {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Binop(binop, x, y) => {
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
            If(e_if, e_then, e_else) => {
                write!(f, "if ")?;
                e_if.0.format(f, indent + INDENT, Prec::MAX)?;
                write!(f, " then")?;

                newline(f, indent + INDENT)?;
                e_then.0.format(f, indent + INDENT, Prec::MAX)?;

                newline(f, indent)?;
                write!(f, "else")?;

                newline(f, indent + INDENT)?;
                e_else.0.format(f, indent + INDENT, Prec::MAX)?;

                newline(f, indent)?;
                write!(f, "end")
            }
        }
    }
}

fn newline(f: &mut fmt::Formatter, indent: u32) -> fmt::Result {
    writeln!(f)?;
    write!(f, "{:indent$}", "", indent = indent as usize)
}

impl fmt::Display for Unop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Unop::*;

        match self {
            Not => write!(f, "not"),
        }
    }
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Binop::*;

        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            Lt => write!(f, "<"),
            Le => write!(f, "<="),
            Gt => write!(f, ">"),
            Ge => write!(f, ">="),
            And => write!(f, "and"),
            Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format(f, 0, Prec::MAX)
    }
}
