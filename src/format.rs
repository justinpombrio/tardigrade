use crate::ast::{Binop, Block, Expr, FuncStmt, LetStmt, Prec, Stmt, Unop};
use crate::type_checker::Type;
use std::fmt;

const INDENT: u32 = 4;

/// Naive pretty printing.
trait Format {
    /// Formats `self`. `indent` is the number of spaces to prefix each new line with, and `prec`
    /// is the precedence level of the containing expression, used for inserting parentheses.
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result;
}

impl Format for Block {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result {
        for stmt in &self.0 {
            stmt.0.format(f, indent, prec)?;
        }
        Ok(())
    }
}

impl Format for Stmt {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result {
        use Stmt::*;

        match self {
            Expr(expr) => expr.0.format(f, indent, prec),
            Let(LetStmt { var, definition }) => {
                write!(f, "let {} = ", var.name)?;
                definition.0.format(f, indent + INDENT, Prec::MAX)
            }
            Func(FuncStmt {
                var,
                params,
                return_type,
                body,
            }) => {
                write!(f, "func {}(", var.name)?;
                let mut params = params.iter();
                if let Some(param) = params.next() {
                    write!(f, "{}: {}", param.var.name, param.ty)?;
                }
                for param in params {
                    write!(f, ", {}: {}", param.var.name, param.ty)?;
                }
                write!(f, ")")?;
                if *return_type != Type::Unit {
                    write!(f, " -> {}", return_type)?;
                }
                write!(f, " =")?;

                newline(f, indent + INDENT)?;
                body.0.format(f, indent + INDENT, Prec::MAX)?;

                newline(f, indent)?;
                write!(f, "end")
            }
        }
    }
}

impl Format for Expr {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result {
        use Expr::*;

        match self {
            Var(v) => write!(f, "{}", v.name),
            Unit => write!(f, "()"),
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
            Apply(func, args) => {
                write!(f, "{}(", func.0.name)?;
                let mut args_iter = args.iter();
                if let Some(first_arg) = args_iter.next() {
                    first_arg.0.format(f, indent, Prec::MAX)?;
                }
                for arg in args_iter {
                    write!(f, ", ")?;
                    arg.0.format(f, indent, Prec::MAX)?;
                }
                write!(f, ")")
            }
            Block(block) => {
                writeln!(f, "block")?;
                block.0.format(f, indent + INDENT, prec)?;
                writeln!(f, "end")
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
            Eq => write!(f, "=="),
            Ne => write!(f, "!="),
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

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.format(f, 0, Prec::MAX)
    }
}
