use crate::ast::{Binop, Block, Expr, FuncStmt, LetStmt, Prec, Stmt, Unop};
use crate::type_checker::Type;
use std::fmt;

pub const INDENT_WIDTH: u32 = 2;

/// Naive pretty printing.
pub trait Format {
    /// Formats `self`. `indentation` is the number of indentation levels to prefix each new line with,
    /// and `prec` is the precedence level of the containing expression, used for inserting
    /// parentheses.
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result;
}

impl Format for str {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, _prec: Prec) -> fmt::Result {
        for (i, line) in self.lines().enumerate() {
            if i != 0 {
                newline(f, indentation)?;
            }
            write!(f, "{}", line)?;
        }
        Ok(())
    }
}

impl Format for String {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result {
        (self as &str).format(f, indentation, prec)
    }
}

impl Format for Block {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result {
        for (i, stmt) in self.0.iter().enumerate() {
            if i != 0 {
                newline(f, indentation)?;
            }
            stmt.0.format(f, indentation, prec)?;
        }
        Ok(())
    }
}

impl Format for Stmt {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result {
        use Stmt::*;

        match self {
            Expr(expr) => expr.0.format(f, indentation, prec),
            Let(LetStmt { var, definition }) => {
                write!(f, "let {} = ", var.name)?;
                definition.0.format(f, indentation + 1, Prec::MAX)
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

                newline(f, indentation + 1)?;
                body.0.format(f, indentation + 1, Prec::MAX)?;

                newline(f, indentation)?;
                write!(f, "end")
            }
        }
    }
}

impl Format for Expr {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result {
        use crate::ast::Literal::*;
        use Expr::*;

        match self {
            Var(v) => write!(f, "{}", v.name),
            Literal(Unit) => write!(f, "()"),
            Literal(Bool(b)) => write!(f, "{}", b),
            Literal(Int(n)) => write!(f, "{}", n),
            Unop(unop, x) => {
                if unop.prec() > prec {
                    write!(f, "(")?;
                }
                write!(f, " {} ", unop)?;
                x.0.format(f, indentation, unop.prec())?;
                if unop.prec() > prec {
                    write!(f, ")")?;
                }
                Ok(())
            }
            Binop(binop, x, y) => {
                if binop.prec() > prec {
                    write!(f, "(")?;
                }
                x.0.format(f, indentation, binop.prec())?;
                write!(f, " {} ", binop)?;
                y.0.format(f, indentation, binop.prec())?;
                if binop.prec() > prec {
                    write!(f, ")")?;
                }
                Ok(())
            }
            If(e_if, e_then, e_else) => {
                write!(f, "if ")?;
                e_if.0.format(f, indentation + 1, Prec::MAX)?;
                write!(f, " then")?;

                newline(f, indentation + 1)?;
                e_then.0.format(f, indentation + 1, Prec::MAX)?;

                newline(f, indentation)?;
                write!(f, "else")?;

                newline(f, indentation + 1)?;
                e_else.0.format(f, indentation + 1, Prec::MAX)?;

                newline(f, indentation)?;
                write!(f, "end")
            }
            Apply(func, args) => {
                write!(f, "{}(", func.0.name)?;
                let mut args_iter = args.iter();
                if let Some(first_arg) = args_iter.next() {
                    first_arg.0.format(f, indentation, Prec::MAX)?;
                }
                for arg in args_iter {
                    write!(f, ", ")?;
                    arg.0.format(f, indentation, Prec::MAX)?;
                }
                write!(f, ")")
            }
            Block(block) => {
                writeln!(f, "block")?;
                block.0.format(f, indentation + 1, prec)?;
                writeln!(f, "end")
            }
        }
    }
}

fn indent(f: &mut impl fmt::Write, indentation: u32) -> fmt::Result {
    write!(
        f,
        "{:indent$}",
        "",
        indent = (indentation * INDENT_WIDTH) as usize
    )
}

fn newline(f: &mut impl fmt::Write, indentation: u32) -> fmt::Result {
    writeln!(f)?;
    indent(f, indentation)
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
