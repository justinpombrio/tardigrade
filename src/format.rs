use crate::ast::{Binop, Block, Expr, FuncStmt, IfExpr, LetStmt, Param, Prec, Stmt, Unop};
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
        for stmt in &self.stmts {
            stmt.0.format(f, indent, prec)?;
        }
        Ok(())
    }
}

impl Format for Stmt {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result {
        use Stmt::*;

        match self {
            Expr((expr, _)) => expr.format(f, indent, prec),
            Let(let_stmt) => let_stmt.format(f, indent, prec),
            Func(func_stmt) => func_stmt.format(f, indent, prec),
        }
    }
}

impl Format for LetStmt {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, _prec: Prec) -> fmt::Result {
        if self.comptime {
            write!(f, "#")?;
        }
        write!(f, "let {} = ", self.var.name)?;
        self.definition.0.format(f, indent + INDENT, Prec::MAX)
    }
}

impl Format for FuncStmt {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result {
        if self.comptime {
            write!(f, "#")?;
        }
        write!(f, "func {}(", self.var.name)?;
        let mut params = self.params.iter();
        if let Some(param) = params.next() {
            param.format(f, indent, prec)?;
        }
        for param in params {
            write!(f, ", ")?;
            param.format(f, indent, prec)?;
        }
        write!(f, ")")?;
        if self.return_type != Type::Unit {
            write!(f, " -> {}", self.return_type)?;
        }
        write!(f, " =")?;

        newline(f, indent + INDENT)?;
        self.body.0.format(f, indent + INDENT, Prec::MAX)?;

        newline(f, indent)?;
        write!(f, "end")
    }
}

impl Format for Param {
    fn format(&self, f: &mut fmt::Formatter, _indent: u32, _prec: Prec) -> fmt::Result {
        write!(f, "{}", self.var.name)?;
        write!(f, ": ")?;
        write!(f, "{}", self.ty)
    }
}

impl Format for Expr {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, prec: Prec) -> fmt::Result {
        use Expr::*;

        match self {
            Var(refn) if refn.comptime => write!(f, "#{}", refn.name),
            Var(refn) => write!(f, "{}", refn.name),
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
            If(if_expr) => if_expr.format(f, indent, prec),
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
                if block.0.comptime {
                    write!(f, "#")?;
                }
                writeln!(f, "block")?;
                block.0.format(f, indent + INDENT, prec)?;
                writeln!(f, "end")
            }
            Comptime(expr) => {
                write!(f, "#(")?;
                expr.0.format(f, indent + INDENT, Prec::MAX)?;
                write!(f, ")")
            }
        }
    }
}

impl Format for IfExpr {
    fn format(&self, f: &mut fmt::Formatter, indent: u32, _prec: Prec) -> fmt::Result {
        if self.comptime {
            write!(f, "#")?;
        }
        write!(f, "if ")?;
        self.e_if.0.format(f, indent + INDENT, Prec::MAX)?;
        write!(f, " then")?;

        newline(f, indent + INDENT)?;
        self.e_then.0.format(f, indent + INDENT, Prec::MAX)?;

        newline(f, indent)?;
        write!(f, "else")?;

        newline(f, indent + INDENT)?;
        self.e_else.0.format(f, indent + INDENT, Prec::MAX)?;

        newline(f, indent)?;
        write!(f, "end")
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
