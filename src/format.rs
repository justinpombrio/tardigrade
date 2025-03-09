use crate::ast::{
    ApplyExpr, Binop, Block, Expr, FuncStmt, IfExpr, LetStmt, Prec, SetStmt, Stmt, Time, Unop,
};
use crate::type_checker::Type;
use std::fmt;

pub const INDENT_WIDTH: u32 = 4;

/// Naive pretty printing.
pub trait Format {
    /// Formats `self`. `indentation` is the number of indentation levels to prefix each new line with,
    /// and `prec` is the precedence level of the containing expression, used for inserting
    /// parentheses.
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result;
}

impl<F: Format + ?Sized> Format for &F {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result {
        F::format(self, f, indentation, prec)
    }
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
            Let(let_stmt) => let_stmt.format(f, indentation, prec),
            Set(set_stmt) => set_stmt.format(f, indentation, prec),
            Func(func_stmt) => func_stmt.format(f, indentation, prec),
        }
    }
}

impl Format for LetStmt {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, _prec: Prec) -> fmt::Result {
        if self.time == Time::Comptime {
            write!(f, "#")?;
        }
        write!(f, "let {} = ", self.name)?;
        self.definition.0.format(f, indentation + 1, Prec::MAX)
    }
}

impl Format for SetStmt {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, _prec: Prec) -> fmt::Result {
        if self.time == Time::Comptime {
            write!(f, "#")?;
        }
        write!(f, "set {} = ", self.var.0.name)?;
        self.definition.0.format(f, indentation + 1, Prec::MAX)
    }
}

impl Format for FuncStmt {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, _prec: Prec) -> fmt::Result {
        if self.time == Time::Comptime {
            write!(f, "#")?;
        }
        write!(f, "func {}(", self.name)?;
        let mut params = self.params.iter();
        if let Some(param) = params.next() {
            write!(f, "{}: {}", param.name, param.ty)?;
        }
        for param in params {
            write!(f, ", {}: {}", param.name, param.ty)?;
        }
        write!(f, ")")?;
        if matches!(self.return_type, Type::Unit) {
            write!(f, " -> {}", self.return_type)?;
        }
        write!(f, " {{")?;

        newline(f, indentation + 1)?;
        self.body.0.format(f, indentation + 1, Prec::MAX)?;

        newline(f, indentation)?;
        write!(f, "}}")
    }
}

impl Format for Expr {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result {
        use Expr::*;

        match self {
            Var(v) if v.time == Time::Comptime => write!(f, "#{}", v.name),
            Var(v) => write!(f, "{}", v.name),
            Literal(lit) => write!(f, "{}", lit),
            Tuple(elems) => {
                write!(f, "(")?;
                sep(f, elems.iter().map(|elem| &elem.0), ", ", indentation, prec)?;
                write!(f, ")")
            }
            TupleAccess(expr, index) => {
                expr.0.format(f, indentation, prec)?;
                write!(f, ".{}", index)
            }
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
            If(if_expr) => if_expr.format(f, indentation, prec),
            Apply(apply_expr) => apply_expr.format(f, indentation, prec),
            Block(block_expr) => {
                if block_expr.time == Time::Comptime {
                    write!(f, "#")?;
                }
                write!(f, "block {{")?;
                newline(f, indentation + 1)?;
                block_expr.block.0.format(f, indentation + 1, prec)?;
                newline(f, indentation)?;
                write!(f, "}}")
            }
            ComptimeExpr(expr) => {
                write!(f, "#(")?;
                expr.0.format(f, indentation, prec)?;
                write!(f, ")")
            }
            Return(expr) => {
                write!(f, "return ")?;
                expr.0.format(f, indentation + 1, prec)
            }
        }
    }
}

impl Format for IfExpr {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, _prec: Prec) -> fmt::Result {
        if self.time == Time::Comptime {
            write!(f, "#")?;
        }
        write!(f, "if ")?;
        self.e_if.0.format(f, indentation + 1, Prec::MAX)?;
        write!(f, " {{")?;

        newline(f, indentation + 1)?;
        self.e_then.0.format(f, indentation + 1, Prec::MAX)?;

        newline(f, indentation)?;
        write!(f, "}} else {{")?;

        newline(f, indentation + 1)?;
        self.e_else.0.format(f, indentation + 1, Prec::MAX)?;

        newline(f, indentation)?;
        write!(f, "}}")
    }
}

impl Format for ApplyExpr {
    fn format(&self, f: &mut impl fmt::Write, indentation: u32, prec: Prec) -> fmt::Result {
        if self.time == Time::Comptime {
            write!(f, "#")?;
        }
        write!(f, "{}(", self.func.0.name)?;
        sep(
            f,
            self.args.iter().map(|arg| &arg.0),
            ", ",
            indentation,
            prec,
        )?;
        write!(f, ")")
    }
}

fn sep<T: Format>(
    f: &mut impl fmt::Write,
    mut elems: impl Iterator<Item = T>,
    separator: &str,
    indentation: u32,
    prec: Prec,
) -> fmt::Result {
    if let Some(elem) = elems.next() {
        elem.format(f, indentation + 1, prec)?;
        for elem in elems {
            write!(f, "{}", separator)?;
            elem.format(f, indentation + 1, prec)?;
        }
    }
    Ok(())
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
