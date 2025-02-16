use crate::ast::{self, Block, Expr, FuncStmt, LetStmt, Literal, Span, Value, VarRefn};
use crate::error::Error;
use crate::logger::{Logger, Verbosity};
use crate::stack::Stack;
use crate::{log, span};
use panfix::Source;

pub struct Interpreter<'s, 'l> {
    source: &'s Source,
    funcs: &'s Vec<FuncStmt>,
    stack: Stack<Value>,
    logger: &'l mut Logger,
}

impl<'s, 'l> Interpreter<'s, 'l> {
    pub fn new(
        source: &'s Source,
        funcs: &'s Vec<FuncStmt>,
        logger: &'l mut Logger,
    ) -> Interpreter<'s, 'l> {
        Interpreter {
            source,
            funcs,
            stack: Stack::new(),
            logger,
        }
    }

    pub fn interp_prog(&mut self, block: &'s (Block, Span)) -> Result<Value, Error<'s>> {
        self.interp_block(block)
    }

    fn interp_block(&mut self, block: &'s (Block, Span)) -> Result<Value, Error<'s>> {
        span!(self.logger, Trace, "block", {
            let value = self.interp_block_impl(block)?;
            log!(self.logger, Trace, block.0);
            log!(self.logger, Trace, "value", ("{}", value));
            Ok(value)
        })
    }

    fn interp_block_impl(&mut self, block: &'s (Block, Span)) -> Result<Value, Error<'s>> {
        use ast::Stmt::*;

        let start_of_block = self.stack.start_block();
        let mut result = Value::unit();
        for stmt in &block.0 .0 {
            match &stmt.0 {
                Expr(e) => {
                    result = self.interp_expr(e)?;
                }
                Let(let_stmt) => {
                    self.interp_let(let_stmt)?;
                }
                Func(_func) => {
                    panic!("interp: leftover function");
                }
            }
        }
        self.stack.end_block(start_of_block);
        Ok(result)
    }

    fn interp_let(&mut self, let_stmt: &'s LetStmt) -> Result<(), Error<'s>> {
        span!(self.logger, Trace, "let", ("{}", let_stmt.var.name), {
            let value = self.interp_expr(&let_stmt.definition)?;
            self.stack.push(value);
            Ok(())
        })
    }

    /// Interpret the expression, producing either a result or a runtime Error. `expr` must be from
    /// the `Source` that this interpreter was constructed with.
    fn interp_expr(&mut self, expr: &'s (Expr, Span)) -> Result<Value, Error<'s>> {
        if self.logger.enabled(Verbosity::Trace) {
            if let (Expr::Literal(literal), _) = expr {
                let value = self.interp_literal(literal);
                log!(self.logger, Trace, "literal", ("{}", value));
                Ok(value)
            } else if let (Expr::Var(var), _) = expr {
                log!(
                    self.logger,
                    Trace,
                    "var",
                    (
                        "{} depth:{} offset:{}",
                        var.name,
                        var.unwrap_depth(),
                        var.unwrap_offset()
                    )
                );
                self.interp_var(var)
            } else {
                span!(self.logger, Trace, "expr", {
                    let value = self.interp_expr_impl(expr)?;
                    log!(self.logger, Trace, expr.0);
                    log!(self.logger, Trace, "value", ("{}", value));
                    Ok(value)
                })
            }
        } else {
            self.interp_expr_impl(expr)
        }
    }

    fn interp_expr_impl(&mut self, expr: &'s (Expr, Span)) -> Result<Value, Error<'s>> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        match &expr.0 {
            Var(var) => self.interp_var(var),
            Literal(literal) => Ok(self.interp_literal(literal)),
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
            Binop(Eq, x, y) => self.apply_binop_ii_b(x, y, |x, y| x == y),
            Binop(Ne, x, y) => self.apply_binop_ii_b(x, y, |x, y| x != y),
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
            Apply(func_refn, args) => {
                log!(
                    self.logger,
                    Trace,
                    "apply",
                    ("{} id:{}", func_refn.0.name, func_refn.0.unwrap_id())
                );
                let func = &self.funcs[func_refn.0.unwrap_id()];
                for arg in args {
                    let arg_val = self.interp_expr(arg)?;
                    self.stack.push(arg_val);
                }
                self.stack.start_frame(func_refn.0.unwrap_depth());
                let result = self.interp_block(&func.body)?;
                self.stack.end_frame(func.params.len());
                Ok(result)
            }
            Block(block) => self.interp_block(block),
        }
    }

    fn interp_literal(&mut self, literal: &Literal) -> Value {
        use Literal::*;

        match literal {
            Unit => Value::unit(),
            Bool(b) => Value::bool(*b),
            Int(n) => Value::int(*n),
        }
    }

    fn interp_var(&mut self, var: &VarRefn) -> Result<Value, Error<'s>> {
        let depth = var.unwrap_depth();
        let offset = var.unwrap_offset();
        Ok(self.stack.lookup(depth, offset).clone())
    }

    pub fn finish(&self) {
        self.stack.verify_empty();
    }

    fn apply_unop_b_b(
        &mut self,
        x: &'s (Expr, Span),
        f: impl Fn(bool) -> bool,
    ) -> Result<Value, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_bool();
        Ok(Value::bool(f(x)))
    }

    fn apply_binop_ii_i(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(i32, i32) -> i32,
    ) -> Result<Value, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_int();
        let y = self.interp_expr(y)?.unwrap_int();
        Ok(Value::int(f(x, y)))
    }

    fn apply_binop_ii_b(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(i32, i32) -> bool,
    ) -> Result<Value, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_int();
        let y = self.interp_expr(y)?.unwrap_int();
        Ok(Value::bool(f(x, y)))
    }

    fn apply_binop_bb_b(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
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
