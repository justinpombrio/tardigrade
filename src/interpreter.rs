use crate::ast::{
    self, ApplyExpr, Block, BlockExpr, Expr, FuncStmt, IfExpr, LetStmt, Literal, SetStmt, Span,
    Stmt, Time, Value, VarRefn,
};
use crate::error::Error;
use crate::logger::{Logger, Verbosity};
use crate::stack::Stack;
use crate::{log, span};
use panfix::Source;

use Time::{Comptime, Runtime};

pub struct Interpreter<'s, 'l> {
    source: &'s Source,
    funcs: &'s Vec<FuncStmt>,
    stack: Stack<Value>,
    logger: &'l mut Logger,
}

enum EvalException<'s> {
    Return(Value),
    Error(Error<'s>),
}

impl<'s> EvalException<'s> {
    fn unwrap_error(self) -> Error<'s> {
        match self {
            EvalException::Return(value) => panic!("compile: unaught return of value {}", value),
            EvalException::Error(err) => err,
        }
    }
}

type EvalResult<'s, V> = Result<V, EvalException<'s>>;

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

    /***************
     * Compilation *
     ***************/

    pub fn compile_prog(
        &mut self,
        block: &'s (Block, Span),
        funcs: &'s Vec<FuncStmt>,
    ) -> Result<((Block, Span), Vec<FuncStmt>), Error<'s>> {
        span!(self.logger, Trace, "compile", {
            span!(self.logger, Trace, "program", {
                log!(self.logger, Trace, &block.0);
            });
            span!(self.logger, Trace, "functions", {
                for func in funcs {
                    span!(self.logger, Trace, "function", {
                        log!(self.logger, Trace, func);
                    });
                }
            });
            let rt_block = self
                .compile_block(block)
                .map_err(|exc| exc.unwrap_error())?;
            let rt_funcs = funcs
                .iter()
                .map(|func| self.compile_func(func))
                .collect::<Result<Vec<_>, _>>()
                .map_err(|exc| exc.unwrap_error())?;
            Ok((rt_block, rt_funcs))
        })
    }

    fn compile_block(&mut self, block: &'s (Block, Span)) -> EvalResult<'s, (Block, Span)> {
        span!(self.logger, Trace, "compile_block", {
            let result = self.compile_block_impl(block)?;
            log!(self.logger, Trace, &block.0);
            span!(self.logger, Trace, "block", {
                log!(self.logger, Trace, &result.0);
            });
            Ok(result)
        })
    }

    fn compile_block_impl(&mut self, block: &'s (Block, Span)) -> EvalResult<'s, (Block, Span)> {
        use ast::Stmt::*;

        let mut stmts = Vec::new();
        let start_of_block = self.stack.start_block();
        for stmt in &block.0 .0 {
            match &stmt.0 {
                Expr(expr) => {
                    let rt_expr = self.compile_expr(expr)?;
                    stmts.push((Expr(rt_expr), expr.1));
                }
                Let(let_stmt) => self.compile_let(let_stmt, stmt.1, &mut stmts)?,
                Set(set_stmt) => self.compile_set(set_stmt, stmt.1, &mut stmts)?,
                Func(_) => {
                    panic!("compile: leftover function");
                }
            }
        }
        self.stack.end_block(start_of_block);
        Ok((Block(stmts), block.1))
    }

    fn compile_func(&mut self, func: &'s FuncStmt) -> EvalResult<'s, FuncStmt> {
        let body = self.compile_block(&func.body)?;
        Ok(FuncStmt {
            var: func.var.clone(),
            params: func.params.clone(),
            return_type: func.return_type.clone(),
            body,
            time: Runtime,
        })
    }

    fn compile_let(
        &mut self,
        let_stmt: &'s LetStmt,
        span: Span,
        stmts: &mut Vec<(Stmt, Span)>,
    ) -> EvalResult<'s, ()> {
        span!(self.logger, Trace, "let", ("{}", let_stmt.var.name), {
            match let_stmt.time {
                Comptime => {
                    self.eval_let(let_stmt)?;
                    Ok(())
                }
                Runtime => {
                    let definition = self.compile_expr(&let_stmt.definition)?;
                    let rt_let = LetStmt {
                        var: let_stmt.var.clone(),
                        definition,
                        time: Runtime,
                    };
                    stmts.push((Stmt::Let(rt_let), span));
                    Ok(())
                }
            }
        })
    }

    fn compile_set(
        &mut self,
        set_stmt: &'s SetStmt,
        span: Span,
        stmts: &mut Vec<(Stmt, Span)>,
    ) -> EvalResult<'s, ()> {
        span!(self.logger, Trace, "set", ("{}", set_stmt.var.0.name), {
            match set_stmt.time {
                Comptime => {
                    self.eval_set(set_stmt)?;
                    Ok(())
                }
                Runtime => {
                    let definition = self.compile_expr(&set_stmt.definition)?;
                    let rt_set = SetStmt {
                        var: set_stmt.var.clone(),
                        definition,
                        time: Runtime,
                    };
                    stmts.push((Stmt::Set(rt_set), span));
                    Ok(())
                }
            }
        })
    }

    fn compile_expr(&mut self, expr: &'s (Expr, Span)) -> EvalResult<'s, (Expr, Span)> {
        if self.logger.enabled(Verbosity::Trace) {
            if let (Expr::Literal(literal), _) = expr {
                log!(self.logger, Trace, "literal", ("{}", literal));
                Ok((Expr::Literal(literal.clone()), expr.1))
            } else if let (Expr::Var(var), span) = expr {
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
                self.compile_var(var, *span)
            } else {
                span!(self.logger, Trace, "expr", {
                    let rt_expr = self.compile_expr_impl(expr)?;
                    log!(self.logger, Trace, &expr.0);
                    span!(self.logger, Trace, "expr", {
                        log!(self.logger, Trace, &rt_expr.0);
                    });
                    Ok(rt_expr)
                })
            }
        } else {
            self.compile_expr_impl(expr)
        }
    }

    fn compile_expr_impl(&mut self, expr: &'s (Expr, Span)) -> EvalResult<'s, (Expr, Span)> {
        use Expr::*;

        let span = expr.1;
        match &expr.0 {
            Var(var) => Ok(self.compile_var(var, span)?),
            Literal(literal) => Ok((Expr::Literal(literal.clone()), span)),
            Unop(unop, x) => {
                let rt_x = self.compile_expr(x)?;
                Ok((Unop(*unop, Box::new(rt_x)), span))
            }
            Binop(binop, x, y) => {
                let rt_x = self.compile_expr(x)?;
                let rt_y = self.compile_expr(y)?;
                Ok((Binop(*binop, Box::new(rt_x), Box::new(rt_y)), span))
            }
            If(if_expr) => self.compile_if_expr(if_expr, span),
            Apply(apply_expr) => self.compile_apply_expr(apply_expr, span),
            Block(block_expr) => self.compile_block_expr(block_expr, span),
            ComptimeExpr(expr) => {
                let value = self.eval_expr(expr)?;
                Ok((Expr::Literal(value.into_literal()), span))
            }
            Return(expr) => {
                let rt_expr = self.compile_expr(expr)?;
                Ok((Expr::Return(Box::new(rt_expr)), span))
            }
        }
    }

    fn compile_if_expr(&mut self, if_expr: &'s IfExpr, span: Span) -> EvalResult<'s, (Expr, Span)> {
        match if_expr.time {
            Comptime => {
                let cond = self.eval_expr(&if_expr.e_if)?.unwrap_bool();
                let block = if cond {
                    self.compile_block(&if_expr.e_then)?
                } else {
                    self.compile_block(&if_expr.e_else)?
                };
                let block_expr = BlockExpr {
                    block: Box::new(block),
                    time: Runtime,
                };
                Ok((Expr::Block(block_expr), span))
            }
            Runtime => {
                let e_if = self.compile_expr(&if_expr.e_if)?;
                let e_then = self.compile_block(&if_expr.e_then)?;
                let e_else = self.compile_block(&if_expr.e_else)?;
                let rt_if_expr = IfExpr {
                    e_if: Box::new(e_if),
                    e_then: Box::new(e_then),
                    e_else: Box::new(e_else),
                    time: Runtime,
                };
                Ok((Expr::If(rt_if_expr), span))
            }
        }
    }

    fn compile_apply_expr(
        &mut self,
        apply_expr: &'s ApplyExpr,
        span: Span,
    ) -> EvalResult<'s, (Expr, Span)> {
        match apply_expr.time {
            Comptime => {
                let value = self.eval_apply_expr(apply_expr)?;
                Ok((Expr::Literal(value.into_literal()), span))
            }
            Runtime => {
                let rt_args = apply_expr
                    .args
                    .iter()
                    .map(|arg| self.compile_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                let rt_apply_expr = ApplyExpr {
                    func: apply_expr.func.clone(),
                    args: rt_args,
                    time: Runtime,
                };
                Ok((Expr::Apply(rt_apply_expr), span))
            }
        }
    }

    fn compile_block_expr(
        &mut self,
        block_expr: &'s BlockExpr,
        span: Span,
    ) -> EvalResult<'s, (Expr, Span)> {
        match block_expr.time {
            Comptime => {
                let value = self.eval_block(&block_expr.block)?;
                Ok((Expr::Literal(value.into_literal()), span))
            }
            Runtime => {
                let rt_block = self.compile_block(&block_expr.block)?;
                let rt_block_expr = BlockExpr {
                    block: Box::new(rt_block),
                    time: Runtime,
                };
                Ok((Expr::Block(rt_block_expr), span))
            }
        }
    }

    fn compile_var(&mut self, var: &VarRefn, span: Span) -> EvalResult<'s, (Expr, Span)> {
        match var.time {
            Runtime => Ok((Expr::Var(var.clone()), span)),
            Comptime => {
                let value = self.eval_var(var)?;
                Ok((Expr::Literal(value.into_literal()), span))
            }
        }
    }

    /**************
     * Evaluation *
     **************/

    pub fn eval_prog(&mut self, block: &'s (Block, Span)) -> Result<Value, Error<'s>> {
        span!(self.logger, Trace, "evaluate", {
            span!(self.logger, Trace, "program", {
                log!(self.logger, Trace, &block.0);
            });
            self.eval_block(block).map_err(|exc| exc.unwrap_error())
        })
    }

    fn eval_block(&mut self, block: &'s (Block, Span)) -> EvalResult<'s, Value> {
        span!(self.logger, Trace, "eval_block", {
            let value = self.eval_block_impl(block)?;
            log!(self.logger, Trace, &block.0);
            log!(self.logger, Trace, "value", ("{}", value));
            Ok(value)
        })
    }

    fn eval_block_impl(&mut self, block: &'s (Block, Span)) -> EvalResult<'s, Value> {
        use ast::Stmt::*;

        let start_of_block = self.stack.start_block();
        let mut result = Value::unit();
        for stmt in &block.0 .0 {
            match &stmt.0 {
                Expr(e) => {
                    result = self.eval_expr(e)?;
                }
                Let(let_stmt) => self.eval_let(let_stmt)?,
                Set(set_stmt) => self.eval_set(set_stmt)?,
                Func(_) => {
                    panic!("eval: leftover function");
                }
            }
        }
        self.stack.end_block(start_of_block);
        Ok(result)
    }

    fn eval_let(&mut self, let_stmt: &'s LetStmt) -> EvalResult<'s, ()> {
        span!(self.logger, Trace, "let", ("{}", let_stmt.var.name), {
            let value = self.eval_expr(&let_stmt.definition)?;
            self.stack.push(value);
            Ok(())
        })
    }

    fn eval_set(&mut self, set_stmt: &'s SetStmt) -> EvalResult<'s, ()> {
        span!(self.logger, Trace, "set", ("{}", set_stmt.var.0.name), {
            let var = &set_stmt.var.0;
            let (depth, offset) = (var.unwrap_depth(), var.unwrap_offset());
            log!(
                self.logger,
                Trace,
                "var",
                ("{} depth:{} offset:{}", var.name, depth, offset)
            );
            let value = self.eval_expr(&set_stmt.definition)?;
            *self.stack.lookup_mut(depth, offset) = value;
            Ok(())
        })
    }

    fn eval_expr(&mut self, expr: &'s (Expr, Span)) -> EvalResult<'s, Value> {
        if self.logger.enabled(Verbosity::Trace) {
            if let (Expr::Literal(literal), _) = expr {
                let value = self.eval_literal(literal);
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
                self.eval_var(var)
            } else {
                span!(self.logger, Trace, "expr", {
                    let value = self.eval_expr_impl(expr)?;
                    log!(self.logger, Trace, &expr.0);
                    log!(self.logger, Trace, "value", ("{}", value));
                    Ok(value)
                })
            }
        } else {
            self.eval_expr_impl(expr)
        }
    }

    fn eval_expr_impl(&mut self, expr: &'s (Expr, Span)) -> EvalResult<'s, Value> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        match &expr.0 {
            Var(var) => self.eval_var(var),
            Literal(literal) => Ok(self.eval_literal(literal)),
            Unop(Not, x) => self.apply_unop_b_b(x, |x| !x),
            Binop(Add, x, y) => self.apply_binop_ii_i(x, y, |x, y| x + y),
            Binop(Sub, x, y) => self.apply_binop_ii_i(x, y, |x, y| x - y),
            Binop(Mul, x, y) => self.apply_binop_ii_i(x, y, |x, y| x * y),
            Binop(Div, x, y) => {
                let span = y.1;
                let x = self.eval_expr(x)?.unwrap_int();
                let y = self.eval_expr(y)?.unwrap_int();
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
            If(if_expr) => self.eval_if_expr(if_expr),
            Apply(apply_expr) => self.eval_apply_expr(apply_expr),
            Block(block) => self.eval_block(&block.block),
            ComptimeExpr(_) => {
                panic!("eval: leftover comptime")
            }
            Return(expr) => {
                let value = self.eval_expr(expr)?;
                Err(EvalException::Return(value))
            }
        }
    }

    fn eval_if_expr(&mut self, expr: &'s IfExpr) -> EvalResult<'s, Value> {
        let b = self.eval_expr(&expr.e_if)?.unwrap_bool();
        if b {
            self.eval_block(&expr.e_then)
        } else {
            self.eval_block(&expr.e_else)
        }
    }

    fn eval_apply_expr(&mut self, expr: &'s ApplyExpr) -> EvalResult<'s, Value> {
        log!(
            self.logger,
            Trace,
            "apply",
            ("{} id:{}", &expr.func.0.name, &expr.func.0.unwrap_id())
        );
        let func = &self.funcs[expr.func.0.unwrap_id()];
        for arg in &expr.args {
            let arg_val = self.eval_expr(arg)?;
            self.stack.push(arg_val);
        }
        self.stack.start_frame(expr.func.0.unwrap_depth());
        let value = match self.eval_block(&func.body) {
            Ok(value) => value,
            Err(EvalException::Return(value)) => value,
            Err(EvalException::Error(err)) => return Err(EvalException::Error(err)),
        };
        self.stack.end_frame(func.params.len());
        Ok(value)
    }

    fn eval_literal(&mut self, literal: &Literal) -> Value {
        use Literal::*;

        match literal {
            Unit => Value::unit(),
            Bool(b) => Value::bool(*b),
            Int(n) => Value::int(*n),
        }
    }

    fn eval_var(&mut self, var: &VarRefn) -> EvalResult<'s, Value> {
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
    ) -> EvalResult<'s, Value> {
        let x = self.eval_expr(x)?.unwrap_bool();
        Ok(Value::bool(f(x)))
    }

    fn apply_binop_ii_i(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(i32, i32) -> i32,
    ) -> EvalResult<'s, Value> {
        let x = self.eval_expr(x)?.unwrap_int();
        let y = self.eval_expr(y)?.unwrap_int();
        Ok(Value::int(f(x, y)))
    }

    fn apply_binop_ii_b(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(i32, i32) -> bool,
    ) -> EvalResult<'s, Value> {
        let x = self.eval_expr(x)?.unwrap_int();
        let y = self.eval_expr(y)?.unwrap_int();
        Ok(Value::bool(f(x, y)))
    }

    fn apply_binop_bb_b(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(bool, bool) -> bool,
    ) -> EvalResult<'s, Value> {
        let x = self.eval_expr(x)?.unwrap_bool();
        let y = self.eval_expr(y)?.unwrap_bool();
        Ok(Value::bool(f(x, y)))
    }

    fn error_divide_by_zero(&self, span: Span) -> EvalException<'s> {
        EvalException::Error(Error::new(
            "Runtime Error",
            self.source,
            span,
            "was zero",
            "Division by zero.",
        ))
    }
}
