//! - At comptime, you _interpret_ comptime expressions and _compile_ runtime expressions.
//! - At runtime, you _interpret_ everything (and everything is runtime).
//!
//! So if you're compiling and see a comptime thing, you stop compiling and start interpreting.

use crate::ast::{self, Block, Expr, FuncStmt, IfExpr, LetStmt, Span, Stmt, Value};
use crate::error::Error;
use crate::stack::Stack;
use panfix::Source;

pub struct Evaluator<'s> {
    source: &'s Source,
    stack: Stack<Value<'s>>,
}

impl<'s> Evaluator<'s> {
    pub fn new(source: &'s Source) -> Evaluator<'s> {
        Evaluator {
            source,
            stack: Stack::new(),
        }
    }

    pub fn interp_block(&mut self, block: &'s (Block, Span)) -> Result<Value<'s>, Error<'s>> {
        use ast::Stmt::*;

        no_comptime(block.0.comptime);
        let start_of_block = self.stack.start_block();
        let mut result = Value::unit();
        for stmt in &block.0.stmts {
            match &stmt.0 {
                Expr(e) => {
                    result = self.interp_expr(e)?;
                }
                Let(let_stmt) => {
                    let value = self.interp_expr(&let_stmt.definition)?;
                    self.stack.push(value);
                }
                Func(func) => {
                    let value = Value::func_ptr(func);
                    self.stack.push(value);
                }
            }
        }
        self.stack.end_block(start_of_block);
        Ok(result)
    }

    pub fn compile_block(&mut self, block: &'s (Block, Span)) -> Result<Block, Error<'s>> {
        use ast::Stmt::*;

        assert!(!block.0.comptime);

        let mut stmts = Vec::new();
        let start_of_block = self.stack.start_block();
        for stmt in &block.0.stmts {
            match &stmt.0 {
                Expr(expr) => {
                    let rt_expr = self.compile_expr(expr)?;
                    stmts.push((Stmt::Expr(rt_expr), expr.1));
                }
                Let(let_stmt) => {
                    if let_stmt.comptime {
                        let value = self.interp_expr(&let_stmt.definition)?;
                        self.stack.push(value);
                    } else {
                        let definition = self.compile_expr(&let_stmt.definition)?;
                        let rt_let = LetStmt {
                            var: let_stmt.var.clone(),
                            definition,
                            comptime: false,
                        };
                        stmts.push((Stmt::Let(rt_let), stmt.1));
                    }
                }
                Func(func) => {
                    if func.comptime {
                        let value = Value::func_ptr(func);
                        self.stack.push(value);
                    } else {
                        let body = self.compile_block(&func.body)?;
                        let rt_func = FuncStmt {
                            var: func.var.clone(),
                            params: func.params.clone(),
                            return_type: func.return_type.clone(),
                            body: (body, func.body.1),
                            comptime: false,
                        };
                    }
                }
            }
        }
        self.stack.end_block(start_of_block);
        Ok(Block {
            stmts,
            comptime: false,
        })
    }

    pub fn interp_expr(&mut self, expr: &'s (Expr, Span)) -> Result<Value<'s>, Error<'s>> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        match &expr.0 {
            Var(var_refn) => Ok(self.stack.lookup(var_refn.refn()).clone()),
            Unit => Ok(Value::unit()),
            Bool(b) => Ok(Value::bool(*b)),
            Int(n) => Ok(Value::int(*n)),
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
            If(if_expr) => {
                let b = self.interp_expr(&if_expr.e_if)?.unwrap_bool();
                if b {
                    self.interp_expr(&if_expr.e_then)
                } else {
                    self.interp_expr(&if_expr.e_else)
                }
            }
            Apply(var_refn, args) => {
                let func = self.stack.lookup(var_refn.0.refn()).clone();
                let func = func.unwrap_func_ptr();
                for arg in args {
                    let arg_val = self.interp_expr(arg)?;
                    self.stack.push(arg_val);
                }
                self.stack.start_frame(var_refn.0.refn());
                let result = self.interp_block(&func.body)?;
                self.stack.end_frame(func.params.len());
                Ok(result)
            }
            Block(block) => self.interp_block(block),
            Comptime(expr) => self.interp_expr(expr),
        }
    }

    pub fn compile_expr(&mut self, expr: &'s (Expr, Span)) -> Result<(Expr, Span), Error<'s>> {
        use ast::Unop::*;
        use Expr::*;

        let rt_expr = match &expr.0 {
            Var(var_refn) => {
                if var_refn.comptime {
                    let value = self.stack.lookup(var_refn.refn()).clone();
                    self.compile_value(value)
                } else {
                    Var(var_refn.clone())
                }
            }
            Unit => Unit,
            Bool(b) => Bool(*b),
            Int(n) => Int(*n),
            Unop(Not, x) => {
                let x = self.compile_expr(x)?;
                Unop(Not, Box::new(x))
            }
            Binop(op, x, y) => {
                let x = self.compile_expr(x)?;
                let y = self.compile_expr(y)?;
                Binop(*op, Box::new(x), Box::new(y))
            }
            If(if_expr) => {
                if if_expr.comptime {
                    let b = self.interp_expr(&if_expr.e_if)?.unwrap_bool();
                    if b {
                        return self.compile_expr(&if_expr.e_then);
                    } else {
                        return self.compile_expr(&if_expr.e_else);
                    }
                } else {
                    let e_if = self.compile_expr(&if_expr.e_if)?;
                    let e_then = self.compile_expr(&if_expr.e_then)?;
                    let e_else = self.compile_expr(&if_expr.e_else)?;
                    If(IfExpr {
                        e_if: Box::new(e_if),
                        e_then: Box::new(e_then),
                        e_else: Box::new(e_else),
                        comptime: false,
                    })
                }
            }
            Apply(var_refn, args) => {
                let rt_args = args
                    .into_iter()
                    .map(|arg| self.compile_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                Apply(var_refn.clone(), rt_args)
            }
            Block(block) => {
                if block.0.comptime {
                    // TODO: Don't duplicate values between Value and Expr.
                    // Instead have Expr::Literal(Value).
                    let value = self.interp_block(block)?;
                    self.compile_value(value)
                } else {
                    let rt_block = self.compile_block(block)?;
                    Block((rt_block, block.1))
                }
            }
            Comptime(expr) => {
                let value = self.interp_expr(expr)?;
                self.compile_value(value)
            }
        };

        Ok((rt_expr, expr.1))
    }

    fn compile_value(&mut self, value: Value<'s>) -> Expr {
        match value.into_expr() {
            Some(expr) => expr,
            None => panic!("Bug in TC: reference to non-value"),
        }
    }

    pub fn finish(&self) {
        self.stack.verify_empty();
    }

    fn apply_unop_b_b(
        &mut self,
        x: &'s (Expr, Span),
        f: impl Fn(bool) -> bool,
    ) -> Result<Value<'s>, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_bool();
        Ok(Value::bool(f(x)))
    }

    fn apply_binop_ii_i(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(i32, i32) -> i32,
    ) -> Result<Value<'s>, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_int();
        let y = self.interp_expr(y)?.unwrap_int();
        Ok(Value::int(f(x, y)))
    }

    fn apply_binop_ii_b(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(i32, i32) -> bool,
    ) -> Result<Value<'s>, Error<'s>> {
        let x = self.interp_expr(x)?.unwrap_int();
        let y = self.interp_expr(y)?.unwrap_int();
        Ok(Value::bool(f(x, y)))
    }

    fn apply_binop_bb_b(
        &mut self,
        x: &'s (Expr, Span),
        y: &'s (Expr, Span),
        f: impl Fn(bool, bool) -> bool,
    ) -> Result<Value<'s>, Error<'s>> {
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

#[track_caller]
fn no_comptime(comptime: bool) {
    if comptime {
        panic!("Interp: leftover comptime")
    }
}
