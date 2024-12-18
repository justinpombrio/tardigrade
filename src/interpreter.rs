use crate::ast::{self, Block, Expr, LetStmt, Span, Value};
use crate::error::Error;
use crate::stack::Stack;
use panfix::Source;

pub struct Interpreter<'s> {
    source: &'s Source,
    stack: Stack<Value<'s>>,
}

impl<'s> Interpreter<'s> {
    pub fn new(source: &'s Source) -> Interpreter<'s> {
        Interpreter {
            source,
            stack: Stack::new(),
        }
    }

    pub fn interp_block(&mut self, block: &'s (Block, Span)) -> Result<Value<'s>, Error<'s>> {
        use ast::Stmt::*;

        let start_of_block = self.stack.start_block();
        let mut result = Value::unit();
        for stmt in &block.0 .0 {
            match &stmt.0 {
                Expr(e) => {
                    result = self.interp_expr(e)?;
                }
                Let(LetStmt { definition, .. }) => {
                    let value = self.interp_expr(definition)?;
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

    /// Interpret the expression, producing either a result or a runtime Error. `expr` must be from
    /// the `Source` that this interpreter was constructed with.
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
            If(e_if, e_then, e_else) => {
                let b = self.interp_expr(e_if)?.unwrap_bool();
                if b {
                    self.interp_expr(e_then)
                } else {
                    self.interp_expr(e_else)
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
