use crate::ast::{
    self, ApplyExpr, Block, Expr, FuncId, FuncRefn, FuncStmt, IfExpr, LetStmt, Literal, Span, Stmt,
    Time, VarRefn,
};
use crate::error::Error;
use crate::logger::{Logger, Verbosity};
use crate::{log, span};
use panfix::Source;
use std::fmt;
use std::ops::Deref;

use Time::{Comptime, Runtime};

pub struct TypeChecker<'s, 'l> {
    source: &'s Source,
    ct_stack: Vec<StackFrame>,
    rt_stack: Vec<StackFrame>,
    ct_funcs: Vec<Option<FuncStmt>>,
    rt_funcs: Vec<Option<FuncStmt>>,
    logger: &'l mut Logger,
}

#[derive(Debug)]
struct StackFrame {
    vars: Vec<(String, Type)>,
    args: Vec<(String, Type)>,
    funcs: Vec<(String, FuncId, FuncType)>,
    /// (vars.len(), args.len(), funcs.len())
    blocks: Vec<(usize, usize, usize)>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Never,
    // If extended, update unify() and matches()
}

impl Type {
    fn unify(&self, other: &Type) -> Option<Type> {
        use Type::*;

        match (self, other) {
            (Never, _) | (_, Never) => Some(Never),
            (Unit, Unit) => Some(Unit),
            (Bool, Bool) => Some(Bool),
            (Int, Int) => Some(Int),
            (_, _) => None,
        }
    }

    fn matches(&self, expected_type: &Type) -> bool {
        use Type::*;

        match (self, expected_type) {
            (Never, _) => true,
            (Unit, Unit) => true,
            (Bool, Bool) => true,
            (Int, Int) => true,
            (_, _) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncType {
    params: Vec<Type>,
    return_type: Box<Type>,
}

#[derive(Clone, Copy)]
struct Context<'a> {
    return_type: Option<&'a Type>,
    // In the future: block for break/continue support
}

impl<'a> Context<'a> {
    fn empty() -> Context<'a> {
        Context { return_type: None }
    }

    fn with_return_type(self, return_type: &'a Type) -> Context<'a> {
        Context {
            return_type: Some(return_type),
        }
    }
}

impl<'s, 'l> TypeChecker<'s, 'l> {
    pub fn new(source: &'s Source, logger: &'l mut Logger) -> TypeChecker<'s, 'l> {
        TypeChecker {
            source,
            ct_stack: Vec::new(),
            rt_stack: Vec::new(),
            ct_funcs: Vec::new(),
            rt_funcs: Vec::new(),
            logger,
        }
    }

    pub fn check_prog(&mut self, block: &mut (Block, Span)) -> Result<Type, Error<'s>> {
        span!(self.logger, Trace, "typecheck", {
            span!(self.logger, Trace, "program", {
                log!(self.logger, Trace, &block.0);
            });
            self.ct_stack.push(StackFrame::new());
            self.rt_stack.push(StackFrame::new());
            let ty = self.check_block(block, Runtime, Context::empty())?;
            self.ct_stack.pop();
            self.rt_stack.pop();
            Ok(ty)
        })
    }

    fn check_block(
        &mut self,
        block: &mut (Block, Span),
        time: Time,
        ctx: Context,
    ) -> Result<Type, Error<'s>> {
        span!(self.logger, Trace, "block", {
            let ty = self.check_block_impl(block, time, ctx)?;
            log!(self.logger, Trace, &block.0);
            log!(self.logger, Trace, "type", ("{}", ty));
            Ok(ty)
        })
    }

    fn check_block_impl(
        &mut self,
        block: &mut (Block, Span),
        time: Time,
        ctx: Context,
    ) -> Result<Type, Error<'s>> {
        let mut result = Type::Unit;
        self.frame_mut(time).start_block();
        let mut remaining_stmts = Vec::new();
        let mut stmts = block.0 .0.drain(..).peekable();
        while let Some((mut stmt, span)) = stmts.next() {
            match &mut stmt {
                Stmt::Expr(ref mut expr) => {
                    let ty = self.check_expr(expr, time, ctx)?;
                    result = ty;
                    remaining_stmts.push((stmt, span));
                }
                Stmt::Let(ref mut let_stmt) => {
                    self.check_let_stmt(let_stmt, time, ctx)?;
                    remaining_stmts.push((stmt, span));
                }
                Stmt::Func(_) => {
                    let contiguous_funcs = {
                        let func = match stmt {
                            Stmt::Func(func) => func,
                            _ => unreachable!(),
                        };
                        let mut funcs = vec![func];
                        while matches!(stmts.peek(), Some((Stmt::Func(_), _))) {
                            match stmts.next() {
                                Some((Stmt::Func(func), _)) => {
                                    funcs.push(func);
                                }
                                _ => unreachable!(),
                            }
                        }
                        funcs
                    };
                    self.check_funcs(contiguous_funcs, time, ctx)?;
                }
            }
        }
        std::mem::drop(stmts);
        block.0 .0.append(&mut remaining_stmts);
        self.frame_mut(time).end_block();
        Ok(result)
    }

    fn check_funcs(
        &mut self,
        funcs: Vec<FuncStmt>,
        time: Time,
        ctx: Context,
    ) -> Result<(), Error<'s>> {
        // A function's id is its index into `self.funcs`.
        // Assigning these ids is tricky because other functions may be given ids while we're type
        // checking the current function.
        let mut func_ids = Vec::new();

        // TODO: check that function names are disjoint
        for func in &funcs {
            let time = time + func.time;
            let id = self.funcs(time).len();
            func_ids.push(id);
            self.funcs_mut(time).push(None);
            let ty = type_of_function(func);
            self.frame_mut(time).push_func(&func.var.name, id, ty);
        }
        for (id, func) in func_ids.into_iter().zip(funcs.into_iter()) {
            self.check_func_stmt(func, id, time, ctx)?;
        }
        Ok(())
    }

    fn check_let_stmt(
        &mut self,
        let_stmt: &mut LetStmt,
        time: Time,
        ctx: Context,
    ) -> Result<(), Error<'s>> {
        span!(self.logger, Trace, "let", ("{}", let_stmt.var.name), {
            self.check_let_stmt_impl(let_stmt, time, ctx)
        })
    }

    fn check_let_stmt_impl(
        &mut self,
        let_stmt: &mut LetStmt,
        time: Time,
        ctx: Context,
    ) -> Result<(), Error<'s>> {
        let time = time + let_stmt.time;
        let ty = self.check_expr(&mut let_stmt.definition, time, ctx)?;
        self.frame_mut(time).push_var(&let_stmt.var.name, ty);
        Ok(())
    }

    fn check_func_stmt(
        &mut self,
        mut func: FuncStmt,
        id: FuncId,
        time: Time,
        ctx: Context,
    ) -> Result<(), Error<'s>> {
        span!(self.logger, Trace, "func", ("{}", func.var.name), {
            let time = time + func.time;
            self.frame_mut(time).start_block();
            // TODO: check that function params are disjoint
            for param in &mut func.params {
                self.frame_mut(time)
                    .push_arg(&param.var.name, param.ty.clone());
            }
            self.stack_mut(time).push(StackFrame::new());
            self.expect_block(
                &mut func.body,
                &func.return_type,
                time,
                ctx.with_return_type(&func.return_type),
            )?;
            self.stack_mut(time).pop();
            self.frame_mut(time).end_block();
            self.funcs_mut(time)[id] = Some(func);
            log!(self.logger, Trace, "id", ("{}", id));
            Ok(())
        })
    }

    /// Type check the given expression, which must be from the `Source` that this `TypeChecker`
    /// was cosntructed from.
    fn check_expr(
        &mut self,
        expr: &mut (Expr, Span),
        time: Time,
        ctx: Context,
    ) -> Result<Type, Error<'s>> {
        if self.logger.enabled(Verbosity::Trace) {
            if let (Expr::Literal(literal), _) = expr {
                let ty = self.check_literal(literal);
                log!(self.logger, Trace, "literal", ("{} : {}", literal, ty));
                Ok(ty)
            } else if let (Expr::Var(var), span) = expr {
                let ty = self.check_var_refn(var, *span, time)?;
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
                Ok(ty)
            } else {
                span!(self.logger, Trace, "expr", {
                    let ty = self.check_expr_impl(expr, time, ctx)?;
                    log!(self.logger, Trace, &expr.0);
                    log!(self.logger, Trace, "type", ("{}", ty));
                    Ok(ty)
                })
            }
        } else {
            self.check_expr_impl(expr, time, ctx)
        }
    }

    fn check_expr_impl(
        &mut self,
        expr: &mut (Expr, Span),
        time: Time,
        ctx: Context,
    ) -> Result<Type, Error<'s>> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        let span = expr.1;
        match &mut expr.0 {
            Var(var_refn) => self.check_var_refn(var_refn, span, time),
            Literal(literal) => Ok(self.check_literal(literal)),
            Unop(Not, x) => {
                self.expect_expr(x, &Type::Bool, time, ctx)?;
                Ok(Type::Bool)
            }
            Binop(Add | Sub | Mul | Div, x, y) => {
                self.expect_expr(x, &Type::Int, time, ctx)?;
                self.expect_expr(y, &Type::Int, time, ctx)?;
                Ok(Type::Int)
            }
            Binop(Eq | Ne | Lt | Le | Gt | Ge, x, y) => {
                self.expect_expr(x, &Type::Int, time, ctx)?;
                self.expect_expr(y, &Type::Int, time, ctx)?;
                Ok(Type::Bool)
            }
            Binop(And | Or, x, y) => {
                self.expect_expr(x, &Type::Bool, time, ctx)?;
                self.expect_expr(y, &Type::Bool, time, ctx)?;

                Ok(Type::Bool)
            }
            If(if_expr) => self.check_if_expr(if_expr, span, time, ctx),
            Apply(apply_expr) => self.check_apply_expr(apply_expr, span, time, ctx),
            Block(block_expr) => {
                self.check_block(&mut block_expr.block, time + block_expr.time, ctx)
            }
            ComptimeExpr(expr) => self.check_expr(expr, Comptime, ctx),
            Return(expr) => {
                if let Some(return_ty) = &ctx.return_type {
                    self.expect_expr(expr, return_ty, time, ctx)?;
                    Ok(Type::Never)
                } else {
                    Err(error_return_outside_of_func(self.source, span))
                }
            }
        }
    }

    fn check_if_expr(
        &mut self,
        if_expr: &mut IfExpr,
        span: Span,
        time: Time,
        ctx: Context,
    ) -> Result<Type, Error<'s>> {
        let time = time + if_expr.time;
        self.expect_expr(&mut if_expr.e_if, &Type::Bool, time, ctx)?;
        let t_then = self.check_expr(&mut if_expr.e_then, time, ctx)?;
        let t_else = self.check_expr(&mut if_expr.e_else, time, ctx)?;
        if let Some(t_result) = t_then.unify(&t_else) {
            Ok(t_result)
        } else {
            Err(error_branch_mismatch(self.source, &t_then, &t_else, span))
        }
    }

    fn check_apply_expr(
        &mut self,
        apply_expr: &mut ApplyExpr,
        span: Span,
        time: Time,
        ctx: Context,
    ) -> Result<Type, Error<'s>> {
        let time = time + apply_expr.time;
        let func_type = self
            .check_func_refn(&mut apply_expr.func.0, apply_expr.func.1, time)?
            .clone();
        if func_type.params.len() != apply_expr.args.len() {
            return Err(error_wrong_num_args(
                self.source,
                func_type.params.len(),
                apply_expr.args.len(),
                span,
            ));
        }
        for (arg, param) in apply_expr.args.iter_mut().zip(func_type.params.iter()) {
            self.expect_expr(arg, param, time, ctx)?;
        }
        Ok(func_type.return_type.deref().clone())
    }

    fn check_literal(&mut self, literal: &Literal) -> Type {
        use Literal::*;

        match literal {
            Unit => Type::Unit,
            Bool(_) => Type::Bool,
            Int(_) => Type::Int,
        }
    }

    fn expect_expr<'t>(
        &mut self,
        expr: &'t mut (Expr, Span),
        expected: &'t Type,
        time: Time,
        ctx: Context,
    ) -> Result<&'t Type, Error<'s>> {
        let actual = self.check_expr(expr, time, ctx)?;
        if actual.matches(expected) {
            Ok(expected)
        } else {
            Err(error_type_mismatch(self.source, &actual, expected, expr.1))
        }
    }

    fn check_func_refn(
        &mut self,
        func: &mut FuncRefn,
        span: Span,
        time: Time,
    ) -> Result<FuncType, Error<'s>> {
        let ty = self.check_func_refn_impl(func, span, time)?.clone();
        log!(
            self.logger,
            Trace,
            "func",
            (
                "{} id:{} depth:{}",
                func.name,
                func.unwrap_id(),
                func.unwrap_depth()
            )
        );
        Ok(ty)
    }

    fn check_func_refn_impl(
        &mut self,
        func_refn: &mut FuncRefn,
        span: Span,
        time: Time,
    ) -> Result<&FuncType, Error<'s>> {
        for (depth, frame) in self.stack(time).iter().rev().enumerate() {
            if let Some((id, ty)) = frame.lookup_func(&func_refn.name) {
                func_refn.depth = Some(depth);
                func_refn.id = Some(id);
                return Ok(ty);
            }
        }
        let prefix = match time {
            Comptime => "#",
            Runtime => "",
        };
        Err(Error::new(
            "Scope Error",
            self.source,
            span,
            "unbound",
            &format!(
                "Function {}{} not found in this scope.",
                prefix, func_refn.name
            ),
        ))
    }

    fn check_var_refn(
        &mut self,
        var_refn: &mut VarRefn,
        span: Span,
        time: Time,
    ) -> Result<Type, Error<'s>> {
        let time = time + var_refn.time;
        for (depth, frame) in self.stack(time).iter().rev().enumerate() {
            if let Some((offset, ty)) = frame.lookup_var(&var_refn.name) {
                if offset < 0 {
                    var_refn.depth = Some(depth - 1);
                } else {
                    var_refn.depth = Some(depth);
                }
                var_refn.offset = Some(offset);
                return Ok(ty.clone());
            }
        }
        let prefix = match time {
            Comptime => "#",
            Runtime => "",
        };
        Err(Error::new(
            "Scope Error",
            self.source,
            span,
            "unbound",
            &format!(
                "Variable {}{} not found in this scope.",
                prefix, var_refn.name
            ),
        ))
    }

    /// Perform some sanity checks, then return a pair of all
    /// `(comptime_funcs, runtime_funcs)`, which have been pulled out of the AST.
    pub fn finish(self) -> (Vec<FuncStmt>, Vec<FuncStmt>) {
        fn unwrap_opts<T>(v: Vec<Option<T>>) -> Vec<T> {
            v.into_iter().map(|opt| opt.unwrap()).collect::<Vec<_>>()
        }

        if !self.ct_stack.is_empty() {
            panic!(
                "Type Checking: leftover comptime stack frame\n{:?}",
                self.ct_stack
            );
        }
        if !self.rt_stack.is_empty() {
            panic!(
                "Type Checking: leftover runtime stack frame\n{:?}",
                self.rt_stack
            );
        }

        (unwrap_opts(self.ct_funcs), unwrap_opts(self.rt_funcs))
    }

    fn funcs(&self, time: Time) -> &Vec<Option<FuncStmt>> {
        match time {
            Comptime => &self.ct_funcs,
            Runtime => &self.rt_funcs,
        }
    }

    fn funcs_mut(&mut self, time: Time) -> &mut Vec<Option<FuncStmt>> {
        match time {
            Comptime => &mut self.ct_funcs,
            Runtime => &mut self.rt_funcs,
        }
    }

    fn stack(&self, time: Time) -> &Vec<StackFrame> {
        match time {
            Comptime => &self.ct_stack,
            Runtime => &self.rt_stack,
        }
    }

    fn stack_mut(&mut self, time: Time) -> &mut Vec<StackFrame> {
        match time {
            Comptime => &mut self.ct_stack,
            Runtime => &mut self.rt_stack,
        }
    }

    fn frame_mut(&mut self, time: Time) -> &mut StackFrame {
        self.stack_mut(time)
            .last_mut()
            .expect("Type Checking: missing stack frame")
    }

    fn expect_block<'t>(
        &mut self,
        block: &'t mut (Block, Span),
        expected: &'t Type,
        time: Time,
        ctx: Context,
    ) -> Result<&'t Type, Error<'s>> {
        let actual = self.check_block(block, time, ctx)?;
        if actual.matches(expected) {
            Ok(expected)
        } else {
            Err(error_type_mismatch(self.source, &actual, expected, block.1))
        }
    }
}

fn error_type_mismatch<'s>(
    source: &'s Source,
    actual: &Type,
    expected: &Type,
    span: Span,
) -> Error<'s> {
    Error::new(
        "Type Error",
        source,
        span,
        &format!("expected {}", expected),
        &format!("Expected type {} but found type {}.", expected, actual),
    )
}

fn error_branch_mismatch<'s>(
    source: &'s Source,
    type_1: &Type,
    type_2: &Type,
    span: Span,
) -> Error<'s> {
    Error::new(
        "Type Error",
        source,
        span,
        "branches have different types",
        &format!(
            "Expected branches to have the same type, but found type {} and type {}.",
            type_1, type_2
        ),
    )
}

fn error_wrong_num_args(source: &Source, expected: usize, actual: usize, span: Span) -> Error {
    Error::new(
        "Type Error",
        source,
        span,
        &format!("expected {} args", expected),
        &format!(
            "Expected {} arguments, but found {} arguments.",
            expected, actual
        ),
    )
}

fn error_return_outside_of_func(source: &Source, span: Span) -> Error {
    Error::new(
        "Syntax Error",
        source,
        span,
        "not inside a function",
        "Return statement found outside of any function, but it's only meaningful inside of a function."
    )
}

fn type_of_function(func: &FuncStmt) -> FuncType {
    FuncType {
        params: func.params.iter().map(|param| param.ty.clone()).collect(),
        return_type: Box::new(func.return_type.clone()),
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Type::*;

        match self {
            Unit => write!(f, "()"),
            Bool => write!(f, "Bool"),
            Int => write!(f, "Int"),
            Never => write!(f, "Never"),
        }
    }
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<function>")
    }
}

impl fmt::Display for StackFrame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "  stack frame")?;
        writeln!(f, "    vars: {:?}", self.vars)?;
        writeln!(f, "    args: {:?}", self.args)?;
        writeln!(f, "    funcs: {:?}", self.funcs)?;
        writeln!(f, "    blocks: {:?}", self.blocks)?;
        writeln!(f, "  end")
    }
}

impl fmt::Display for TypeChecker<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn show_stack_and_funcs(
            f: &mut fmt::Formatter,
            stack: &Vec<StackFrame>,
            funcs: &Vec<Option<FuncStmt>>,
        ) -> fmt::Result {
            writeln!(f, "  stack")?;
            for frame in stack {
                write!(f, "    {}", frame)?;
            }
            writeln!(f, "  end")?;
            writeln!(f, "  functions")?;
            for func in funcs {
                if let Some(func) = func {
                    writeln!(f, "    {}", func.var.name)?;
                } else {
                    writeln!(f, "    None")?;
                }
            }
            writeln!(f, "  end")
        }

        writeln!(f, "comptime")?;
        show_stack_and_funcs(f, &self.ct_stack, &self.ct_funcs)?;
        writeln!(f, "end")?;
        writeln!(f, "runtime")?;
        show_stack_and_funcs(f, &self.rt_stack, &self.rt_funcs)?;
        writeln!(f, "end")
    }
}

impl StackFrame {
    fn new() -> StackFrame {
        StackFrame {
            vars: Vec::new(),
            args: Vec::new(),
            funcs: Vec::new(),
            blocks: Vec::new(),
        }
    }

    fn push_var(&mut self, var_name: &str, ty: Type) {
        self.vars.push((var_name.to_owned(), ty));
    }

    fn push_arg(&mut self, arg_name: &str, ty: Type) {
        self.args.push((arg_name.to_owned(), ty));
    }

    fn push_func(&mut self, func_name: &str, func_id: FuncId, ty: FuncType) {
        self.funcs.push((func_name.to_owned(), func_id, ty))
    }

    fn lookup_var(&self, var_name: &str) -> Option<(isize, &Type)> {
        for (i, (arg, ty)) in self.args.iter().rev().enumerate() {
            if *arg == var_name {
                return Some((-(i as isize) - 3, ty));
            }
        }
        for (i, (var, ty)) in self.vars.iter().enumerate().rev() {
            if *var == var_name {
                return Some((i as isize, ty));
            }
        }
        None
    }

    fn lookup_func(&self, func_name: &str) -> Option<(FuncId, &FuncType)> {
        for (func, id, ty) in self.funcs.iter().rev() {
            if *func == func_name {
                return Some((*id, ty));
            }
        }
        None
    }

    fn start_block(&mut self) {
        self.blocks
            .push((self.vars.len(), self.args.len(), self.funcs.len()));
    }

    fn end_block(&mut self) {
        let (vars_len, args_len, funcs_len) = self.blocks.pop().unwrap();
        self.vars.truncate(vars_len);
        self.args.truncate(args_len);
        self.funcs.truncate(funcs_len);
    }
}
