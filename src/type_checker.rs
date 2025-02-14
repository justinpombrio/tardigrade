use crate::ast::{self, Block, Expr, FuncId, FuncRefn, FuncStmt, LetStmt, Span, Stmt, VarRefn};
use crate::error::Error;
use panfix::Source;
use std::fmt;
use std::ops::Deref;

pub struct TypeChecker<'s> {
    source: &'s Source,
    stack: Vec<StackFrame>,
    funcs: Vec<FuncStmt>,
}

#[derive(Debug)]
struct StackFrame {
    vars: Vec<(String, Type)>,
    args: Vec<(String, Type)>,
    funcs: Vec<(String, FuncId, FuncType)>,
    /// (vars.len(), args.len(), funcs.len())
    blocks: Vec<(usize, usize, usize)>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Unit,
    Bool,
    Int,
}

#[derive(Debug, Clone)]
pub struct FuncType {
    params: Vec<Type>,
    return_type: Box<Type>,
}

impl<'s> TypeChecker<'s> {
    pub fn new(source: &'s Source) -> TypeChecker<'s> {
        TypeChecker {
            source,
            stack: Vec::new(),
            funcs: Vec::new(),
        }
    }

    pub fn check_prog(&mut self, block: &mut (Block, Span)) -> Result<Type, Error<'s>> {
        self.stack.push(StackFrame::new());
        let ty = self.check_block(block)?;
        self.stack.pop();
        Ok(ty)
    }

    fn check_block(&mut self, block: &mut (Block, Span)) -> Result<Type, Error<'s>> {
        let mut result = Type::Unit;
        self.frame_mut().start_block();
        let mut remaining_stmts = Vec::new();
        let mut stmts = block.0 .0.drain(..).peekable();
        while let Some((mut stmt, span)) = stmts.next() {
            match &mut stmt {
                Stmt::Expr(ref mut expr) => {
                    let ty = self.check_expr(expr)?;
                    result = ty;
                    remaining_stmts.push((stmt, span));
                }
                Stmt::Let(ref mut let_stmt) => {
                    self.check_let_stmt(let_stmt)?;
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
                    self.check_funcs(contiguous_funcs)?;
                }
            }
        }
        std::mem::drop(stmts);
        block.0 .0.append(&mut remaining_stmts);
        self.frame_mut().end_block();
        Ok(result)
    }

    fn check_funcs(&mut self, funcs: Vec<FuncStmt>) -> Result<(), Error<'s>> {
        // 1. Push the function names so that we can look up references to them.
        //    The id of each function must match its index in `self.funcs` below!
        // TODO: check that function names are disjoint
        for (i, func) in funcs.iter().enumerate() {
            let id = self.funcs.len() + i;
            let ty = type_of_function(func);
            self.frame_mut().push_func(&func.var.name, id, ty);
        }
        // 2. Type check each function in turn, then push it onto the global list of funcs.
        for mut func in funcs {
            self.frame_mut().start_block();
            // TODO: check that function params are disjoint
            for param in &mut func.params {
                self.frame_mut().push_arg(&param.var.name, param.ty.clone());
            }
            self.stack.push(StackFrame::new());
            self.expect_block(&mut func.body, &func.return_type)?;
            self.stack.pop();
            self.frame_mut().end_block();
            self.funcs.push(func);
        }
        Ok(())
    }

    fn check_let_stmt(&mut self, let_stmt: &mut LetStmt) -> Result<(), Error<'s>> {
        let ty = self.check_expr(&mut let_stmt.definition)?;
        self.frame_mut().push_var(&let_stmt.var.name, ty);
        Ok(())
    }

    /// Type check the given expression, which must be from the `Source` that this `TypeChecker`
    /// was cosntructed from.
    pub fn check_expr(&mut self, expr: &mut (Expr, Span)) -> Result<Type, Error<'s>> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        match &mut expr.0 {
            Var(var_refn) => self.check_var_refn(var_refn, expr.1).cloned(),
            Unit => Ok(Type::Unit),
            Bool(_) => Ok(Type::Bool),
            Int(_) => Ok(Type::Int),
            Unop(Not, x) => {
                self.expect_expr(x, &Type::Bool)?;
                Ok(Type::Bool)
            }
            Binop(Add | Sub | Mul | Div, x, y) => {
                self.expect_expr(x, &Type::Int)?;
                self.expect_expr(y, &Type::Int)?;
                Ok(Type::Int)
            }
            Binop(Eq | Ne | Lt | Le | Gt | Ge, x, y) => {
                self.expect_expr(x, &Type::Int)?;
                self.expect_expr(y, &Type::Int)?;
                Ok(Type::Bool)
            }
            Binop(And | Or, x, y) => {
                self.expect_expr(x, &Type::Bool)?;
                self.expect_expr(y, &Type::Bool)?;

                Ok(Type::Bool)
            }
            If(e_if, e_then, e_else) => {
                self.expect_expr(e_if, &Type::Bool)?;
                let t_then = self.check_expr(e_then)?;
                let t_else = self.check_expr(e_else)?;
                if t_then == t_else {
                    Ok(t_else)
                } else {
                    Err(error_branch_mismatch(self.source, &t_then, &t_else, expr.1))
                }
            }
            Apply(func_refn, args) => {
                let func_type = self.check_func_refn(&mut func_refn.0, func_refn.1)?.clone();
                if func_type.params.len() != args.len() {
                    return Err(error_wrong_num_args(
                        self.source,
                        func_type.params.len(),
                        args.len(),
                        expr.1,
                    ));
                }
                for (arg, param) in args.iter_mut().zip(func_type.params.iter()) {
                    self.expect_expr(arg, param)?;
                }
                Ok(func_type.return_type.deref().clone())
            }
            Block(block) => self.check_block(block),
        }
    }

    pub fn finish(self) -> Vec<FuncStmt> {
        if !self.stack.is_empty() {
            panic!("Type Checking: leftover stack frame\n{:?}", self.stack);
        }
        self.funcs
    }

    fn frame_mut(&mut self) -> &mut StackFrame {
        self.stack
            .last_mut()
            .expect("Type Checking: missing stack frame")
    }

    fn expect_block<'t>(
        &mut self,
        block: &'t mut (Block, Span),
        expected: &'t Type,
    ) -> Result<&'t Type, Error<'s>> {
        let actual = self.check_block(block)?;
        if &actual == expected {
            Ok(expected)
        } else {
            Err(error_type_mismatch(self.source, &actual, expected, block.1))
        }
    }

    fn expect_expr<'t>(
        &mut self,
        expr: &'t mut (Expr, Span),
        expected: &'t Type,
    ) -> Result<&'t Type, Error<'s>> {
        let actual = self.check_expr(expr)?;
        if &actual == expected {
            Ok(expected)
        } else {
            Err(error_type_mismatch(self.source, &actual, expected, expr.1))
        }
    }

    fn check_func_refn(
        &self,
        func_refn: &mut FuncRefn,
        span: Span,
    ) -> Result<&FuncType, Error<'s>> {
        for (depth, frame) in self.stack.iter().rev().enumerate() {
            if let Some((id, ty)) = frame.lookup_func(&func_refn.name) {
                func_refn.depth = Some(depth);
                func_refn.id = Some(id);
                return Ok(ty);
            }
        }
        Err(Error::new(
            "Scope Error",
            self.source,
            span,
            "unbound",
            &format!("Function '{}' not found in this scope.", func_refn.name),
        ))
    }

    fn check_var_refn(&self, var_refn: &mut VarRefn, span: Span) -> Result<&Type, Error<'s>> {
        for (depth, frame) in self.stack.iter().rev().enumerate() {
            if let Some((offset, ty)) = frame.lookup_var(&var_refn.name) {
                if offset < 0 {
                    var_refn.depth = Some(depth - 1);
                } else {
                    var_refn.depth = Some(depth);
                }
                var_refn.offset = Some(offset);
                return Ok(ty);
            }
        }
        Err(Error::new(
            "Scope Error",
            self.source,
            span,
            "unbound",
            &format!("Variable '{}' not found in this scope.", var_refn.name),
        ))
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

impl fmt::Display for TypeChecker<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "stack")?;
        for frame in &self.stack {
            write!(f, "{}", frame)?;
        }
        writeln!(f, "end")?;
        writeln!(f, "functions")?;
        for func in &self.funcs {
            writeln!(f, "  {}", func.var.name)?;
        }
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
        for (i, (var, ty)) in self.vars.iter().enumerate() {
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
