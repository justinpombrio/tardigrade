use crate::ast::{self, Block, Expr, FuncStmt, Span, Stmt};
use crate::error::Error;
use crate::stack::Stack;
use panfix::Source;
use std::fmt;
use std::ops::Deref;

#[derive(Debug, Clone)]
enum ValueOrFuncType {
    Value(Type),
    Func(FuncType),
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

pub struct TypeChecker<'s> {
    source: &'s Source,
    stack: Stack<ValueOrFuncType>,
}

impl<'s> TypeChecker<'s> {
    pub fn new(source: &'s Source) -> TypeChecker<'s> {
        TypeChecker {
            source,
            stack: Stack::new(),
        }
    }

    pub fn check_block(&mut self, block: &(Block, Span)) -> Result<Type, Error<'s>> {
        use ast::Stmt::*;

        let mut result = Type::Unit;
        let start_of_block = self.stack.start_block();
        let mut stmts = block.0 .0.iter().peekable();
        while let Some((stmt, _)) = stmts.next() {
            match &stmt {
                Expr(expr) => result = self.check_expr(expr)?,
                Let(let_stmt) => {
                    let ty = self.check_expr(&let_stmt.definition)?;
                    self.stack.push(ValueOrFuncType::Value(ty));
                }
                Func(func) => {
                    let contiguous_funcs = {
                        let mut funcs = vec![func];
                        while matches!(stmts.peek(), Some((Stmt::Func(_), _))) {
                            match stmts.next() {
                                Some((Stmt::Func(func), _)) => funcs.push(func),
                                _ => unreachable!(),
                            }
                        }
                        funcs
                    };
                    for func in &contiguous_funcs {
                        let ty = type_of_function(func);
                        self.stack.push(ValueOrFuncType::Func(ty));
                    }
                    for func in contiguous_funcs {
                        for param in &func.params {
                            self.stack.push(ValueOrFuncType::Value(param.ty.clone()));
                        }
                        self.stack.start_frame_at_depth_zero();
                        self.expect_block(&func.body, &func.return_type)?;
                        self.stack.end_frame(func.params.len());
                    }
                }
            }
        }
        self.stack.end_block(start_of_block);
        Ok(result)
    }

    /// Type check the given expression, which must be from the `Source` that this `TypeChecker`
    /// was cosntructed from.
    pub fn check_expr(&mut self, expr: &(Expr, Span)) -> Result<Type, Error<'s>> {
        use ast::Binop::*;
        use ast::Unop::*;
        use Expr::*;

        match &expr.0 {
            Var(var_refn) => {
                let ty = self.stack.lookup(var_refn.refn());
                expect_value(self.source, ty, expr.1).cloned()
            }
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
            Apply(var_refn, args) => {
                let ty = self.stack.lookup(var_refn.0.refn()).clone();
                let func_type = expect_func(self.source, &ty, var_refn.1)?;
                if func_type.params.len() != args.len() {
                    return Err(error_wrong_num_args(
                        self.source,
                        func_type.params.len(),
                        args.len(),
                        expr.1,
                    ));
                }
                for (arg, param) in args.iter().zip(func_type.params.iter()) {
                    self.expect_expr(arg, param)?;
                }
                Ok(func_type.return_type.deref().clone())
            }
            Block(block) => self.check_block(block),
        }
    }

    pub fn finish(&self) {
        self.stack.verify_empty();
    }

    fn expect_block<'t>(
        &mut self,
        block: &'t (Block, Span),
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
        expr: &'t (Expr, Span),
        expected: &'t Type,
    ) -> Result<&'t Type, Error<'s>> {
        let actual = self.check_expr(expr)?;
        if &actual == expected {
            Ok(expected)
        } else {
            Err(error_type_mismatch(self.source, &actual, expected, expr.1))
        }
    }
}

fn expect_value<'s, 't>(
    source: &'s Source,
    ty: &'t ValueOrFuncType,
    span: Span,
) -> Result<&'t Type, Error<'s>> {
    match ty {
        ValueOrFuncType::Value(value_type) => Ok(value_type),
        ValueOrFuncType::Func(_) => Err(error_expected_value(source, span)),
    }
}

fn expect_func<'s, 't>(
    source: &'s Source,
    ty: &'t ValueOrFuncType,
    span: Span,
) -> Result<&'t FuncType, Error<'s>> {
    match ty {
        ValueOrFuncType::Func(func_type) => Ok(func_type),
        ValueOrFuncType::Value(value_type) => {
            Err(error_expected_function(source, value_type, span))
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

fn error_expected_value(source: &Source, span: Span) -> Error {
    Error::new(
        "Type Error",
        source,
        span,
        "this is a function",
        "Expected a value, but found a function",
    )
}

fn error_expected_function<'s>(source: &'s Source, actual: &Type, span: Span) -> Error<'s> {
    Error::new(
        "Type Error",
        source,
        span,
        "expected function",
        &format!("Expected function but found type {}.", actual),
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

impl fmt::Display for ValueOrFuncType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueOrFuncType::*;

        match self {
            Value(ty) => write!(f, "{}", ty),
            Func(ty) => write!(f, "{}", ty),
        }
    }
}
