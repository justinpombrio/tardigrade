use crate::ast::{Block, Expr, Span, Stmt, VarRefn};
use crate::error::Error;
use crate::stack::Stack;
use panfix::Source;

/// Checks that all variables are bound and checks for cyclic references between values.
pub struct ScopeChecker<'s> {
    source: &'s Source,
    stack: Stack<String>,
    args_per_frame: Vec<usize>,
}

impl<'s> ScopeChecker<'s> {
    pub fn new(source: &'s Source) -> ScopeChecker<'s> {
        ScopeChecker {
            source,
            stack: Stack::new(),
            args_per_frame: Vec::new(),
        }
    }

    pub fn resolve_block(&mut self, block: &mut (Block, Span)) -> Result<(), Error<'s>> {
        let start_of_block = self.stack.start_block();
        let mut stmts = block.0 .0.iter_mut().peekable();
        while let Some((stmt, _)) = stmts.next() {
            match stmt {
                Stmt::Expr(expr) => self.resolve_expr(expr)?,
                Stmt::Let(let_stmt) => {
                    self.resolve_expr(&mut let_stmt.definition)?;
                    self.stack.push(let_stmt.var.name.clone());
                }
                Stmt::Func(func) => {
                    let mut contiguous_funcs = {
                        let mut funcs = vec![func];
                        while matches!(stmts.peek(), Some((Stmt::Func(_), _))) {
                            match stmts.next() {
                                Some((Stmt::Func(func), _)) => funcs.push(func),
                                _ => unreachable!(),
                            }
                        }
                        funcs
                    };
                    // TODO: check that function names are disjoint
                    for func in &mut contiguous_funcs {
                        self.stack.push(func.var.name.clone());
                    }
                    for func in &mut contiguous_funcs {
                        // TODO: check that function params are disjoint
                        for param in &mut func.params {
                            self.stack.push(param.var.name.clone());
                        }
                        self.stack.start_frame_at_depth_zero();
                        self.args_per_frame.push(func.params.len());
                        self.resolve_block(&mut func.body)?;
                        self.stack.end_frame(func.params.len());
                        self.args_per_frame.pop();
                    }
                }
            }
        }
        self.stack.end_block(start_of_block);
        Ok(())
    }

    pub fn finish(&self) {
        self.stack.verify_empty();
    }

    pub fn resolve_expr(&mut self, expr: &mut (Expr, Span)) -> Result<(), Error<'s>> {
        match &mut expr.0 {
            Expr::Var(var) => self.lookup(var, expr.1),
            Expr::Unit | Expr::Bool(_) | Expr::Int(_) => Ok(()),
            Expr::Unop(_, expr) => self.resolve_expr(expr),
            Expr::Binop(_, e_1, e_2) => {
                self.resolve_expr(e_1)?;
                self.resolve_expr(e_2)
            }
            Expr::If(e_cond, e_then, e_else) => {
                self.resolve_expr(e_cond)?;
                self.resolve_expr(e_then)?;
                self.resolve_expr(e_else)
            }
            Expr::Apply(func, args) => {
                self.lookup(&mut func.0, func.1)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
                Ok(())
            }
            Expr::Block(block) => self.resolve_block(block),
        }
    }

    fn lookup(&self, var_refn: &mut VarRefn, span: Span) -> Result<(), Error<'s>> {
        let result = if let Some(refn) = self
            .stack
            .find_refn_slowly(&self.args_per_frame, |name| name == &var_refn.name)
        {
            var_refn.refn = Some(refn);
            Ok(())
        } else {
            Err(Error::new(
                "Scope Error",
                self.source,
                span,
                "unbound",
                &format!("Variable '{}' not found in this scope.", var_refn.name),
            ))
        };
        result
    }
}
