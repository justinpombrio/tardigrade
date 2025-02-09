use crate::ast::{Block, Expr, Span, Stmt, VarRefn};
use crate::error::Error;
use crate::stack::Stack;
use panfix::Source;

/// Checks that all variables are bound and checks for cyclic references between values.
pub struct ScopeChecker<'s> {
    source: &'s Source,
    ct_stack: Stack<String>,
    rt_stack: Stack<String>,
    args_per_frame: Vec<usize>,
}

impl<'s> ScopeChecker<'s> {
    pub fn new(source: &'s Source) -> ScopeChecker<'s> {
        ScopeChecker {
            source,
            ct_stack: Stack::new(),
            rt_stack: Stack::new(),
            args_per_frame: Vec::new(),
        }
    }

    pub fn resolve_block(
        &mut self,
        block: &mut (Block, Span),
        comptime: bool,
    ) -> Result<(), Error<'s>> {
        let comptime = comptime | block.0.comptime;
        let start_of_block = self.stack_mut(comptime).start_block();
        let mut stmts = block.0.stmts.iter_mut().peekable();
        while let Some((stmt, _)) = stmts.next() {
            match stmt {
                Stmt::Expr(expr) => self.resolve_expr(expr, comptime)?,
                Stmt::Let(let_stmt) => {
                    let comptime = comptime | let_stmt.comptime;
                    self.resolve_expr(&mut let_stmt.definition, comptime)?;
                    self.stack_mut(comptime).push(let_stmt.var.name.clone());
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
                        let comptime = comptime | func.comptime;
                        self.stack_mut(comptime).push(func.var.name.clone());
                    }
                    for func in &mut contiguous_funcs {
                        let comptime = comptime | func.comptime;
                        // TODO: check that function params are disjoint
                        for param in &mut func.params {
                            self.stack_mut(comptime).push(param.var.name.clone());
                        }
                        self.stack_mut(comptime).start_frame_at_depth_zero();
                        self.args_per_frame.push(func.params.len());
                        self.resolve_block(&mut func.body, comptime)?;
                        self.stack_mut(comptime).end_frame(func.params.len());
                        self.args_per_frame.pop();
                    }
                }
            }
        }
        self.stack_mut(comptime).end_block(start_of_block);
        Ok(())
    }

    pub fn resolve_expr(
        &mut self,
        expr: &mut (Expr, Span),
        comptime: bool,
    ) -> Result<(), Error<'s>> {
        match &mut expr.0 {
            Expr::Var(var) => self.lookup(var, expr.1, comptime),
            Expr::Unit | Expr::Bool(_) | Expr::Int(_) => Ok(()),
            Expr::Unop(_, expr) => self.resolve_expr(expr, comptime),
            Expr::Binop(_, e_1, e_2) => {
                self.resolve_expr(e_1, comptime)?;
                self.resolve_expr(e_2, comptime)
            }
            Expr::If(if_expr) => {
                self.resolve_expr(&mut if_expr.e_if, comptime | if_expr.comptime)?;
                self.resolve_expr(&mut if_expr.e_then, comptime)?;
                self.resolve_expr(&mut if_expr.e_else, comptime)
            }
            Expr::Apply(func, args) => {
                self.lookup(&mut func.0, func.1, comptime)?;
                for arg in args {
                    self.resolve_expr(arg, comptime)?;
                }
                Ok(())
            }
            Expr::Block(block) => self.resolve_block(block, comptime),
            Expr::Comptime(expr) => self.resolve_expr(expr, true),
        }
    }

    pub fn finish(&self) {
        self.ct_stack.verify_empty();
        self.rt_stack.verify_empty();
    }

    fn lookup(&self, var_refn: &mut VarRefn, span: Span, comptime: bool) -> Result<(), Error<'s>> {
        let result = if let Some(refn) = self
            .stack(comptime | var_refn.comptime)
            .find_refn_slowly(&self.args_per_frame, |var| var == &var_refn.name)
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

    fn stack(&self, comptime: bool) -> &Stack<String> {
        match comptime {
            true => &self.ct_stack,
            false => &self.rt_stack,
        }
    }

    fn stack_mut(&mut self, comptime: bool) -> &mut Stack<String> {
        match comptime {
            true => &mut self.ct_stack,
            false => &mut self.rt_stack,
        }
    }
}
