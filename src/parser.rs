//! Parse an AST out of a token tree. Error on redundant nested comptimes.

use crate::ast::{
    ApplyExpr, Block, BlockExpr, Expr, FuncRefn, FuncStmt, IfExpr, LetStmt, Literal, Param, Span,
    Stmt, Time, Var, VarRefn,
};
use crate::error::Error;
use crate::grammar::{construct_grammar, ExprToken, LiteralToken, StmtToken, Token, TypeToken};
use crate::logger::Logger;
use crate::type_checker::Type;
use crate::{log, span};
use panfix::{Parser as PanfixParser, Source, Visitor};

use Time::{Comptime, Runtime};

pub struct Parser(PanfixParser);

impl Parser {
    pub fn new() -> Parser {
        Parser(construct_grammar())
    }

    pub fn parse<'s>(
        &self,
        source: &'s Source,
        logger: &mut Logger,
    ) -> Result<(Block, Span), Error<'s>> {
        let tree = self.0.parse(source)?;
        span!(logger, Trace, "source", {
            log!(logger, Trace, &source.source());
        });
        span!(logger, Trace, "parse_tree", {
            log_parse_tree(tree.visitor(), logger);
        });
        self.parse_block_with_span(tree.visitor(), Runtime)
    }

    fn parse_block_with_span<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
    ) -> Result<(Block, Span), Error<'s>> {
        let block = self.parse_block(v, time)?;
        Ok((block, v.span()))
    }

    fn parse_block<'s>(&self, mut v: Visitor<'s, '_, '_>, time: Time) -> Result<Block, Error<'s>> {
        let mut stmts = Vec::new();
        loop {
            let token = self.token(v);
            match token {
                Token::Blank => break,
                Token::Juxtapose => {
                    stmts.push(self.parse_stmt_with_span(v.child(0), time)?);
                    v = v.child(1);
                }
                Token::Stmt(_) | Token::Expr(_) => {
                    stmts.push(self.parse_stmt_with_span(v, time)?);
                    break;
                }
                _ => return Err(self.error_expected(v, "statement")),
            }
        }
        Ok(Block(stmts))
    }

    fn parse_stmt_with_span<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
    ) -> Result<(Stmt, Span), Error<'s>> {
        match self.token(v) {
            Token::Expr(token) => {
                let expr = self.parse_expr(v, token, time)?;
                let span = v.span();
                Ok((Stmt::Expr((expr, span)), span))
            }
            Token::Stmt(token) => {
                let stmt = self.parse_stmt(v, token, time)?;
                Ok((stmt, v.span()))
            }
            _ => Err(self.error_expected(v, "expression")),
        }
    }

    fn parse_stmt<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        token: StmtToken,
        time: Time,
    ) -> Result<Stmt, Error<'s>> {
        match token {
            StmtToken::Let => Ok(Stmt::Let(self.parse_let(v, time, Runtime)?)),
            StmtToken::LetCT => Ok(Stmt::Let(self.parse_let(v, time, Comptime)?)),
            StmtToken::Func => Ok(Stmt::Func(self.parse_func(v, time, Runtime)?)),
            StmtToken::FuncCT => Ok(Stmt::Func(self.parse_func(v, time, Comptime)?)),
        }
    }

    fn parse_let<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
        let_time: Time,
    ) -> Result<LetStmt, Error<'s>> {
        let time = self.combine_times(v, time, let_time)?;
        let var = self.parse_var(v.child(0))?;
        let definition = self.parse_expr_with_span(v.child(1), time)?;
        Ok(LetStmt {
            var,
            definition,
            time: let_time,
        })
    }

    fn parse_func<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
        func_time: Time,
    ) -> Result<FuncStmt, Error<'s>> {
        let time = self.combine_times(v, time, func_time)?;
        let var = self.parse_var(v.child(0))?;
        let params = self.parse_params(v.child(1))?;
        let return_type = self.parse_return_type(v.child(2))?;
        let body = self.parse_block_with_span(v.child(3), time)?;
        Ok(FuncStmt {
            var,
            params,
            return_type,
            body,
            time: func_time,
        })
    }

    fn parse_return_type<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<Type, Error<'s>> {
        match self.token(v) {
            Token::Blank => Ok(Type::Unit),
            Token::Arrow => self.parse_type(v.child(0)),
            _ => Err(self.error_expected(v, "return type annotation")),
        }
    }

    fn parse_params<'s>(&self, mut v: Visitor<'s, '_, '_>) -> Result<Vec<Param>, Error<'s>> {
        let mut params = Vec::new();
        while self.token(v) == Token::Comma {
            params.push(self.parse_param(v.child(0))?);
            v = v.child(1);
        }
        if self.token(v) != Token::Blank {
            params.push(self.parse_param(v)?);
        }
        Ok(params)
    }

    fn parse_param<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<Param, Error<'s>> {
        match self.token(v) {
            Token::Colon => {
                let var = self.parse_var(v.child(0))?;
                let ty = self.parse_type(v.child(1))?;
                Ok(Param { var, ty })
            }
            _ => Err(self.error_expected(v, "function parameter")),
        }
    }

    fn parse_args<'s>(
        &self,
        mut v: Visitor<'s, '_, '_>,
        time: Time,
    ) -> Result<Vec<(Expr, Span)>, Error<'s>> {
        let mut args = Vec::new();
        while self.token(v) == Token::Comma {
            args.push(self.parse_expr_with_span(v.child(0), time)?);
            v = v.child(1);
        }
        if self.token(v) != Token::Blank {
            args.push(self.parse_expr_with_span(v, time)?);
        }
        Ok(args)
    }

    fn parse_refn<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        expected_msg: &str,
        time: Time,
    ) -> Result<(&'s str, Time), Error<'s>> {
        match self.token(v) {
            Token::Expr(ExprToken::Var) => {
                self.combine_times(v, time, Runtime)?;
                Ok((v.source(), Runtime))
            }
            Token::Expr(ExprToken::VarCT) => {
                self.combine_times(v, time, Comptime)?;
                Ok((&v.source()[1..], Comptime))
            }
            _ => Err(self.error_expected(v, expected_msg)),
        }
    }

    fn parse_type<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<Type, Error<'s>> {
        match self.token(v) {
            // May need to distinguish the unit value from the unit type one day
            Token::Expr(ExprToken::Literal(LiteralToken::Unit)) => Ok(Type::Unit),
            Token::Type(TypeToken::Bool) => Ok(Type::Bool),
            Token::Type(TypeToken::Int) => Ok(Type::Int),
            _ => Err(self.error_expected(v, "type")),
        }
    }

    fn parse_expr_with_span<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
    ) -> Result<(Expr, Span), Error<'s>> {
        match self.token(v) {
            Token::Expr(token) => {
                let expr = self.parse_expr(v, token, time)?;
                Ok((expr, v.span()))
            }
            _ => Err(self.error_expected(v, "expression")),
        }
    }

    fn parse_expr<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        token: ExprToken,
        time: Time,
    ) -> Result<Expr, Error<'s>> {
        match token {
            ExprToken::Literal(literal_tok) => {
                let literal = self.parse_literal(v, literal_tok)?;
                Ok(Expr::Literal(literal))
            }
            ExprToken::Var | ExprToken::VarCT => {
                let (var_name, var_time) = self.parse_refn(v, "variable name", time)?;
                Ok(Expr::Var(VarRefn::new(var_name, var_time)))
            }
            ExprToken::EUnop(unop) => {
                let expr = self.parse_expr_with_span(v.child(0), time)?;
                Ok(Expr::Unop(unop, Box::new(expr)))
            }
            ExprToken::EBinop(binop) => {
                let left = self.parse_expr_with_span(v.child(0), time)?;
                let right = self.parse_expr_with_span(v.child(1), time)?;
                Ok(Expr::Binop(binop, Box::new(left), Box::new(right)))
            }
            ExprToken::Parens => {
                if self.token(v.child(0)) == Token::Blank {
                    Ok(Expr::Literal(Literal::Unit))
                } else {
                    let expr = self.parse_expr_with_span(v.child(0), time)?;
                    Ok(expr.0)
                }
            }
            ExprToken::If => {
                let if_expr = self.parse_if_expr(v, time, Runtime)?;
                Ok(Expr::If(if_expr))
            }
            ExprToken::IfCT => {
                let if_expr = self.parse_if_expr(v, time, Comptime)?;
                Ok(Expr::If(if_expr))
            }
            ExprToken::Apply => {
                let apply_expr = self.parse_apply_expr(v, time)?;
                Ok(Expr::Apply(apply_expr))
            }
            ExprToken::Block => {
                let block = self.parse_block_expr(v, time, Runtime)?;
                Ok(Expr::Block(block))
            }
            ExprToken::BlockCT => {
                let block = self.parse_block_expr(v, time, Comptime)?;
                Ok(Expr::Block(block))
            }
            ExprToken::Comptime => {
                let time = self.combine_times(v, time, Comptime)?;
                let expr = self.parse_expr_with_span(v.child(0), time)?;
                Ok(Expr::ComptimeExpr(Box::new(expr)))
            }
            ExprToken::Return => {
                let expr = self.parse_expr_with_span(v.child(0), time)?;
                Ok(Expr::Return(Box::new(expr)))
            }
        }
    }

    fn parse_literal<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        token: LiteralToken,
    ) -> Result<Literal, Error<'s>> {
        match token {
            LiteralToken::Unit => Ok(Literal::Unit),
            LiteralToken::True => Ok(Literal::Bool(true)),
            LiteralToken::False => Ok(Literal::Bool(false)),
            LiteralToken::Int => match v.source().parse::<i32>() {
                Ok(n) => Ok(Literal::Int(n)),
                Err(err) => {
                    Err(self.error(v, "invalid int", &format!("Invalid integer: '{}'", err)))
                }
            },
        }
    }

    fn parse_block_expr<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
        block_time: Time,
    ) -> Result<BlockExpr, Error<'s>> {
        let time = self.combine_times(v, time, block_time)?;
        let block = self.parse_block_with_span(v.child(0), time)?;
        Ok(BlockExpr {
            block: Box::new(block),
            time: block_time,
        })
    }

    fn parse_if_expr<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
        if_time: Time,
    ) -> Result<IfExpr, Error<'s>> {
        let e_if = self.parse_expr_with_span(v.child(0), time + if_time)?;
        let e_then = self.parse_expr_with_span(v.child(1), time)?;
        let e_else = self.parse_expr_with_span(v.child(2), time)?;
        Ok(IfExpr {
            e_if: Box::new(e_if),
            e_then: Box::new(e_then),
            e_else: Box::new(e_else),
            time: if_time,
        })
    }

    fn parse_apply_expr<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        time: Time,
    ) -> Result<ApplyExpr, Error<'s>> {
        let (func_name, func_time) = self.parse_refn(v.child(0), "function name", time)?;
        let func = FuncRefn::new(func_name);
        let func_span = v.child(0).span();
        let args = self.parse_args(v.child(1), time + func_time)?;
        Ok(ApplyExpr {
            func: (func, func_span),
            args,
            time: func_time,
        })
    }

    fn parse_var<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<Var, Error<'s>> {
        match self.token(v) {
            Token::Expr(ExprToken::Var) => Ok(Var::new(v.source())),
            _ => Err(self.error_expected(v, "variable name")),
        }
    }

    fn token(&self, v: Visitor) -> Token {
        Token::from_str(v.name())
    }

    #[allow(unused)] // likely to be used in the future
    fn expect_blank<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<(), Error<'s>> {
        let token = self.token(v);
        if token == Token::Blank {
            Ok(())
        } else {
            Err(v
                .error(
                    "unexpected",
                    &format!("Expected nothing, but found {}", token),
                )
                .into())
        }
    }

    fn combine_times<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
        outer_time: Time,
        inner_time: Time,
    ) -> Result<Time, Error<'s>> {
        match (outer_time, inner_time) {
            (Runtime, Runtime) => Ok(Runtime),
            (Comptime, Runtime) | (Runtime, Comptime) => Ok(Comptime),
            (Comptime, Comptime) => Err(self.error_nested_comptime(v)),
        }
    }

    fn error_nested_comptime<'s>(&self, v: Visitor<'s, '_, '_>) -> Error<'s> {
        self.error(
            v,
            "nested comptime",
            "This nested comptime annotation is redundant.",
        )
    }

    fn error_expected<'s>(&self, v: Visitor<'s, '_, '_>, expected: &str) -> Error<'s> {
        let token = self.token(v);
        if token == Token::Blank {
            v.error(
                &format!("missing {}", expected),
                &format!("Missing {}.", expected),
            )
            .into()
        } else {
            v.error(
                &format!("expected {}", expected),
                &format!("Expected {} but found {}", expected, token),
            )
            .into()
        }
    }

    fn error<'s>(&self, v: Visitor<'s, '_, '_>, label: &str, message: &str) -> Error<'s> {
        v.error(label, message).into()
    }
}

impl<'s> From<panfix::ParseError<'s>> for Error<'s> {
    fn from(err: panfix::ParseError<'s>) -> Error<'s> {
        Error {
            kind: "Parse Error",
            source: err.source,
            span: err.span,
            label: err.short_message,
            message: err.message,
        }
    }
}

fn log_parse_tree(v: Visitor, logger: &mut Logger) {
    match v.num_children() {
        0 => log!(logger, Trace, v.name(), ("{}", v.source())),
        n => span!(logger, Trace, v.name(), {
            for i in 0..n {
                log_parse_tree(v.child(i), logger);
            }
        }),
    }
}
