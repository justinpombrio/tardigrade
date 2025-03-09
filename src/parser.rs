//! Parse an AST out of a token tree. Error on redundant nested comptimes.

use crate::ast::{
    ApplyExpr, Block, BlockExpr, Expr, FuncRefn, FuncStmt, IfExpr, LetStmt, Literal, Param,
    SetStmt, Span, Stmt, Time, VarRefn,
};
use crate::error::Error;
use crate::grammar::{construct_grammar, ExprToken, LiteralToken, StmtToken, Token, TypeToken};
use crate::logger::Logger;
use crate::type_checker::Type;
use crate::{log, span};
use panfix::{Parser as PanfixParser, Source, Visitor as PanfixVisitor};

use Time::{Comptime, Runtime};

type Visitor<'s, 't> = PanfixVisitor<'s, 't, 't, Token>;

pub struct Parser(PanfixParser<Token>);

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
        v: Visitor<'s, '_>,
        time: Time,
    ) -> Result<(Block, Span), Error<'s>> {
        let block = self.parse_block(v, time)?;
        Ok((block, v.span()))
    }

    fn parse_block<'s>(&self, mut v: Visitor<'s, '_>, time: Time) -> Result<Block, Error<'s>> {
        let mut stmts = Vec::new();
        loop {
            match v.token() {
                Token::Blank => break,
                Token::Expr(_) => {
                    let expr = self.parse_expr_with_span(v, time)?;
                    let span = expr.1;
                    stmts.push((Stmt::Expr(expr), span));
                    break;
                }
                Token::Stmt(_) => {
                    let stmt = self.parse_stmt_with_span(&mut v, time)?;
                    stmts.push(stmt);
                }
                Token::Semicolon => {
                    let expr = self.parse_expr_with_span(v.child(0), time)?;
                    let span = expr.1;
                    stmts.push((Stmt::Expr(expr), span));
                    v = v.child(1);
                }
                _ => return Err(self.error_expected(v, "statement")),
            }
        }
        Ok(Block(stmts))
    }

    fn parse_stmt_with_span<'s>(
        &self,
        v: &mut Visitor<'s, '_>,
        time: Time,
    ) -> Result<(Stmt, Span), Error<'s>> {
        let stmt = match v.token() {
            Token::Stmt(token) => match token {
                StmtToken::Let => Stmt::Let(self.parse_let(v, time, Runtime)?),
                StmtToken::LetCT => Stmt::Let(self.parse_let(v, time, Comptime)?),
                StmtToken::Set => Stmt::Set(self.parse_set(v, time, Runtime)?),
                StmtToken::SetCT => Stmt::Set(self.parse_set(v, time, Comptime)?),
                StmtToken::Func => Stmt::Func(self.parse_func(v, time, Runtime)?),
                StmtToken::FuncCT => Stmt::Func(self.parse_func(v, time, Comptime)?),
            },
            _ => return Err(self.error_expected(*v, "expression")),
        };
        Ok((stmt, v.span()))
    }

    fn parse_let<'s>(
        &self,
        v: &mut Visitor<'s, '_>,
        time: Time,
        let_time: Time,
    ) -> Result<LetStmt, Error<'s>> {
        let time = self.combine_times(*v, time, let_time)?;
        let (name, name_time) = self.parse_var(v.child(0))?;
        match name_time {
            Runtime => (),
            Comptime => return Err(self.error_unexpected_comptime_var(
                v.child(0),
                "A let variable can't be marked as comptime with `#`. Did you mean to say `#let`?",
            )),
        }
        let definition = self.parse_expr_with_span(v.child(1), time)?;
        *v = v.child(2);
        Ok(LetStmt {
            name,
            definition,
            time: let_time,
        })
    }

    fn parse_set<'s>(
        &self,
        v: &mut Visitor<'s, '_>,
        time: Time,
        set_time: Time,
    ) -> Result<SetStmt, Error<'s>> {
        let time = self.combine_times(*v, time, set_time)?;
        let (var_name, var_time) = self.parse_ident(v.child(0), "variable name", time)?;
        let var_span = v.child(0).span();
        let var = VarRefn::new(var_name, var_time);
        let definition = self.parse_expr_with_span(v.child(1), time)?;
        *v = v.child(2);
        Ok(SetStmt {
            var: (var, var_span),
            definition,
            time: set_time,
        })
    }

    fn parse_func<'s>(
        &self,
        v: &mut Visitor<'s, '_>,
        time: Time,
        func_time: Time,
    ) -> Result<FuncStmt, Error<'s>> {
        let time = self.combine_times(*v, time, func_time)?;
        let (name, name_time) = self.parse_var(v.child(0))?;
        match name_time {
            Runtime => (),
            Comptime => {
                return Err(self.error_unexpected_comptime_var(
                    v.child(0),
                    "A function name can't be marked as comptime with `#`. Did you mean to say `#func`?",
                ))
            }
        }
        let params = self.parse_params(v.child(1))?;
        let return_type = self.parse_return_type(v.child(2))?;
        let body = self.parse_block_with_span(v.child(3), time)?;
        *v = v.child(4);
        Ok(FuncStmt {
            name: name.to_owned(),
            params,
            return_type,
            body,
            time: func_time,
        })
    }

    fn parse_return_type<'s>(&self, v: Visitor<'s, '_>) -> Result<Type, Error<'s>> {
        match v.token() {
            Token::Blank => Ok(Type::Unit),
            Token::Arrow => self.parse_type(v.child(0)),
            _ => Err(self.error_expected(v, "return type annotation")),
        }
    }

    fn parse_params<'s>(&self, v: Visitor<'s, '_>) -> Result<Vec<Param>, Error<'s>> {
        self.parse_comma_sep(v, |v| self.parse_param(v))
    }

    fn parse_param<'s>(&self, v: Visitor<'s, '_>) -> Result<Param, Error<'s>> {
        match v.token() {
            Token::Colon => {
                let (name, name_time) = self.parse_var(v.child(0))?;
                match name_time {
                    Runtime => (),
                    Comptime => {
                        return Err(self.error_unexpected_comptime_var(
                            v.child(0),
                            "A function parameter can't be marked as comptime with `#`.",
                        ))
                    }
                }
                let ty = self.parse_type(v.child(1))?;
                Ok(Param { name, ty })
            }
            _ => Err(self.error_expected(v, "function parameter")),
        }
    }

    fn parse_args<'s>(
        &self,
        v: Visitor<'s, '_>,
        time: Time,
    ) -> Result<Vec<(Expr, Span)>, Error<'s>> {
        self.parse_comma_sep(v, |v| self.parse_expr_with_span(v, time))
    }

    fn parse_ident<'s>(
        &self,
        v: Visitor<'s, '_>,
        expected_msg: &str,
        time: Time,
    ) -> Result<(&'s str, Time), Error<'s>> {
        match v.token() {
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

    fn parse_var<'s>(&self, v: Visitor<'s, '_>) -> Result<(String, Time), Error<'s>> {
        match v.token() {
            Token::Expr(ExprToken::VarCT) => Ok((v.source().to_owned(), Comptime)),
            Token::Expr(ExprToken::Var) => Ok((v.source().to_owned(), Runtime)),
            _ => Err(self.error_expected(v, "variable name")),
        }
    }

    fn parse_type<'s>(&self, v: Visitor<'s, '_>) -> Result<Type, Error<'s>> {
        match v.token() {
            // May need to distinguish the unit value from the unit type one day
            Token::Expr(ExprToken::Parens) => {
                let inner_token = v.child(0).token();
                if inner_token == Token::Blank {
                    Ok(Type::Unit)
                } else if inner_token == Token::Comma {
                    let elems = self.parse_comma_sep(v.child(0), |v| self.parse_type(v))?;
                    Ok(Type::Tuple(elems))
                } else {
                    Err(self.error_expected(v, "type"))
                }
            }
            Token::Type(TypeToken::Bool) => Ok(Type::Bool),
            Token::Type(TypeToken::Int) => Ok(Type::Int),
            _ => Err(self.error_expected(v, "type")),
        }
    }

    fn parse_expr_with_span<'s>(
        &self,
        v: Visitor<'s, '_>,
        time: Time,
    ) -> Result<(Expr, Span), Error<'s>> {
        match v.token() {
            Token::Expr(token) => {
                let expr = self.parse_expr(v, token, time)?;
                Ok((expr, v.span()))
            }
            _ => Err(self.error_expected(v, "expression")),
        }
    }

    fn parse_expr<'s>(
        &self,
        v: Visitor<'s, '_>,
        token: ExprToken,
        time: Time,
    ) -> Result<Expr, Error<'s>> {
        match token {
            ExprToken::Literal(literal_tok) => {
                let literal = self.parse_literal(v, literal_tok)?;
                Ok(Expr::Literal(literal))
            }
            ExprToken::Var | ExprToken::VarCT => {
                let (var_name, var_time) = self.parse_ident(v, "variable name", time)?;
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
                let inner_token = v.child(0).token();
                if inner_token == Token::Blank {
                    Ok(Expr::Literal(Literal::Unit))
                } else if inner_token == Token::Comma {
                    let elems =
                        self.parse_comma_sep(v.child(0), |v| self.parse_expr_with_span(v, time))?;
                    Ok(Expr::Tuple(elems))
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
            ExprToken::Dot => self.parse_dot_expr(v, time),
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
        v: Visitor<'s, '_>,
        token: LiteralToken,
    ) -> Result<Literal, Error<'s>> {
        match token {
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
        v: Visitor<'s, '_>,
        time: Time,
        block_time: Time,
    ) -> Result<BlockExpr, Error<'s>> {
        let time = self.combine_times(v, time, block_time)?;
        self.expect_blank(v.child(0))?;
        let block = self.parse_block_with_span(v.child(1), time)?;
        Ok(BlockExpr {
            block: Box::new(block),
            time: block_time,
        })
    }

    fn parse_if_expr<'s>(
        &self,
        v: Visitor<'s, '_>,
        time: Time,
        if_time: Time,
    ) -> Result<IfExpr, Error<'s>> {
        let e_if = self.parse_expr_with_span(v.child(0), time + if_time)?;
        let e_then = self.parse_block_with_span(v.child(1), time)?;
        self.expect_blank(v.child(2))?;
        self.expect_blank(v.child(3))?;
        let e_else = self.parse_block_with_span(v.child(4), time)?;
        Ok(IfExpr {
            e_if: Box::new(e_if),
            e_then: Box::new(e_then),
            e_else: Box::new(e_else),
            time: if_time,
        })
    }

    fn parse_dot_expr<'s>(&self, v: Visitor<'s, '_>, time: Time) -> Result<Expr, Error<'s>> {
        let v_expr = v.child(0);
        let v_index = v.child(1);
        let expr = self.parse_expr_with_span(v_expr, time)?;
        match v.child(1).token() {
            Token::Expr(ExprToken::Literal(LiteralToken::Int)) => {
                match v_index.source().parse::<u8>() {
                    Ok(index) => Ok(Expr::TupleAccess(Box::new(expr), index)),
                    Err(err) => Err(self.error(
                        v_index,
                        "invalid index",
                        &format!("Invalid tuple index: '{}'", err),
                    )),
                }
            }
            _ => Err(self.error(v_index, "expected index", "Expected tuple index")),
        }
    }

    fn parse_apply_expr<'s>(&self, v: Visitor<'s, '_>, time: Time) -> Result<ApplyExpr, Error<'s>> {
        let (func_name, func_time) = self.parse_ident(v.child(0), "function name", time)?;
        let func = FuncRefn::new(func_name);
        let func_span = v.child(0).span();
        let args = self.parse_args(v.child(1), time + func_time)?;
        Ok(ApplyExpr {
            func: (func, func_span),
            args,
            time: func_time,
        })
    }

    fn parse_comma_sep<'s, T>(
        &self,
        mut v: Visitor<'s, '_>,
        parse_elem: impl Fn(Visitor<'s, '_>) -> Result<T, Error<'s>>,
    ) -> Result<Vec<T>, Error<'s>> {
        let mut elems = Vec::new();
        while v.token() == Token::Comma {
            elems.push(parse_elem(v.child(0))?);
            v = v.child(1);
        }
        if v.token() != Token::Blank {
            elems.push(parse_elem(v)?);
        }
        Ok(elems)
    }

    fn expect_blank<'s>(&self, v: Visitor<'s, '_>) -> Result<(), Error<'s>> {
        if v.token() == Token::Blank {
            Ok(())
        } else {
            Err(self.error_expected_nothing(v))
        }
    }

    fn combine_times<'s>(
        &self,
        v: Visitor<'s, '_>,
        outer_time: Time,
        inner_time: Time,
    ) -> Result<Time, Error<'s>> {
        match (outer_time, inner_time) {
            (Runtime, Runtime) => Ok(Runtime),
            (Comptime, Runtime) | (Runtime, Comptime) => Ok(Comptime),
            (Comptime, Comptime) => Err(self.error_nested_comptime(v)),
        }
    }

    fn error_expected_nothing<'s>(&self, v: Visitor<'s, '_>) -> Error<'s> {
        self.error(
            v,
            "unexpected",
            &format!("Expected nothing, but found {}", v.token()),
        )
    }

    fn error_unexpected_comptime_var<'s>(&self, v: Visitor<'s, '_>, msg: &str) -> Error<'s> {
        self.error(v, "unexpected comptime", msg)
    }

    fn error_nested_comptime<'s>(&self, v: Visitor<'s, '_>) -> Error<'s> {
        self.error(
            v,
            "nested comptime",
            "This nested comptime annotation is redundant.",
        )
    }

    fn error_expected<'s>(&self, v: Visitor<'s, '_>, expected: &str) -> Error<'s> {
        if v.token() == Token::Blank {
            v.error(
                &format!("missing {}", expected),
                &format!("Missing {}.", expected),
            )
            .into()
        } else {
            v.error(
                &format!("expected {}", expected),
                &format!("Expected {} but found {}", expected, v.token()),
            )
            .into()
        }
    }

    fn error<'s>(&self, v: Visitor<'s, '_>, label: &str, message: &str) -> Error<'s> {
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
        0 => log!(logger, Trace, &v.token().to_string(), ("{}", v.source())),
        n => span!(logger, Trace, &v.token().to_string(), {
            for i in 0..n {
                log_parse_tree(v.child(i), logger);
            }
        }),
    }
}
