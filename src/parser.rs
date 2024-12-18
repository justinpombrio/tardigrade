use crate::ast::{Block, Expr, FuncStmt, LetStmt, Param, Span, Stmt, Var, VarRefn};
use crate::error::Error;
use crate::grammar::{construct_grammar, ExprToken, StmtToken, Token, TypeToken, ValueToken};
use crate::type_checker::Type;
use panfix::{Parser as PanfixParser, Source, Visitor};

pub struct Parser(PanfixParser);

impl Parser {
    pub fn new() -> Parser {
        Parser(construct_grammar())
    }

    pub fn parse<'s>(&self, source: &'s Source) -> Result<(Block, Span), Error<'s>> {
        let tree = self.0.parse(source)?;
        self.parse_block_with_span(tree.visitor())
    }

    fn parse_block_with_span<'s>(
        &self,
        v: Visitor<'s, '_, '_>,
    ) -> Result<(Block, Span), Error<'s>> {
        let block = self.parse_block(v)?;
        Ok((block, v.span()))
    }

    fn parse_block<'s>(&self, mut v: Visitor<'s, '_, '_>) -> Result<Block, Error<'s>> {
        let mut stmts = Vec::new();
        loop {
            let token = self.token(v);
            match token {
                Token::Blank => break,
                Token::Juxtapose => {
                    stmts.push(self.parse_stmt_with_span(v.child(0))?);
                    v = v.child(1);
                }
                Token::Stmt(_) | Token::Expr(_) => {
                    stmts.push(self.parse_stmt_with_span(v)?);
                    break;
                }
                _ => return Err(self.error_expected(v, "statement")),
            }
        }
        Ok(Block(stmts))
    }

    fn parse_stmt_with_span<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<(Stmt, Span), Error<'s>> {
        match self.token(v) {
            Token::Expr(token) => {
                let expr = self.parse_expr(v, token)?;
                let span = v.span();
                Ok((Stmt::Expr((expr, span)), span))
            }
            Token::Stmt(token) => {
                let stmt = self.parse_stmt(v, token)?;
                Ok((stmt, v.span()))
            }
            _ => Err(self.error_expected(v, "expression")),
        }
    }

    fn parse_stmt<'s>(&self, v: Visitor<'s, '_, '_>, token: StmtToken) -> Result<Stmt, Error<'s>> {
        match token {
            StmtToken::Let => {
                let var = self.parse_var(v.child(0))?;
                let definition = self.parse_expr_with_span(v.child(1))?;
                Ok(Stmt::Let(LetStmt { var, definition }))
            }
            StmtToken::Func => {
                let var = self.parse_var(v.child(0))?;
                let params = self.parse_params(v.child(1))?;
                let return_type = self.parse_return_type(v.child(2))?;
                let body = self.parse_block_with_span(v.child(3))?;
                Ok(Stmt::Func(FuncStmt {
                    var,
                    params,
                    return_type,
                    body,
                }))
            }
        }
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

    fn parse_args<'s>(&self, mut v: Visitor<'s, '_, '_>) -> Result<Vec<(Expr, Span)>, Error<'s>> {
        let mut args = Vec::new();
        while self.token(v) == Token::Comma {
            args.push(self.parse_expr_with_span(v.child(0))?);
            v = v.child(1);
        }
        if self.token(v) != Token::Blank {
            args.push(self.parse_expr_with_span(v)?);
        }
        Ok(args)
    }

    fn parse_var_refn<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<(VarRefn, Span), Error<'s>> {
        match self.token(v) {
            Token::Expr(ExprToken::Var) => Ok((VarRefn::new(v.source()), v.span())),
            _ => Err(self.error_expected(v, "function name")),
        }
    }

    fn parse_type<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<Type, Error<'s>> {
        match self.token(v) {
            // May need to distinguish the unit value from the unit type one day
            Token::Expr(ExprToken::Value(ValueToken::Unit)) => Ok(Type::Unit),
            Token::Type(TypeToken::Bool) => Ok(Type::Bool),
            Token::Type(TypeToken::Int) => Ok(Type::Int),
            _ => Err(self.error_expected(v, "type")),
        }
    }

    fn parse_expr_with_span<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<(Expr, Span), Error<'s>> {
        match self.token(v) {
            Token::Expr(token) => {
                let expr = self.parse_expr(v, token)?;
                Ok((expr, v.span()))
            }
            _ => Err(self.error_expected(v, "expression")),
        }
    }

    fn parse_expr<'s>(&self, v: Visitor<'s, '_, '_>, token: ExprToken) -> Result<Expr, Error<'s>> {
        match token {
            ExprToken::Value(ValueToken::Unit) => Ok(Expr::Unit),
            ExprToken::Value(ValueToken::True) => Ok(Expr::Bool(true)),
            ExprToken::Value(ValueToken::False) => Ok(Expr::Bool(false)),
            ExprToken::Value(ValueToken::Int) => match v.source().parse::<i32>() {
                Ok(n) => Ok(Expr::Int(n)),
                Err(err) => {
                    Err(self.error(v, "invalid int", &format!("Invalid integer: '{}'", err)))
                }
            },
            ExprToken::Var => Ok(Expr::Var(VarRefn::new(v.source()))),
            ExprToken::Unop(unop) => {
                let expr = self.parse_expr_with_span(v.child(0))?;
                Ok(Expr::Unop(unop, Box::new(expr)))
            }
            ExprToken::Binop(binop) => {
                let left = self.parse_expr_with_span(v.child(0))?;
                let right = self.parse_expr_with_span(v.child(1))?;
                Ok(Expr::Binop(binop, Box::new(left), Box::new(right)))
            }
            ExprToken::If => {
                let e_if = self.parse_expr_with_span(v.child(0))?;
                let e_then = self.parse_expr_with_span(v.child(1))?;
                let e_else = self.parse_expr_with_span(v.child(2))?;
                Ok(Expr::If(Box::new(e_if), Box::new(e_then), Box::new(e_else)))
            }
            ExprToken::Apply => {
                let refn = self.parse_var_refn(v.child(0))?;
                let args = self.parse_args(v.child(1))?;
                Ok(Expr::Apply(refn, args))
            }
            ExprToken::Block => {
                let block = self.parse_block_with_span(v.child(0))?;
                Ok(Expr::Block(block))
            }
        }
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
