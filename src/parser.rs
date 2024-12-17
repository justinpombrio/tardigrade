use crate::ast::{Binop_II_I, Expr, Span};
use crate::error::Error;
use panfix::{pattern, Grammar, GrammarError, Parser as PanfixParser, Source, Visitor};

pub struct Parser(PanfixParser);

impl Parser {
    pub fn new() -> Parser {
        // The grammar is specified in source code, so any error in it is a bug.
        Parser::new_impl().unwrap()
    }

    fn new_impl() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new("[ \n\r\t]+")?;
        grammar.regex("Int", r#"-?0|[1-9][0-9]*"#)?;

        grammar.right_assoc();
        grammar.op("Mul", pattern!(_ "*" _))?;
        grammar.op("Div", pattern!(_ "/" _))?;

        grammar.right_assoc();
        grammar.op("Add", pattern!(_ "+" _))?;
        grammar.op("Sub", pattern!(_ "-" _))?;

        let parser = grammar.finish()?;
        Ok(Parser(parser))
    }

    pub fn parse<'s>(&self, source: &'s Source) -> Result<(Expr, Span), Error<'s>> {
        let tree = self.0.parse(source)?;
        self.parse_expr_with_span(tree.visitor())
    }

    fn parse_expr_with_span<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<(Expr, Span), Error<'s>> {
        let expr = self.parse_expr(v)?;
        Ok((expr, v.span()))
    }

    fn parse_expr<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<Expr, Error<'s>> {
        match v.name() {
            "Blank" => Err(self.error(v, "expected expression", "Missing expression.")),
            "Juxtapose" => Err(self.error(
                v,
                "extra expression",
                "Multiple expressions in a row, expected only one.",
            )),
            "Int" => match v.source().parse::<i32>() {
                Ok(n) => Ok(Expr::Int(n)),
                Err(err) => Err(self.error(v, "invalid int", &format!("Invalid int: '{}'", err))),
            },
            "Add" | "Sub" | "Mul" | "Div" => self.parse_binop(v),
            other => panic!("Bug: missing case in parser/expr: '{}'", other),
        }
    }

    fn parse_binop<'s>(&self, v: Visitor<'s, '_, '_>) -> Result<Expr, Error<'s>> {
        use Binop_II_I::*;

        let binop = match v.name() {
            "Add" => Add,
            "Sub" => Sub,
            "Mul" => Mul,
            "Div" => Div,
            other => panic!("Bug: missing case in parser/binop: '{}'", other),
        };
        let left = self.parse_expr_with_span(v.child(0))?;
        let right = self.parse_expr_with_span(v.child(1))?;
        Ok(Expr::Binop_II_I(binop, Box::new(left), Box::new(right)))
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
