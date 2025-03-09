use crate::ast::{Binop, Unop};
use panfix::{pattern, Grammar, GrammarError, Parser as PanfixParser, Token as PanfixToken};
use std::fmt;

pub fn construct_grammar() -> PanfixParser<Token> {
    // The grammar is specified in source code, so any error in it is a bug.
    match construct_grammar_impl() {
        Ok(parser) => parser,
        Err(err) => panic!("{}", err),
    }
}

fn construct_grammar_impl() -> Result<PanfixParser<Token>, GrammarError> {
    use Binop::*;
    use ExprToken::*;
    use LiteralToken::*;
    use StmtToken::*;
    use Token::*;
    use Unop::*;

    let mut grammar = Grammar::<Token>::new("([ \n\r\t]+|//.*)+")?;

    grammar.regex(Expr(Literal(Int)), r#"-?0|[1-9][0-9]*"#)?;
    grammar.regex(Expr(Var), r#"[_a-z][_0-9a-zA-Z]*"#)?;
    grammar.regex(Expr(VarCT), r#"#[_a-z][_0-9a-zA-Z]*"#)?;
    grammar.string(Expr(Literal(True)), "true")?;
    grammar.string(Expr(Literal(False)), "false")?;

    grammar.string(Type(TypeToken::Bool), "Bool")?;
    grammar.string(Type(TypeToken::Int), "Int")?;

    grammar.op(Expr(Block), pattern!("block" "{" "}"))?;
    grammar.op(Expr(BlockCT), pattern!("#block" "{" "}"))?;
    grammar.op(Expr(Comptime), pattern!("#(" ")"))?;
    grammar.op(Expr(If), pattern!("if" "{" "}" "else" "{" "}"))?;
    grammar.op(Expr(IfCT), pattern!("#if" "{" "}" "else" "{" "}"))?;
    grammar.op(Expr(Parens), pattern!("(" ")"))?;

    grammar.left_assoc();
    grammar.op(Expr(Dot), pattern!(_ "." _))?;

    grammar.left_assoc();
    grammar.op(Expr(Apply), pattern!(_ "(" ")"))?;

    grammar.left_assoc();
    grammar.op(Expr(EBinop(Mul)), pattern!(_ "*" _))?;
    grammar.op(Expr(EBinop(Div)), pattern!(_ "/" _))?;

    grammar.left_assoc();
    grammar.op(Expr(EBinop(Add)), pattern!(_ "+" _))?;
    grammar.op(Expr(EBinop(Sub)), pattern!(_ "-" _))?;

    grammar.right_assoc();
    grammar.op(Expr(EBinop(Eq)), pattern!(_ "==" _))?;
    grammar.op(Expr(EBinop(Ne)), pattern!(_ "!=" _))?;
    grammar.op(Expr(EBinop(Lt)), pattern!(_ "<" _))?;
    grammar.op(Expr(EBinop(Le)), pattern!(_ "<=" _))?;
    grammar.op(Expr(EBinop(Gt)), pattern!(_ ">" _))?;
    grammar.op(Expr(EBinop(Ge)), pattern!(_ ">=" _))?;

    grammar.left_assoc();
    grammar.op(Expr(EUnop(Not)), pattern!("not" _))?;

    grammar.left_assoc();
    grammar.op(Expr(EBinop(And)), pattern!(_ "and" _))?;

    grammar.left_assoc();
    grammar.op(Expr(EBinop(Or)), pattern!(_ "or" _))?;

    grammar.left_assoc();
    grammar.op(Expr(Return), pattern!("return" _))?;

    grammar.left_assoc();
    grammar.op(Arrow, pattern!("->" _))?;

    grammar.right_assoc();
    grammar.op(Colon, pattern!(_ ":" _))?;

    grammar.right_assoc();
    grammar.op(Comma, pattern!(_ "," _))?;

    grammar.right_assoc();
    grammar.op(Semicolon, pattern!(_ ";" _))?;
    grammar.op(Stmt(Let), pattern!("let" "=" ";" _))?;
    grammar.op(Stmt(LetCT), pattern!("#let" "=" ";" _))?;
    grammar.op(Stmt(Set), pattern!("set" "=" ";" _))?;
    grammar.op(Stmt(SetCT), pattern!("#set" "=" ";" _))?;
    grammar.op(Stmt(Func), pattern!("func" "(" ")" "{" "}" _))?;
    grammar.op(Stmt(FuncCT), pattern!("#func" "(" ")" "{" "}" _))?;

    grammar.right_assoc();
    grammar.juxtapose()?;

    grammar.finish()
}

impl PanfixToken for Token {
    const LEX_ERROR: Token = Token::LexError;
    const BLANK: Token = Token::Blank;
    const JUXTAPOSE: Token = Token::Juxtapose;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    LexError,
    Blank,
    Juxtapose,
    Comma,
    Semicolon,
    Colon,
    Arrow,
    Stmt(StmtToken),
    Type(TypeToken),
    Expr(ExprToken),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StmtToken {
    Let,
    LetCT,
    Set,
    SetCT,
    Func,
    FuncCT,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeToken {
    Bool,
    Int,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprToken {
    Literal(LiteralToken),
    Var,
    VarCT,
    Dot,
    Apply,
    EUnop(Unop),
    EBinop(Binop),
    Parens,
    If,
    IfCT,
    Block,
    BlockCT,
    Comptime,
    Return,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralToken {
    True,
    False,
    Int,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StmtToken::*;
        use Token::*;

        match self {
            LexError => write!(f, "LEX_ERROR"), // should never happen
            Blank => write!(f, "nothing"),
            Juxtapose => write!(f, "multiple items"),
            Comma => write!(f, "','"),
            Semicolon => write!(f, "';'"),
            Colon => write!(f, "':'"),
            Arrow => write!(f, "'->'"),
            Stmt(Let) => write!(f, "let statement"),
            Stmt(LetCT) => write!(f, "comptime let statement"),
            Stmt(Set) => write!(f, "set statement"),
            Stmt(SetCT) => write!(f, "comptime set statement"),
            Stmt(Func) => write!(f, "function definition"),
            Stmt(FuncCT) => write!(f, "comptime function definition"),
            Type(_) => write!(f, "type"),
            Expr(_) => write!(f, "expression"),
        }
    }
}
