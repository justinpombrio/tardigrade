use crate::ast::{Binop, Unop};
use panfix::{pattern, Grammar, GrammarError, Parser as PanfixParser};
use std::fmt;

pub fn construct_grammar() -> PanfixParser {
    // The grammar is specified in source code, so any error in it is a bug.
    match construct_grammar_impl() {
        Ok(parser) => parser,
        Err(err) => panic!("{}", err),
    }
}

fn construct_grammar_impl() -> Result<PanfixParser, GrammarError> {
    let mut grammar = Grammar::new("([ \n\r\t]+|//.*)+")?;

    grammar.regex("Int", r#"-?0|[1-9][0-9]*"#)?;
    grammar.regex("Var", r#"[_a-z][_0-9a-zA-Z]*"#)?;
    grammar.regex("VarCT", r#"#[_a-z][_0-9a-zA-Z]*"#)?;
    grammar.string("True", "true")?;
    grammar.string("False", "false")?;

    grammar.string("TypeBool", "Bool")?;
    grammar.string("TypeInt", "Int")?;

    grammar.op("Block", pattern!("block" "end"))?;
    grammar.op("BlockCT", pattern!("#block" "end"))?;
    grammar.op("Func", pattern!("func" "(" ")" "=" "end"))?;
    grammar.op("FuncCT", pattern!("#func" "(" ")" "=" "end"))?;
    grammar.op("Comptime", pattern!("#(" ")"))?;
    grammar.op("If", pattern!("if" "then" "else" "end"))?;
    grammar.op("IfCT", pattern!("#if" "then" "else" "end"))?;
    grammar.op("Parens", pattern!("(" ")"))?;

    grammar.right_assoc();
    grammar.op("Apply", pattern!(_ "(" ")"))?;

    grammar.right_assoc();
    grammar.op("Mul", pattern!(_ "*" _))?;
    grammar.op("Div", pattern!(_ "/" _))?;

    grammar.right_assoc();
    grammar.op("Add", pattern!(_ "+" _))?;
    grammar.op("Sub", pattern!(_ "-" _))?;

    grammar.right_assoc();
    grammar.op("Eq", pattern!(_ "==" _))?;
    grammar.op("Ne", pattern!(_ "!=" _))?;
    grammar.op("Lt", pattern!(_ "<" _))?;
    grammar.op("Le", pattern!(_ "<=" _))?;
    grammar.op("Gt", pattern!(_ ">" _))?;
    grammar.op("Ge", pattern!(_ ">=" _))?;

    grammar.right_assoc();
    grammar.op("Not", pattern!("not" _))?;

    grammar.right_assoc();
    grammar.op("And", pattern!(_ "and" _))?;

    grammar.right_assoc();
    grammar.op("Or", pattern!(_ "or" _))?;

    grammar.right_assoc();
    grammar.op("Return", pattern!("return" _))?;

    grammar.right_assoc();
    grammar.op("Arrow", pattern!("->" _))?;

    grammar.right_assoc();
    grammar.op("Colon", pattern!(_ ":" _))?;

    grammar.right_assoc();
    grammar.op("Comma", pattern!(_ "," _))?;

    grammar.right_assoc();
    grammar.op("Let", pattern!("let" "=" _))?;
    grammar.op("LetCT", pattern!("#let" "=" _))?;
    grammar.op("Set", pattern!("set" "=" _))?;
    grammar.op("SetCT", pattern!("#set" "=" _))?;

    grammar.right_assoc();
    grammar.juxtapose()?;

    grammar.finish()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Blank,
    Juxtapose,
    Comma,
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
    Unit,
    True,
    False,
    Int,
}

impl Token {
    // Really, the Panfix parser should be parameterized over this Token type rather than working
    // with strings. But since they're strings we'll convert them.
    pub fn from_str(op_name: &str) -> Token {
        use Token::*;

        match op_name {
            "Blank" => Blank,
            "Juxtapose" => Juxtapose,
            "Comma" => Comma,
            "Colon" => Colon,
            "Arrow" => Arrow,
            "Let" => Stmt(StmtToken::Let),
            "LetCT" => Stmt(StmtToken::LetCT),
            "Set" => Stmt(StmtToken::Set),
            "SetCT" => Stmt(StmtToken::SetCT),
            "Func" => Stmt(StmtToken::Func),
            "FuncCT" => Stmt(StmtToken::FuncCT),
            "TypeBool" => Type(TypeToken::Bool),
            "TypeInt" => Type(TypeToken::Int),
            op_name => Expr(ExprToken::from_str(op_name)),
        }
    }
}

impl ExprToken {
    fn from_str(op_name: &str) -> ExprToken {
        use ExprToken::*;

        match op_name {
            "Unit" => Literal(LiteralToken::Unit),
            "True" => Literal(LiteralToken::True),
            "False" => Literal(LiteralToken::False),
            "Int" => Literal(LiteralToken::Int),
            "Var" => Var,
            "VarCT" => VarCT,
            "Apply" => Apply,
            "Parens" => Parens,
            "If" => If,
            "IfCT" => IfCT,
            "Not" => EUnop(Unop::Not),
            "Add" => EBinop(Binop::Add),
            "Sub" => EBinop(Binop::Sub),
            "Mul" => EBinop(Binop::Mul),
            "Div" => EBinop(Binop::Div),
            "Eq" => EBinop(Binop::Eq),
            "Ne" => EBinop(Binop::Ne),
            "Lt" => EBinop(Binop::Lt),
            "Le" => EBinop(Binop::Le),
            "Gt" => EBinop(Binop::Gt),
            "Ge" => EBinop(Binop::Ge),
            "And" => EBinop(Binop::And),
            "Or" => EBinop(Binop::Or),
            "Block" => Block,
            "BlockCT" => BlockCT,
            "Comptime" => Comptime,
            "Return" => Return,
            other => panic!("Bug in parser: missing token {}", other),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StmtToken::*;
        use Token::*;

        match self {
            Blank => write!(f, "nothing"),
            Juxtapose => write!(f, "multiple items"),
            Comma => write!(f, "','"),
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
