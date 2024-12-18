use crate::ast::{Binop, Unop};
use panfix::{pattern, Grammar, GrammarError, Parser as PanfixParser};
use std::fmt;

pub fn construct_grammar() -> PanfixParser {
    // The grammar is specified in source code, so any error in it is a bug.
    construct_grammar_impl().unwrap()
}

fn construct_grammar_impl() -> Result<PanfixParser, GrammarError> {
    let mut grammar = Grammar::new("([ \n\r\t]+|//.*)+")?;

    grammar.regex("Int", r#"-?0|[1-9][0-9]*"#)?;
    grammar.regex("Var", r#"[_a-z][_0-9a-zA-Z]*"#)?;
    grammar.string("Unit", "()")?;
    grammar.string("True", "true")?;
    grammar.string("False", "false")?;

    grammar.string("TypeBool", "Bool")?;
    grammar.string("TypeInt", "Int")?;

    grammar.op("Block", pattern!("block" "end"))?;
    grammar.op("Func", pattern!("func" "(" ")" "=" "end"))?;

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
    grammar.op("Arrow", pattern!("->" _))?;

    grammar.right_assoc();
    grammar.op("Colon", pattern!(_ ":" _))?;

    grammar.right_assoc();
    grammar.op("If", pattern!("if" "then" "else" "end"))?;

    grammar.right_assoc();
    grammar.op("Comma", pattern!(_ "," _))?;

    grammar.right_assoc();
    grammar.op("Let", pattern!("let" "=" _))?;

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
    Func,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeToken {
    Bool,
    Int,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprToken {
    Value(ValueToken),
    Var,
    Apply,
    Unop(Unop),
    Binop(Binop),
    If,
    Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueToken {
    Unit,
    True,
    False,
    Int,
}

impl Token {
    // Really, the Panfix parser should be parameterized over this Token type rather than working
    // with strings. But since they're strings we'll convert them.
    pub fn from_str(op_name: &str) -> Token {
        match op_name {
            "Blank" => Token::Blank,
            "Juxtapose" => Token::Juxtapose,
            "Comma" => Token::Comma,
            "Colon" => Token::Colon,
            "Arrow" => Token::Arrow,
            "Let" => Token::Stmt(StmtToken::Let),
            "Func" => Token::Stmt(StmtToken::Func),
            "TypeBool" => Token::Type(TypeToken::Bool),
            "TypeInt" => Token::Type(TypeToken::Int),
            op_name => Token::Expr(ExprToken::from_str(op_name)),
        }
    }
}

impl ExprToken {
    fn from_str(op_name: &str) -> ExprToken {
        match op_name {
            "Unit" => ExprToken::Value(ValueToken::Unit),
            "True" => ExprToken::Value(ValueToken::True),
            "False" => ExprToken::Value(ValueToken::False),
            "Int" => ExprToken::Value(ValueToken::Int),
            "Var" => ExprToken::Var,
            "Apply" => ExprToken::Apply,
            "If" => ExprToken::If,
            "Not" => ExprToken::Unop(Unop::Not),
            "Add" => ExprToken::Binop(Binop::Add),
            "Sub" => ExprToken::Binop(Binop::Sub),
            "Mul" => ExprToken::Binop(Binop::Mul),
            "Div" => ExprToken::Binop(Binop::Div),
            "Eq" => ExprToken::Binop(Binop::Eq),
            "Ne" => ExprToken::Binop(Binop::Ne),
            "Lt" => ExprToken::Binop(Binop::Lt),
            "Le" => ExprToken::Binop(Binop::Le),
            "Gt" => ExprToken::Binop(Binop::Gt),
            "Ge" => ExprToken::Binop(Binop::Ge),
            "And" => ExprToken::Binop(Binop::And),
            "Or" => ExprToken::Binop(Binop::Or),
            "Block" => ExprToken::Block,
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
            Stmt(Func) => write!(f, "function definition"),
            Type(_) => write!(f, "type"),
            Expr(_) => write!(f, "expression"),
        }
    }
}
