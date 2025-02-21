use crate::type_checker::Type;
use std::fmt;
use std::ops::Add;

/*******
 * AST *
 *******/

pub type Span = panfix::Span;

pub type FuncId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Time {
    Runtime,
    Comptime,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
}

#[derive(Debug)]
pub struct Block(pub Vec<(Stmt, Span)>);

#[derive(Debug)]
pub enum Stmt {
    Let(LetStmt),
    Func(FuncStmt),
    Expr((Expr, Span)),
}

#[derive(Debug)]
pub struct LetStmt {
    pub var: Var,
    pub definition: (Expr, Span),
    pub time: Time,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub var: Var,
    pub ty: Type,
}

#[derive(Debug)]
pub struct FuncStmt {
    pub var: Var,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: (Block, Span),
    pub time: Time,
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
pub enum Expr {
    Var(VarRefn),
    Literal(Literal),
    Unop(Unop, Box<(Expr, Span)>),
    Binop(Binop, Box<(Expr, Span)>, Box<(Expr, Span)>),
    If(IfExpr),
    Apply(ApplyExpr),
    Block(BlockExpr),
    ComptimeExpr(Box<(Expr, Span)>),
    Return(Box<(Expr, Span)>),
}

#[derive(Debug)]
pub struct ApplyExpr {
    pub func: (FuncRefn, Span),
    pub args: Vec<(Expr, Span)>,
    pub time: Time,
}

#[derive(Debug)]
pub struct IfExpr {
    pub e_if: Box<(Expr, Span)>,
    pub e_then: Box<(Expr, Span)>,
    pub e_else: Box<(Expr, Span)>,
    pub time: Time,
}

#[derive(Debug)]
pub struct BlockExpr {
    pub block: Box<(Block, Span)>,
    pub time: Time,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unop {
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

/// References the `offset`th variable in the `depth`th statically-enclosing stack frame.
///
/// (See https://en.wikipedia.org/wiki/Call_stack#Lexically_nested_routines)
#[derive(Debug, Clone)]
pub struct VarRefn {
    pub name: String,
    // Filled out during type checking
    pub depth: Option<usize>,
    pub offset: Option<isize>,
    pub time: Time,
}

#[derive(Debug, Clone)]
pub struct FuncRefn {
    pub name: String,
    // Filled out during type checking
    pub depth: Option<usize>,
    pub id: Option<FuncId>,
}

impl VarRefn {
    pub fn new(name: &str, time: Time) -> VarRefn {
        VarRefn {
            name: name.to_owned(),
            // Will be filled out during type checking
            depth: None,
            offset: None,
            time,
        }
    }

    pub fn unwrap_depth(&self) -> usize {
        self.depth
            .expect("VarRefn.depth not set during type checking")
    }

    pub fn unwrap_offset(&self) -> isize {
        self.offset
            .expect("VarRefn.offset not set during type checking")
    }
}

impl FuncRefn {
    pub fn new(name: &str) -> FuncRefn {
        FuncRefn {
            name: name.to_owned(),
            // Will be filled out during type checking
            depth: None,
            id: None,
        }
    }

    pub fn unwrap_depth(&self) -> usize {
        self.depth
            .expect("FuncRefn.depth not set during type checking")
    }

    pub fn unwrap_id(&self) -> FuncId {
        self.id.expect("FuncRefn.id not set during type checking")
    }
}

impl Var {
    pub fn new(name: &str) -> Var {
        Var {
            name: name.to_owned(),
        }
    }
}

/********
 * Prec *
 ********/

/// Precedence level. Smaller precedence binds tighter / wins.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Prec(u16);

impl Prec {
    pub const MAX: Prec = Prec(u16::MAX);
}

impl Binop {
    pub fn prec(&self) -> Prec {
        use Binop::*;

        let prec = match self {
            Mul | Div => 10,
            Add | Sub => 20,
            Eq | Ne | Lt | Le | Gt | Ge => 30,
            And => 50,
            Or => 60,
        };
        Prec(prec)
    }
}

impl Unop {
    pub fn prec(&self) -> Prec {
        Prec(40)
    }
}

/**********
 * Values *
 **********/

#[derive(Debug, Clone)]
pub enum Literal {
    Unit,
    Bool(bool),
    Int(i32),
}

/// A runtime value. The Value knows its type, but it will not tell you because:
///
/// - A full implementation would compile to assembly and not know the underlying type, except as
///   revealed by the type checker.
/// - The value does store its own type to notice at runtime if the typechecker messed up.
#[derive(Debug, Clone)]
pub struct Value(ValueCase);

#[derive(Debug, Clone)]
enum ValueCase {
    Unit,
    Bool(bool),
    Int(i32),
}

impl Value {
    pub fn unit() -> Value {
        Value(ValueCase::Unit)
    }

    pub fn bool(b: bool) -> Value {
        Value(ValueCase::Bool(b))
    }

    pub fn int(int: i32) -> Value {
        Value(ValueCase::Int(int))
    }

    pub fn unwrap_unit(self) {
        if !matches!(self.0, ValueCase::Unit) {
            self.type_mismatch(&Type::Unit)
        }
    }

    pub fn unwrap_bool(self) -> bool {
        if let ValueCase::Bool(b) = self.0 {
            b
        } else {
            self.type_mismatch(&Type::Bool)
        }
    }

    pub fn unwrap_int(self) -> i32 {
        if let ValueCase::Int(int) = self.0 {
            int
        } else {
            self.type_mismatch(&Type::Int)
        }
    }

    fn type_mismatch(self, expected: &Type) -> ! {
        panic!(
            "Bug in type checker! Wrong type: expected {} but found {}",
            expected,
            self.0.type_of()
        )
    }

    pub fn into_literal(self) -> Literal {
        self.0.into_literal()
    }
}

impl ValueCase {
    fn type_of(self) -> Type {
        use ValueCase::*;

        match self {
            Unit => Type::Unit,
            Bool(_) => Type::Bool,
            Int(_) => Type::Int,
        }
    }

    fn into_literal(self) -> Literal {
        use ValueCase::*;

        match self {
            Unit => Literal::Unit,
            Bool(b) => Literal::Bool(b),
            Int(n) => Literal::Int(n),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueCase::*;

        match self.0 {
            Unit => write!(f, "()"),
            Bool(b) => write!(f, "{}", b),
            Int(n) => write!(f, "{}", n),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Literal::*;

        match self {
            Unit => write!(f, "()"),
            Bool(b) => write!(f, "{}", b),
            Int(n) => write!(f, "{}", n),
        }
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Time::*;

        match self {
            Runtime => write!(f, "runtime"),
            Comptime => write!(f, "comptime"),
        }
    }
}

impl Add for Time {
    type Output = Time;

    fn add(self, other: Time) -> Time {
        use Time::*;

        match (self, other) {
            (Runtime, Runtime) => Runtime,
            (Comptime, _) | (_, Comptime) => Comptime,
        }
    }
}
