use crate::type_checker::Type;
use std::fmt;

/*******
 * AST *
 *******/

pub type Span = panfix::Span;

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    Int(i32),
    Unop(Unop, Box<(Expr, Span)>),
    Binop(Binop, Box<(Expr, Span)>, Box<(Expr, Span)>),
    If(Box<(Expr, Span)>, Box<(Expr, Span)>, Box<(Expr, Span)>),
}

#[derive(Debug, Clone, Copy)]
pub enum Unop {
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

/// Precedence level. Smaller precedence binds tighter / wins.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
            Lt | Le | Gt | Ge => 30,
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

/// A runtime value. The Value knows its type, but it will not tell you because:
///
/// - A full implementation would compile to assembly and not know the underlying type, except as
///   revealed by the type checker.
/// - The value does store its own type to notice at runtime if the typechecker messed up.
#[derive(Debug, Clone)]
pub struct Value(ValueCase);

#[derive(Debug, Clone)]
enum ValueCase {
    Bool(bool),
    Int(i32),
}

impl Value {
    pub fn bool(b: bool) -> Value {
        Value(ValueCase::Bool(b))
    }

    pub fn int(int: i32) -> Value {
        Value(ValueCase::Int(int))
    }

    pub fn unwrap_bool(self) -> bool {
        if let ValueCase::Bool(b) = self.0 {
            b
        } else {
            self.type_error(Type::Bool)
        }
    }

    pub fn unwrap_int(self) -> i32 {
        if let ValueCase::Int(int) = self.0 {
            int
        } else {
            self.type_error(Type::Int)
        }
    }

    fn type_error(self, expected: Type) -> ! {
        panic!(
            "Type checking bug! Wrong type: expected {} but found {}",
            expected,
            self.0.type_of()
        )
    }
}

impl ValueCase {
    fn type_of(self) -> Type {
        use ValueCase::*;

        match self {
            Bool(_) => Type::Bool,
            Int(_) => Type::Int,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueCase::*;

        match self.0 {
            Bool(b) => write!(f, "{}", b),
            Int(n) => write!(f, "{}", n),
        }
    }
}
