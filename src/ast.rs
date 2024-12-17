use crate::type_checker::Type;
use std::fmt;

/*******
 * AST *
 *******/

pub type Span = panfix::Span;

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i32),
    #[allow(non_camel_case_types)]
    Binop_II_I(Binop_II_I, Box<(Expr, Span)>, Box<(Expr, Span)>),
}

/// Binary operator from (int, int) to int.
#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum Binop_II_I {
    Add,
    Sub,
    Mul,
    Div,
}

/// Precedence level. Smaller precedence binds tighter / wins.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Prec(u16);

impl Prec {
    pub const MAX: Prec = Prec(u16::MAX);
}

/* Will be needed later
impl Expr {
    pub fn prec(&self) -> Prec {
        use Expr::*;

        match self {
            Int(_) => Prec(0),
            Binop_II_I(binop, _, _) => binop.prec(),
        }
    }
}
*/

impl Binop_II_I {
    pub fn prec(&self) -> Prec {
        use Binop_II_I::*;

        let prec = match self {
            Add | Sub => 20,
            Mul | Div => 10,
        };
        Prec(prec)
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
    Int(i32),
}

impl Value {
    pub fn int(int: i32) -> Value {
        Value(ValueCase::Int(int))
    }

    pub fn unwrap_int(self) -> i32 {
        #[allow(irrefutable_let_patterns)] // Will be refutable in the future
        if let ValueCase::Int(int) = self.0 {
            int
        } else {
            self.type_error(Type::Int)
        }
    }

    fn type_error(self, expected: Type) -> ! {
        panic!(
            "Type checking bug. Wrong type: expected {} but found {}",
            expected,
            self.0.type_of()
        )
    }
}

impl ValueCase {
    fn type_of(self) -> Type {
        use ValueCase::*;

        match self {
            Int(_) => Type::Int,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueCase::*;

        match self.0 {
            Int(n) => write!(f, "{}", n),
        }
    }
}
