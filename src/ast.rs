use crate::type_checker::Type;
use std::fmt;

/*******
 * AST *
 *******/

pub type Span = panfix::Span;

pub type FuncId = usize;

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
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub enum Expr {
    Var(VarRefn),
    Unit,
    Bool(bool),
    Int(i32),
    Unop(Unop, Box<(Expr, Span)>),
    Binop(Binop, Box<(Expr, Span)>, Box<(Expr, Span)>),
    If(Box<(Expr, Span)>, Box<(Expr, Span)>, Box<(Expr, Span)>),
    Apply((FuncRefn, Span), Vec<(Expr, Span)>),
    Block((Block, Span)),
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
#[derive(Debug)]
pub struct VarRefn {
    pub name: String,
    // Filled out during type checking
    pub depth: Option<usize>,
    pub offset: Option<isize>,
}

#[derive(Debug)]
pub struct FuncRefn {
    pub name: String,
    // Filled out during type checking
    pub depth: Option<usize>,
    pub id: Option<FuncId>,
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
        self.depth
            .expect("FuncRefn.id not set during type checking")
    }
}

impl VarRefn {
    pub fn new(name: &str) -> VarRefn {
        VarRefn {
            name: name.to_owned(),
            // Will be filled out during type checking
            depth: None,
            offset: None,
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

/// A runtime value. The Value knows its type, but it will not tell you because:
///
/// - A full implementation would compile to assembly and not know the underlying type, except as
///   revealed by the type checker.
/// - The value does store its own type to notice at runtime if the typechecker messed up.
#[derive(Debug, Clone)]
pub struct Value<'s>(ValueCase<'s>);

#[derive(Debug, Clone)]
enum ValueCase<'s> {
    Unit,
    Bool(bool),
    Int(i32),
    FuncPtr(&'s FuncStmt),
}

impl<'s> Value<'s> {
    pub fn unit() -> Value<'s> {
        Value(ValueCase::Unit)
    }

    pub fn bool(b: bool) -> Value<'s> {
        Value(ValueCase::Bool(b))
    }

    pub fn int(int: i32) -> Value<'s> {
        Value(ValueCase::Int(int))
    }

    pub fn func_ptr(func: &'s FuncStmt) -> Value<'s> {
        Value(ValueCase::FuncPtr(func))
    }

    pub fn unwrap_unit(self) {
        if !matches!(self.0, ValueCase::Unit) {
            self.type_mismatch(Some(&Type::Unit))
        }
    }

    pub fn unwrap_bool(self) -> bool {
        if let ValueCase::Bool(b) = self.0 {
            b
        } else {
            self.type_mismatch(Some(&Type::Bool))
        }
    }

    pub fn unwrap_int(self) -> i32 {
        if let ValueCase::Int(int) = self.0 {
            int
        } else {
            self.type_mismatch(Some(&Type::Int))
        }
    }

    pub fn unwrap_func_ptr(self) -> &'s FuncStmt {
        if let ValueCase::FuncPtr(func) = self.0 {
            func
        } else {
            self.type_mismatch(None)
        }
    }

    fn type_mismatch(self, expected: Option<&Type>) -> ! {
        let actual = match self.0.type_of() {
            None => "function".to_owned(),
            Some(ty) => ty.to_string(),
        };
        let expected = match expected {
            None => "function".to_owned(),
            Some(ty) => ty.to_string(),
        };
        panic!(
            "Bug in type checker! Wrong type: expected {} but found {}",
            expected, actual,
        )
    }
}

impl ValueCase<'_> {
    fn type_of(self) -> Option<Type> {
        use ValueCase::*;

        match self {
            Unit => Some(Type::Unit),
            Bool(_) => Some(Type::Bool),
            Int(_) => Some(Type::Int),
            FuncPtr(_) => None,
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ValueCase::*;

        match self.0 {
            Unit => write!(f, "()"),
            Bool(b) => write!(f, "{}", b),
            Int(n) => write!(f, "{}", n),
            FuncPtr(func) => write!(f, "func {}", func.var.name),
        }
    }
}
