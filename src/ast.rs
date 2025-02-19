use std::{
    fmt::{Display, Formatter, Result},
    rc::Rc,
};

use crate::span::Span;

pub trait Spanned {
    fn span(&self) -> Span;
}

macro_rules! spanned {
    ($n:ident) => {
        impl Spanned for $n {
            fn span(&self) -> Span {
                self.span
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program(pub Vec<Item>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Type(TypeDecl),
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl {
    pub name: String,
    pub typ: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Var(String, Span),
    Ref(Rc<Type>, Span),
    Product(Vec<Type>, Span),
    Fun(Vec<Type>, Rc<Type>, Span),
}

impl Spanned for Type {
    fn span(&self) -> Span {
        match self {
            Type::Var(_, span)
            | Type::Ref(_, span)
            | Type::Product(_, span)
            | Type::Fun(_, _, span) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Arg {
    pub var: String,
    pub typ: Type,
    pub span: Span,
}

pub type Block = Vec<Statement>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDecl {
    pub name: String,
    pub args: Vec<Arg>,
    pub ret_typ: Type,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Single(String, Span),
    Tuple(Vec<Pat>, Span),
}

impl Spanned for Pat {
    fn span(&self) -> Span {
        match self {
            Pat::Single(_, span) | Pat::Tuple(_, span) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let {
        pattern: Pat,
        expr: Expression,
        typ: Type,
        span: Span,
    },
    Return {
        expr: Expression,
        span: Span,
    },
    IfElse {
        cond: Expression,
        then: Block,
        otherwise: Block,
        span: Span,
    },
}

impl Spanned for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Let { span, .. }
            | Statement::Return { span, .. }
            | Statement::IfElse { span, .. } => *span,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum OpCode {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Binop {
        left: Box<Expression>,
        right: Box<Expression>,
        op: OpCode,
        span: Span,
    },
    New {
        inner: Box<Expression>,
        span: Span,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
        span: Span,
    },
    Tuple {
        elems: Vec<Expression>,
        span: Span,
    },
    Var {
        name: String,
        span: Span,
    },
    Int {
        value: i64,
        span: Span,
    },
    Bool {
        value: bool,
        span: Span,
    },
}

impl Spanned for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Binop { span, .. }
            | Expression::New { span, .. }
            | Expression::Call { span, .. }
            | Expression::Tuple { span, .. }
            | Expression::Int { span, .. }
            | Expression::Var { span, .. }
            | Expression::Bool { span, .. } => *span,
        }
    }
}

spanned!(Arg);
spanned!(FuncDecl);

fn comma_sep<T, F>(i: impl IntoIterator<Item = T>, fun: F, f: &mut Formatter<'_>) -> Result
where
    F: Fn(T, &mut Formatter<'_>) -> Result,
{
    let mut first = true;
    for item in i {
        if !first {
            write!(f, ", ")?;
        }
        first = false;
        fun(item, f)?;
    }
    Ok(())
}

impl Expression {
    fn display(&self, f: &mut Formatter<'_>, _prec: i32) -> Result {
        match self {
            Expression::Binop {
                left, right, op, ..
            } => {
                write!(f, "(")?;
                let prec = match op {
                    OpCode::Add | OpCode::Sub => 0,
                    OpCode::Mul | OpCode::Div => 1,
                };
                left.display(f, prec)?;
                match op {
                    OpCode::Add => write!(f, " + ")?,
                    OpCode::Sub => write!(f, " - ")?,
                    OpCode::Mul => write!(f, " * ")?,
                    OpCode::Div => write!(f, " / ")?,
                }
                right.display(f, prec)?;
                write!(f, ")")?;
                Ok(())
            }
            Expression::New { inner, .. } => {
                write!(f, "(")?;
                write!(f, "new")?;
                inner.display(f, 3)?;
                write!(f, ")")?;
                Ok(())
            }
            Expression::Call { func, args, .. } => {
                func.display(f, 3)?;
                write!(f, "(")?;
                comma_sep(args, |x, f| x.display(f, 3), f)?;
                write!(f, ")")?;
                Ok(())
            }
            Expression::Tuple { elems: args, .. } => {
                write!(f, "(")?;
                comma_sep(args, |x, f| x.display(f, 3), f)?;
                write!(f, ")")?;
                Ok(())
            }
            Expression::Var { name, .. } => write!(f, "{name}"),
            Expression::Int { value, .. } => write!(f, "{value}"),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.display(f, 0)
    }
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Pat::Single(v, _) => {
                write!(f, "{v}")
            }
            Pat::Tuple(pats, _) => {
                write!(f, "(")?;
                comma_sep(pats, |x, f| write!(f, "{x}"), f)?;
                write!(f, ")")
            }
        }
    }
}

impl Statement {
    fn display(&self, f: &mut Formatter<'_>, indent: u32) -> Result {
        for _ in 0..indent {
            write!(f, "    ")?;
        }
        match self {
            Statement::Let {
                pattern, expr, typ, ..
            } => {
                write!(f, "let {pattern}: {typ} = {expr};")?;
                Ok(())
            }
            Statement::IfElse {
                cond,
                then,
                otherwise,
                ..
            } => {
                writeln!(f, "if {cond} ")?;
                display_block(f, indent, then)?;
                for _ in 0..indent {
                    write!(f, "    ")?;
                }
                writeln!(f, "else ")?;
                display_block(f, indent, otherwise)?;
                for _ in 0..indent {
                    write!(f, "    ")?;
                }
                writeln!(f)?;
                Ok(())
            }
            Statement::Return { expr, .. } => write!(f, "return {expr};"),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.display(f, 0)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Type::Var(v, _) => write!(f, "{v}"),
            Type::Ref(inner, _) => write!(f, "ref {inner}"),
            Type::Product(items, _) => {
                write!(f, "(")?;
                comma_sep(items, |x, f| write!(f, "{x}"), f)?;
                write!(f, ")")?;
                Ok(())
            }
            Type::Fun(args, ret, _) => {
                write!(f, "(")?;
                comma_sep(args, |x, f| write!(f, "{x}"), f)?;
                write!(f, " -> {ret})")?;
                Ok(())
            }
        }
    }
}

impl Type {
    pub fn as_func(&self) -> Option<(&[Type], &Type, Span)> {
        match self {
            Type::Fun(items, res, span) => Some((items, res, *span)),
            _ => None,
        }
    }
}

impl Item {
    pub fn func(&self) -> Option<&FuncDecl> {
        match self {
            Item::Func(func_decl) => Some(func_decl),
            _ => None,
        }
    }

    pub fn type_decl(&self) -> Option<&TypeDecl> {
        match self {
            Item::Type(x) => Some(x),
            _ => None,
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        for item in &self.0 {
            writeln!(f, "{item}")?;
        }

        Ok(())
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Item::Type(type_decl) => write!(f, "{type_decl}"),
            Item::Func(func_decl) => write!(f, "{func_decl}"),
        }
    }
}

impl Display for TypeDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "type {} = {}", self.name, self.typ)
    }
}

fn display_block(f: &mut Formatter<'_>, indent: u32, block: &Block) -> Result {
    writeln!(f, "{{")?;
    for st in block {
        st.display(f, indent + 1)?;
        writeln!(f)?;
    }
    write!(f, "}}")?;
    Ok(())
}

impl Display for FuncDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "fn {}(", self.name)?;
        comma_sep(&self.args, |x, f| write!(f, "{}: {}", x.var, x.typ), f)?;
        writeln!(f, ") -> {} {{", self.ret_typ)?;
        for st in &self.body {
            st.display(f, 1)?;
            writeln!(f)?;
        }
        write!(f, "}}")?;

        Ok(())
    }
}
