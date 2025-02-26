use crate::{
    ast::{self, Arg, OpCode, Type, TypeDecl},
    span::Span,
};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInfo(Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program(pub Vec<Item>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Type(TypeDecl),
    Func(FuncDecl),
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
pub enum Statement {
    Let {
        pattern: ast::Pat,
        expr: Expression,
        typ: Rc<Type>,
        span: Span,
    },
    Return {
        expr: Expression,
        typ: Rc<Type>,
        span: Span,
    },
    IfElse {
        cond: Expression,
        then: Block,
        otherwise: Block,
        span: Span,
    },
    Mutate {
        lhs: String,
        rhs: Expression,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Binop {
        left: Box<Expression>,
        right: Box<Expression>,
        op: OpCode,
        span: Span,
        typ: Rc<Type>,
    },
    New {
        inner: Box<Expression>,
        span: Span,
        typ: Rc<Type>,
    },
    Deref {
        inner: Box<Expression>,
        span: Span,
        typ: Rc<Type>,
    },
    Free {
        inner: Box<Expression>,
        span: Span,
        typ: Rc<Type>,
    },
    Tuple {
        elems: Vec<Expression>,
        span: Span,
        typ: Rc<Type>,
    },
    Call {
        func: Box<Expression>,
        args: Vec<Expression>,
        span: Span,
        typ: Rc<Type>,
    },
    Var {
        name: String,
        span: Span,
        typ: Rc<Type>,
    },
    Int {
        value: i64,
        span: Span,
        typ: Rc<Type>,
    },
    Bool {
        value: bool,
        span: Span,
        typ: Rc<Type>,
    },
}

impl Expression {
    fn as_source(&self) -> ast::Expression {
        match self {
            Expression::Binop {
                left,
                right,
                op,
                span,
                ..
            } => ast::Expression::Binop {
                left: Box::new(left.as_source()),
                right: Box::new(right.as_source()),
                op: *op,
                span: *span,
            },
            Expression::New { inner, span, .. } => ast::Expression::New {
                inner: Box::new(inner.as_source()),
                span: *span,
            },
            Expression::Deref { inner, span, .. } => ast::Expression::Deref {
                inner: Box::new(inner.as_source()),
                span: *span,
            },
            Expression::Free { inner, span, .. } => ast::Expression::Free {
                inner: Box::new(inner.as_source()),
                span: *span,
            },
            Expression::Tuple { elems, span, .. } => ast::Expression::Tuple {
                elems: elems.iter().map(|x| x.as_source()).collect(),
                span: *span,
            },
            Expression::Call {
                func, args, span, ..
            } => ast::Expression::Call {
                func: Box::new(func.as_source()),
                args: args.iter().map(|x| x.as_source()).collect(),
                span: *span,
            },
            Expression::Var { name, span, .. } => ast::Expression::Var {
                name: name.to_owned(),
                span: *span,
            },
            &Expression::Int { value, span, .. } => ast::Expression::Int { value, span },
            &Expression::Bool { value, span, .. } => ast::Expression::Bool { value, span },
        }
    }
}

impl Statement {
    fn as_source(&self) -> ast::Statement {
        match self {
            Statement::Let {
                pattern,
                expr,
                typ,
                span,
            } => ast::Statement::Let {
                pattern: pattern.to_owned(),
                expr: expr.as_source(),
                typ: (**typ).clone(),
                span: *span,
            },
            Statement::Return { expr, span, .. } => ast::Statement::Return {
                expr: expr.as_source(),
                span: *span,
            },
            Statement::IfElse {
                cond,
                then,
                otherwise,
                span,
                ..
            } => ast::Statement::IfElse {
                cond: cond.as_source(),
                then: then.iter().map(|x| x.as_source()).collect(),
                otherwise: otherwise.iter().map(|x| x.as_source()).collect(),
                span: *span,
            },
            Statement::Mutate { lhs, rhs, span } => ast::Statement::Mutate {
                lhs: lhs.clone(),
                rhs: rhs.as_source(),
                span: *span,
            },
        }
    }
}

macro_rules! display_impl {
    ($t:ident) => {
        impl std::fmt::Display for $t {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.as_source().fmt(f)
            }
        }
    };
}

display_impl!(Expression);
display_impl!(Statement);

impl Expression {
    pub fn typ(&self) -> Rc<Type> {
        match self {
            Expression::Binop { typ, .. }
            | Expression::New { typ, .. }
            | Expression::Deref { typ, .. }
            | Expression::Free { typ, .. }
            | Expression::Call { typ, .. }
            | Expression::Tuple { typ, .. }
            | Expression::Var { typ, .. }
            | Expression::Int { typ, .. }
            | Expression::Bool { typ, .. } => Rc::clone(typ),
        }
    }
}

