use std::rc::Rc;

use crate::{
    ast::{self, Arg, OpCode, Type, TypeDecl},
    span::Span,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeInfo(Type);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program(pub Vec<Item>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Type(TypeDecl),
    Func(FuncDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDecl {
    pub name: String,
    pub args: Vec<Arg>,
    pub ret_typ: Type,
    pub body: Vec<Statement>,
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
            Expression::Int { value, span, .. } => ast::Expression::Int {
                value: *value,
                span: *span,
            },
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
            | Expression::Call { typ, .. }
            | Expression::Tuple { typ, .. }
            | Expression::Var { typ, .. }
            | Expression::Int { typ, .. } => Rc::clone(typ),
        }
    }
}
