use std::{borrow::Cow, fmt::Display};

use crate::span::Span;

pub mod ast;
pub mod check;
pub mod infer;

pub type Result<T> = std::result::Result<T, TypeError>;

#[derive(Debug, Clone)]
pub struct TypeError {
    msg: Cow<'static, str>,
    span: Span,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}: {}", self.span, self.msg)
    }
}

