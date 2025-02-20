use std::rc::Rc;

use crate::{ast::Type, span::Span};

#[derive(Debug, PartialEq, Eq)]
pub struct Builtin {
    name: String,
    link_name: Option<String>,
    pub args: Vec<Type>,
    pub ret_typ: Type,
}

impl Builtin {
    pub fn print_int() -> Builtin {
        Builtin {
            name: "print_int".into(),
            link_name: None,
            args: vec![Type::int()],
            ret_typ: Type::Product(vec![], Span::empty()),
        }
    }

    pub fn print_ch() -> Builtin {
        Builtin {
            name: "print_ch".into(),
            link_name: None,
            args: vec![Type::int()],
            ret_typ: Type::Product(vec![], Span::empty()),
        }
    }

    pub fn link_name(&self) -> &str {
        if let Some(link_name) = &self.link_name {
            link_name
        } else {
            &self.name
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn fn_type(&self) -> Type {
        Type::Fun(
            self.args.to_owned(),
            Rc::new(self.ret_typ.to_owned()),
            Span::empty(),
        )
    }
}

pub fn builtins() -> Vec<Builtin> {
    vec![Builtin::print_int(), Builtin::print_ch()]
}
