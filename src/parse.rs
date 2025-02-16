use std::cell::LazyCell;

use crate::{
    ast::{
        Arg, Expression, FuncDecl, Item, OpCode, Pat, Program, Spanned, Statement, Type, TypeDecl,
    },
    span::Span,
    tok::{Lexer, Token, TokenKind},
};

pub struct Parser<'a> {
    lex: Lexer<'a>,
    sym: Option<Token>,
}

macro_rules! accept_data {
    ($accept:ident, $expect:ident, $t:ty, $pat:path) => {
        fn $accept(&mut self) -> Option<($t, Span)> {
            let tok = self.sym.take()?;
            let span = tok.span();
            match tok.kind() {
                $pat(x) => {
                    self.advance();
                    Some((x.to_owned(), span))
                }
                _ => {
                    self.sym = Some(tok);
                    None
                }
            }
        }

        fn $expect(&mut self) -> Option<($t, Span)> {
            if let Some(x) = self.$accept() {
                return Some(x);
            }
            self.error("expect: unexpected symbol");
            None
        }
    };
}

impl<'a> Parser<'a> {
    pub fn new(mut lex: Lexer<'a>) -> Self {
        let sym = lex.next();
        Self { lex, sym }
    }

    fn error(&self, s: &str) {
        let sym_str = self
            .sym
            .as_ref()
            .map(|x| format!("{:?}", x.kind()))
            .unwrap_or_else(|| String::from("EOF"));
        println!("{s} current token is {}", sym_str);
    }

    fn advance(&mut self) {
        self.sym = self.lex.next();
    }

    fn accept_eof(&mut self) -> bool {
        self.sym.is_none()
    }

    fn accept(&mut self, s: TokenKind) -> Option<Token> {
        let tok = self.sym.take()?;
        if tok.kind().matches_kind(&s) {
            self.advance();
            return Some(tok);
        }
        self.sym = Some(tok);

        None
    }

    accept_data!(accept_ident, expect_ident, String, TokenKind::Ident);
    accept_data!(accept_int, expect_int, i64, TokenKind::Int);

    fn expect(&mut self, s: TokenKind) -> Option<Token> {
        let k = s.clone();
        if let Some(tok) = self.accept(s) {
            Some(tok)
        } else {
            self.error(&format!("expect: expected {k:?}"));
            None
        }
    }

    fn expect_eof(&mut self) -> bool {
        if self.accept_eof() {
            true
        } else {
            self.error("expect: expected eof");
            false
        }
    }
}

impl Parser<'_> {
    fn peek_close_paren(&self) -> bool {
        self.sym
            .as_ref()
            .is_some_and(|x| matches!(x.kind(), TokenKind::CloseParen))
    }

    fn peek_close_brace(&self) -> bool {
        self.sym
            .as_ref()
            .is_some_and(|x| matches!(x.kind(), TokenKind::CloseBrace))
    }
}

macro_rules! comma_sep {
    ($name:ident, $single:ident, $after:expr, $t:ty) => {
        fn $name(&mut self) -> Option<Vec<$t>> {
            let after = $after;
            if after(self) {
                return Some(vec![]);
            }

            let first = self.$single()?;
            let mut res = vec![first];
            while self.accept(TokenKind::Comma).is_some() {
                if after(self) {
                    break;
                }
                let t = self.$single()?;
                res.push(t);
            }
            Some(res)
        }
    };
}

impl Parser<'_> {
    fn type_decl(&mut self, start: Span) -> Option<TypeDecl> {
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Eq)?;
        let typ = self.typ()?;
        Some(TypeDecl {
            span: start.merge(typ.span()),
            name,
            typ,
        })
    }

    fn typ(&mut self) -> Option<Type> {
        if let Some((var, span)) = self.accept_ident() {
            Some(Type::Var(var, span))
        } else if let Some(tok) = self.accept(TokenKind::Ref) {
            let typ = self.typ()?;
            let span = tok.span().merge(typ.span());
            Some(Type::Ref(Box::new(typ), span))
        } else if let Some(tok) = self.accept(TokenKind::OpenParen) {
            let typs = self.comma_sep_types()?;
            let end = self.expect(TokenKind::CloseParen)?;
            let span = tok.span().merge(end.span());
            Some(Type::Product(typs, span))
        } else {
            self.error("type: expected VAR, '(', 'ref'");
            None
        }
    }

    comma_sep!(comma_sep_types, typ, |x: &Self| x.peek_close_paren(), Type);
    comma_sep!(comma_sep_args, arg, |x: &Self| x.peek_close_paren(), Arg);
    comma_sep!(comma_sep_pat, pattern, |x: &Self| x.peek_close_paren(), Pat);
    comma_sep!(
        comma_sep_expr,
        expression,
        |x: &Self| x.peek_close_paren(),
        Expression
    );

    fn arg(&mut self) -> Option<Arg> {
        let (var, span) = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let typ = self.typ()?;
        let span = span.merge(typ.span());
        Some(Arg { var, typ, span })
    }

    fn fn_decl(&mut self, start: Span) -> Option<FuncDecl> {
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::OpenParen)?;
        let args = self.comma_sep_args()?;
        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::Arrow)?;
        let ret_typ = self.typ()?;
        let (body, body_span) = self.block()?;
        Some(FuncDecl {
            name,
            args,
            ret_typ,
            body,
            span: start.merge(body_span),
        })
    }

    fn block(&mut self) -> Option<(Vec<Statement>, Span)> {
        let open = self.expect(TokenKind::OpenBrace)?;
        let mut statements = vec![];
        while !self.peek_close_brace() {
            let stmt = self.statement()?;
            statements.push(stmt);
        }
        let close = self.expect(TokenKind::CloseBrace)?;
        Some((statements, open.span().merge(close.span())))
    }

    fn pattern(&mut self) -> Option<Pat> {
        if let Some((id, span)) = self.accept_ident() {
            Some(Pat::Single(id, span))
        } else if let Some(open) = self.accept(TokenKind::OpenParen) {
            let patterns = self.comma_sep_pat()?;
            let close = self.expect(TokenKind::CloseParen)?;
            Some(Pat::Tuple(patterns, open.span().merge(close.span())))
        } else {
            self.error("pattern: syntax error");
            None
        }
    }

    fn statement(&mut self) -> Option<Statement> {
        if let Some(let_tok) = self.accept(TokenKind::Let) {
            let pattern = self.pattern()?;
            self.expect(TokenKind::Colon)?;
            let typ = self.typ()?;
            self.expect(TokenKind::Eq)?;
            let expr = self.expression()?;
            let semi = self.expect(TokenKind::Semicolon)?;
            Some(Statement::Let {
                pattern,
                expr,
                typ,
                span: let_tok.span().merge(semi.span()),
            })
        } else if let Some(ret) = self.accept(TokenKind::Return) {
            let expr = self.expression()?;
            let semi = self.expect(TokenKind::Semicolon)?;

            Some(Statement::Return {
                expr,
                span: ret.span().merge(semi.span()),
            })
        } else {
            self.error("statement: syntax error");
            None
        }
    }

    fn expression(&mut self) -> Option<Expression> {
        let mut e = self.term()?;
        loop {
            let opcode = if self.accept(TokenKind::Plus).is_some() {
                OpCode::Add
            } else if self.accept(TokenKind::Minus).is_some() {
                OpCode::Sub
            } else {
                break;
            };
            let t = self.term()?;
            e = Expression::Binop {
                span: e.span().merge(t.span()),
                left: Box::new(e),
                right: Box::new(t),
                op: opcode,
            }
        }
        Some(e)
    }

    fn term(&mut self) -> Option<Expression> {
        let mut e = self.atom()?;
        loop {
            let opcode = if self.accept(TokenKind::Times).is_some() {
                OpCode::Mul
            } else if self.accept(TokenKind::Div).is_some() {
                OpCode::Div
            } else {
                break;
            };
            let t = self.term()?;
            e = Expression::Binop {
                span: e.span().merge(t.span()),
                left: Box::new(e),
                right: Box::new(t),
                op: opcode,
            }
        }
        Some(e)
    }

    fn atom(&mut self) -> Option<Expression> {
        let res = if self.accept(TokenKind::OpenParen).is_some() {
            let e = self.expression()?;
            self.expect(TokenKind::CloseParen)?;
            Some(e)
        } else if let Some((name, span)) = self.accept_ident() {
            Some(Expression::Var { name, span })
        } else if let Some((value, span)) = self.accept_int() {
            Some(Expression::Int { value, span })
        } else if let Some(new) = self.expect(TokenKind::New) {
            let e = self.expression()?;
            Some(Expression::New {
                span: new.span().merge(e.span()),
                inner: Box::new(e),
            })
        } else {
            self.error("expression: unexpected char");
            None
        }?;

        if self.accept(TokenKind::OpenParen).is_some() {
            let args = self.comma_sep_expr()?;
            let end = self.expect(TokenKind::CloseParen)?;
            Some(Expression::Call {
                span: res.span().merge(end.span()),
                func: Box::new(res),
                args,
            })
        } else {
            Some(res)
        }
    }

    pub fn program(&mut self) -> Option<Program> {
        let mut items = vec![];
        loop {
            if let Some(t0) = self.accept(TokenKind::Type) {
                let td = self.type_decl(t0.span())?;
                items.push(Item::Type(td));
            } else if let Some(t0) = self.accept(TokenKind::Fn) {
                let fun = self.fn_decl(t0.span())?;
                items.push(Item::Func(fun));
            } else if self.expect_eof() {
                break;
            } else {
                self.error("program: syntax error");
                return None;
            }
        }

        Some(Program(items))
    }
}
