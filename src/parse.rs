use std::rc::Rc;

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

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("{0}")]
    Msg(String),
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub struct ParseError {
    cur_sym: Option<Token>,
    kind: ParseErrorKind,
}

pub type Result<T> = std::result::Result<T, ParseError>;

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(span) = self.cur_sym.as_ref().map(|x| x.span()) {
            write!(f, "{span}: ")?;
        }

        write!(f, "{} ", self.kind)?;

        if let Some(cur_sym) = &self.cur_sym {
            write!(f, "current token is {:?}", cur_sym.kind())?;
        } else {
            write!(f, "current token is EOF")?;
        }

        Ok(())
    }
}

macro_rules! accept_data {
    ($accept:ident, $expect:ident, $t:ty, $pat:path) => {
        #[allow(unused)]
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

        #[allow(unused)]
        fn $expect(&mut self) -> Result<($t, Span)> {
            if let Some(x) = self.$accept() {
                return Ok(x);
            }
            Err(self.error("expect: unexpected symbol"))
        }
    };
}

impl<'a> Parser<'a> {
    pub fn new(mut lex: Lexer<'a>) -> Self {
        let sym = lex.next();
        Self { lex, sym }
    }

    fn error(&self, s: &str) -> ParseError {
        let sym_str = self
            .sym
            .as_ref()
            .map(|x| format!("{:?} at {:?}", x.kind(), x.span()))
            .unwrap_or_else(|| String::from("EOF"));
        println!("{s} current token is {}", sym_str);
        ParseError {
            cur_sym: self.sym.clone(),
            kind: ParseErrorKind::Msg(s.to_owned()),
        }
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

    fn expect(&mut self, s: TokenKind) -> Result<Token> {
        let k = s.clone();
        if let Some(tok) = self.accept(s) {
            Ok(tok)
        } else {
            Err(self.error(&format!("expect: expected {k:?}")))
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
        fn $name(&mut self) -> Result<Vec<$t>> {
            let after = $after;
            if after(self) {
                return Ok(vec![]);
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
            Ok(res)
        }
    };
}

impl Parser<'_> {
    fn type_decl(&mut self, start: Span) -> Result<TypeDecl> {
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::Eq)?;
        let typ = self.typ()?;
        Ok(TypeDecl {
            span: start.merge(typ.span()),
            name,
            typ,
        })
    }

    fn typ(&mut self) -> Result<Type> {
        if let Some((var, span)) = self.accept_ident() {
            Ok(Type::Var(var, span))
        } else if let Some(tok) = self.accept(TokenKind::Ref) {
            let typ = self.typ()?;
            let span = tok.span().merge(typ.span());
            Ok(Type::Ref(Rc::new(typ), span))
        } else if let Some(tok) = self.accept(TokenKind::OpenParen) {
            let typs = self.comma_sep_types()?;
            let end = self.expect(TokenKind::CloseParen)?;
            let span = tok.span().merge(end.span());
            Ok(Type::Product(typs, span))
        } else {
            Err(self.error("type: expected VAR, '(', 'ref'"))
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

    fn arg(&mut self) -> Result<Arg> {
        let (var, span) = self.expect_ident()?;
        self.expect(TokenKind::Colon)?;
        let typ = self.typ()?;
        let span = span.merge(typ.span());
        Ok(Arg { var, typ, span })
    }

    fn fn_decl(&mut self, start: Span) -> Result<FuncDecl> {
        let (name, _) = self.expect_ident()?;
        self.expect(TokenKind::OpenParen)?;
        let args = self.comma_sep_args()?;
        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::Arrow)?;
        let ret_typ = self.typ()?;
        let (body, body_span) = self.block()?;
        Ok(FuncDecl {
            name,
            args,
            ret_typ,
            body,
            span: start.merge(body_span),
        })
    }

    fn block(&mut self) -> Result<(Vec<Statement>, Span)> {
        let open = self.expect(TokenKind::OpenBrace)?;
        let mut statements = vec![];
        while !self.peek_close_brace() {
            let stmt = self.statement()?;
            statements.push(stmt);
        }
        let close = self.expect(TokenKind::CloseBrace)?;
        Ok((statements, open.span().merge(close.span())))
    }

    fn pattern(&mut self) -> Result<Pat> {
        if let Some((id, span)) = self.accept_ident() {
            Ok(Pat::Single(id, span))
        } else if let Some(open) = self.accept(TokenKind::OpenParen) {
            let patterns = self.comma_sep_pat()?;
            let close = self.expect(TokenKind::CloseParen)?;
            Ok(Pat::Tuple(patterns, open.span().merge(close.span())))
        } else {
            Err(self.error("pattern: syntax error"))
        }
    }

    fn statement(&mut self) -> Result<Statement> {
        if let Some(let_tok) = self.accept(TokenKind::Let) {
            let pattern = self.pattern()?;
            self.expect(TokenKind::Colon)?;
            let typ = self.typ()?;
            self.expect(TokenKind::Eq)?;
            let expr = self.expression()?;
            let semi = self.expect(TokenKind::Semicolon)?;
            Ok(Statement::Let {
                pattern,
                expr,
                typ,
                span: let_tok.span().merge(semi.span()),
            })
        } else if let Some(ret) = self.accept(TokenKind::Return) {
            let expr = self.expression()?;
            let semi = self.expect(TokenKind::Semicolon)?;

            Ok(Statement::Return {
                expr,
                span: ret.span().merge(semi.span()),
            })
        } else if let Some(if_tok) = self.accept(TokenKind::If) {
            let condition = self.expression()?;
            let (then, _) = self.block()?;
            self.expect(TokenKind::Else)?;
            let (otherwise, span) = self.block()?;
            Ok(Statement::IfElse {
                cond: condition,
                then,
                otherwise,
                span: if_tok.span().merge(span),
            })
        } else if let Some((lhs, span)) = self.accept_ident() {
            self.expect(TokenKind::Gets)?;
            let rhs = self.expression()?;
            let semi = self.expect(TokenKind::Semicolon)?;
            Ok(Statement::Mutate {
                lhs,
                rhs,
                span: span.merge(semi.span()),
            })
        } else {
            Err(self.error("statement: syntax error"))
        }
    }

    fn expression(&mut self) -> Result<Expression> {
        self.comparison()
    }

    fn comparison(&mut self) -> Result<Expression> {
        let left = self.sum()?;
        if self.accept(TokenKind::Eq).is_some() {
            let right = self.sum()?;
            Ok(Expression::Binop {
                span: left.span().merge(right.span()),
                left: Box::new(left),
                right: Box::new(right),
                op: OpCode::Eq,
            })
        } else if self.accept(TokenKind::Neq).is_some() {
            let right = Box::new(self.sum()?);
            Ok(Expression::Binop {
                span: left.span().merge(right.span()),
                left: Box::new(left),
                right,
                op: OpCode::Neq,
            })
        } else {
            Ok(left)
        }
    }

    fn sum(&mut self) -> Result<Expression> {
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
        Ok(e)
    }

    fn term(&mut self) -> Result<Expression> {
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
        Ok(e)
    }

    fn atom(&mut self) -> Result<Expression> {
        let res = if let Some(open) = self.accept(TokenKind::OpenParen) {
            let exprs = self.comma_sep_expr()?;
            let close = self.expect(TokenKind::CloseParen)?;

            if exprs.len() == 1 {
                let mut exprs = exprs;
                exprs.pop().unwrap()
            } else {
                Expression::Tuple {
                    elems: exprs,
                    span: open.span().merge(close.span()),
                }
            }
        } else if let Some((name, span)) = self.accept_ident() {
            Expression::Var { name, span }
        } else if let Some((value, span)) = self.accept_int() {
            Expression::Int { value, span }
        } else if let Some(t) = self.accept(TokenKind::True) {
            Expression::Bool {
                value: true,
                span: t.span(),
            }
        } else if let Some(t) = self.accept(TokenKind::False) {
            Expression::Bool {
                value: false,
                span: t.span(),
            }
        } else if let Some(new) = self.accept(TokenKind::New) {
            let e = self.atom()?;
            Expression::New {
                span: new.span().merge(e.span()),
                inner: Box::new(e),
            }
        } else if let Some(new) = self.accept(TokenKind::Free) {
            let e = self.atom()?;
            Expression::Free {
                span: new.span().merge(e.span()),
                inner: Box::new(e),
            }
        } else if let Some(star) = self.accept(TokenKind::Times) {
            let e = self.atom()?;
            Expression::Deref {
                span: star.span().merge(e.span()),
                inner: Box::new(e),
            }
        } else {
            return Err(self.error("expression: expected one of [OpenParen, Ident, Int, New, If]"));
        };

        if self.accept(TokenKind::OpenParen).is_some() {
            let args = self.comma_sep_expr()?;
            let end = self.expect(TokenKind::CloseParen)?;
            Ok(Expression::Call {
                span: res.span().merge(end.span()),
                func: Box::new(res),
                args,
            })
        } else {
            Ok(res)
        }
    }

    pub fn program(&mut self) -> Result<Program> {
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
                return Err(self.error("program: syntax error"));
            }
        }

        Ok(Program(items))
    }
}
