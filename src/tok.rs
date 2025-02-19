use crate::span::{Pos, Span};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    span: Span,
    kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }

    pub fn new_from_str(start: Pos, s: &str) -> Self {
        let kind = match s {
            "ref" => TokenKind::Ref,
            "fn" => TokenKind::Fn,
            "type" => TokenKind::Type,
            "let" => TokenKind::Let,
            "new" => TokenKind::New,
            "return" => TokenKind::Return,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "=" => TokenKind::Eq,
            "<>" => TokenKind::Neq,
            "," => TokenKind::Comma,
            ":" => TokenKind::Colon,
            ";" => TokenKind::Semicolon,
            "+" => TokenKind::Plus,
            "-" => TokenKind::Minus,
            "*" => TokenKind::Times,
            "/" => TokenKind::Div,
            "->" => TokenKind::Arrow,
            "(" => TokenKind::OpenParen,
            ")" => TokenKind::CloseParen,
            "{" => TokenKind::OpenBrace,
            "}" => TokenKind::CloseBrace,
            _ => TokenKind::Ident(s.to_owned()),
        };

        let end_col = start.col + s.len() as u32;
        let end = Pos::new(start.line, end_col);

        Token {
            span: Span::new(start, end),
            kind,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    Ident(String),
    Int(i64),
    Error(String),
    Ref,
    Fn,
    Type,
    Let,
    Eq,
    Neq,
    Comma,
    Colon,
    Arrow,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Plus,
    Minus,
    Times,
    Div,
    New,
    If,
    Else,
    Return,
    False,
    True,
}

impl TokenKind {
    pub fn matches_kind(&self, other: &TokenKind) -> bool {
        use TokenKind::*;
        match self {
            Ident(_) => matches!(other, Ident(_)),
            Error(_) => matches!(other, Error(_)),
            Int(_) => matches!(other, Int(_)),
            _ => self == other,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    s: &'a str,
    line: u32,
    col: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self { s, line: 1, col: 1 }
    }

    fn skip_whitespace(&mut self) {
        let end = self
            .s
            .char_indices()
            .find(|(_, ch)| !ch.is_whitespace())
            .map(|(i, _)| i)
            .unwrap_or(self.s.len());
        let whitespace = &self.s[..end];
        let lines = whitespace.chars().filter(|&ch| ch == '\n').count();
        let col = whitespace
            .chars()
            .rev()
            .take_while(|&ch| ch != '\n')
            .count();

        self.line += lines as u32;
        if lines > 0 {
            self.col = col as u32 + 1;
        } else {
            self.col += col as u32;
        }

        self.s = &self.s[end..];
    }

    fn split_at(&mut self, end: usize) -> &str {
        let s = &self.s[..end];
        self.s = &self.s[end..];
        self.col += s.chars().count() as u32;
        s
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic()
}
fn is_ident_cont(c: char) -> bool {
    is_ident_start(c) || c.is_ascii_digit()
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        let start = Pos::new(self.line, self.col);

        let head = self.s.chars().next()?;

        match head {
            _ if is_ident_start(head) => {
                let end = self
                    .s
                    .char_indices()
                    .find(|&(_, ch)| !is_ident_cont(ch))
                    .map(|(i, _)| i)
                    .unwrap_or(self.s.len());
                let s = self.split_at(end);
                return Some(Token::new_from_str(start, s));
            }
            '0'..='9' => {
                let end = self
                    .s
                    .char_indices()
                    .find(|&(_, ch)| !ch.is_ascii_digit())
                    .map(|(i, _)| i)
                    .unwrap_or(self.s.len());
                let s = self.split_at(end);
                match s.parse::<i64>() {
                    Ok(i) => {
                        let span = Span::new(start, start.add_col(s.len() as _));
                        return Some(Token::new(span, TokenKind::Int(i)));
                    }
                    Err(_) => {
                        return Some(Token::new(
                            Span::new(start, start.add_col(s.len() as _)),
                            TokenKind::Error(s.to_owned()),
                        ));
                    }
                }
            }

            '-' => {
                let after = self.s[1..].chars().next();
                if after == Some('>') {
                    let s = self.split_at(2);
                    return Some(Token::new_from_str(start, s));
                }
                let s = self.split_at(1);
                return Some(Token::new_from_str(start, s));
            }

            '<' => {
                let after = self.s[1..].chars().next();
                if after == Some('>') {
                    let s = self.split_at(2);
                    return Some(Token::new_from_str(start, s));
                }
            }

            '=' | '{' | '}' | '(' | ')' | ',' | ':' | ';' | '+' | '*' | '/' => {
                let s = self.split_at(1);
                return Some(Token::new_from_str(start, s));
            }

            _ => {}
        }

        let s = self.split_at(head.len_utf8());
        Some(Token::new(
            Span::new(start, start.add_col(1)),
            TokenKind::Error(s.to_owned()),
        ))
    }
}
