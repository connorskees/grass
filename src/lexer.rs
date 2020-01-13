use std::convert::TryFrom;
use std::iter::Peekable;
use std::str::Chars;

use crate::common::{AtRule, Keyword, Op, Pos, Symbol};
use crate::selector::{Attribute, AttributeKind};
use crate::units::Unit;
use crate::{Token, TokenKind, Whitespace};

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    tokens: Vec<Token>,
    buf: Peekable<Chars<'a>>,
    pos: Pos,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! symbol {
            ($self:ident, $symbol:ident) => {{
                $self.buf.next();
                $self.pos.next_char();
                TokenKind::Symbol(Symbol::$symbol)
            }};
        }
        macro_rules! whitespace {
            ($self:ident, $whitespace:ident) => {{
                $self.buf.next();
                $self.pos.next_char();
                TokenKind::Whitespace(Whitespace::$whitespace)
            }};
        }
        let kind: TokenKind = match self.buf.peek().unwrap_or(&'\0') {
            'a'..='z' | 'A'..='Z' | '-' | '_' => self.lex_ident(),
            '@' => self.lex_at_rule(),
            '0'..='9' => self.lex_num(),
            '$' => self.lex_variable(),
            ':' => symbol!(self, Colon),
            ',' => symbol!(self, Comma),
            '.' => symbol!(self, Period),
            ';' => symbol!(self, SemiColon),
            '(' => symbol!(self, OpenParen),
            ')' => symbol!(self, CloseParen),
            '+' => symbol!(self, Plus),
            '~' => symbol!(self, Tilde),
            '\'' => symbol!(self, SingleQuote),
            '"' => symbol!(self, DoubleQuote),
            ' ' => whitespace!(self, Space),
            '\t' => whitespace!(self, Tab),
            '\n' => {
                self.buf.next();
                self.pos.newline();
                TokenKind::Whitespace(Whitespace::Newline)
            }
            '\r' => {
                self.buf.next();
                TokenKind::Whitespace(Whitespace::CarriageReturn)
            }
            '#' => self.lex_hash(),
            '{' => symbol!(self, OpenCurlyBrace),
            '*' => symbol!(self, Mul),
            '}' => symbol!(self, CloseCurlyBrace),
            '&' => symbol!(self, BitAnd),
            '/' => self.lex_forward_slash(),
            '%' => {
                self.buf.next();
                self.pos.next_char();
                TokenKind::Unit(Unit::Percent)
            }
            '[' => {
                self.buf.next();
                self.pos.next_char();
                self.lex_attr()
            }
            '!' => self.lex_exclamation(),
            '<' => symbol!(self, Lt),
            '>' => symbol!(self, Gt),
            '\0' => return None,
            _ => todo!("unknown char"),
        };
        self.pos.next_char();
        Some(Token {
            kind,
            pos: self.pos,
        })
    }
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\n' || c == '\r'
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &'a str) -> Lexer<'a> {
        Lexer {
            tokens: Vec::with_capacity(buf.len()),
            buf: buf.chars().peekable(),
            pos: Pos::new(),
        }
    }

    fn lex_exclamation(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        macro_rules! assert_char {
            ($self:ident, $($char:literal)*) => {
                $(
                    assert_eq!($char, $self.buf.next().expect("expected char").to_ascii_lowercase(), "expected keyword `important`");
                )*
            }
        };
        match self.buf.peek() {
            Some('i') | Some('I') => {
                self.buf.next();
                assert_char!(self, 'm' 'p' 'o' 'r' 't' 'a' 'n' 't');
            }
            Some('=') => {
                self.buf.next();
                return TokenKind::Op(Op::NotEqual);
            }
            _ => todo!("expected either `i` or `=` after `!`"),
        };
        TokenKind::Keyword(Keyword::Important)
    }

    fn devour_whitespace(&mut self) {
        while let Some(c) = self.buf.peek() {
            if !is_whitespace(*c) {
                break;
            }
            self.buf.next();
            self.pos.next_char();
        }
    }

    fn lex_at_rule(&mut self) -> TokenKind {
        self.buf.next();
        let mut string = String::with_capacity(99);
        while let Some(c) = self.buf.peek() {
            if !c.is_alphabetic() && c != &'-' && c != &'_' {
                break;
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            string.push(tok);
        }

        if let Ok(rule) = AtRule::try_from(string.as_ref()) {
            TokenKind::AtRule(rule)
        } else {
            panic!("expected ident after `@`")
        }
    }

    fn lex_forward_slash(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        match self.buf.peek().expect("expected something after '/'") {
            '/' => {
                self.buf.by_ref().take_while(|x| x != &'\n').for_each(drop);
            }
            '*' => {
                self.buf.next();
                self.pos.next_char();
                let mut comment = String::new();
                while let Some(tok) = self.buf.next() {
                    if tok == '\n' {
                        self.pos.newline()
                    } else {
                        self.pos.next_char();
                    }
                    if tok == '*' && self.buf.peek() == Some(&'/') {
                        self.buf.next();
                        break;
                    }
                    comment.push(tok);
                }
                return TokenKind::MultilineComment(comment);
            }
            _ => return TokenKind::Symbol(Symbol::Div),
        }
        TokenKind::Whitespace(Whitespace::Newline)
    }

    fn lex_num(&mut self) -> TokenKind {
        let mut string = String::with_capacity(99);
        while let Some(c) = self.buf.peek() {
            if !c.is_numeric() && c != &'.' {
                break;
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            string.push(tok);
        }

        TokenKind::Number(string)
    }

    fn lex_hash(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        if self.buf.peek() == Some(&'{') {
            self.buf.next();
            self.pos.next_char();
            return TokenKind::Interpolation;
        }
        TokenKind::Symbol(Symbol::Hash)
    }

    fn lex_attr(&mut self) -> TokenKind {
        let mut attr = String::with_capacity(99);
        while let Some(c) = self.buf.peek() {
            if !c.is_alphabetic() && c != &'-' && c != &'_' {
                break;
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            attr.push(tok);
        }

        self.devour_whitespace();

        let kind = match self
            .buf
            .next()
            .expect("todo! expected kind (should be error)")
        {
            ']' => {
                return TokenKind::Attribute(Attribute {
                    kind: AttributeKind::Any,
                    attr,
                    value: String::new(),
                    case_sensitive: true,
                })
            }
            'i' => {
                self.devour_whitespace();
                assert!(self.buf.next() == Some(']'));
                return TokenKind::Attribute(Attribute {
                    kind: AttributeKind::Any,
                    attr,
                    value: String::new(),
                    case_sensitive: false,
                });
            }
            '=' => AttributeKind::Equals,
            '~' => AttributeKind::InList,
            '|' => AttributeKind::BeginsWithHyphenOrExact,
            '^' => AttributeKind::StartsWith,
            '$' => AttributeKind::EndsWith,
            '*' => AttributeKind::Contains,
            _ => todo!("expected kind (should be error)"),
        };

        if kind != AttributeKind::Equals {
            assert!(self.buf.next() == Some('='));
        }

        self.devour_whitespace();

        let mut value = String::with_capacity(99);
        let mut case_sensitive = true;

        while let Some(c) = self.buf.peek() {
            if !c.is_alphabetic() && c != &'-' && c != &'_' && c != &'"' && c != &'\'' {
                break;
            }

            if c == &'i' {
                let tok = self
                    .buf
                    .next()
                    .expect("this is impossible because we have already peeked");
                self.pos.next_char();
                self.devour_whitespace();
                match self.buf.next() {
                    Some(']') => case_sensitive = false,
                    Some(val) => {
                        self.pos.next_char();
                        value.push(tok);
                        value.push(val);
                    }
                    None => todo!("expected something to come after "),
                }
                continue;
            }

            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            value.push(tok);
        }

        self.devour_whitespace();

        assert!(self.buf.next() == Some(']'));

        TokenKind::Attribute(Attribute {
            kind,
            attr,
            value,
            case_sensitive,
        })
    }

    fn lex_variable(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        let mut name = String::with_capacity(99);
        while let Some(c) = self.buf.peek() {
            if !c.is_alphabetic() && c != &'-' && c != &'_' {
                break;
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            if tok == '_' {
                name.push('-');
            } else {
                name.push(tok);
            }
        }
        TokenKind::Variable(name)
    }

    fn lex_ident(&mut self) -> TokenKind {
        let mut string = String::with_capacity(99);
        while let Some(c) = self.buf.peek() {
            // we know that the first char is alphabetic from peeking
            if !c.is_alphanumeric() && c != &'-' && c != &'_' {
                break;
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            if tok == '_' {
                string.push('-');
            } else {
                string.push(tok);
            }
        }

        if let Ok(kw) = Keyword::try_from(string.as_ref()) {
            return TokenKind::Keyword(kw);
        }

        if let Ok(kw) = Unit::try_from(string.as_ref()) {
            return TokenKind::Unit(kw);
        }

        TokenKind::Ident(string)
    }
}
