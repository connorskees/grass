use std::convert::TryFrom;
use std::iter::Peekable;
use std::str::Chars;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::atrule::AtRuleKind;
use crate::common::{Keyword, Op, Pos, Symbol};
use crate::{Token, TokenKind, Whitespace};

// Rust does not allow us to escape '\f'
const FORM_FEED: char = '\x0C';

pub static IS_UTF8: AtomicBool = AtomicBool::new(false);

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
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
            'a'..='z' | 'A'..='Z' | '_' => self.lex_ident(),
            '-' => {
                self.buf.next();
                self.pos.next_char();
                match self.buf.peek().unwrap() {
                    '0'..='9' | '.' => match self.lex_num() {
                        TokenKind::Number(n) => {
                            let mut s = String::from("-");
                            s.push_str(&n);
                            TokenKind::Number(s)
                        }
                        _ => unsafe { std::hint::unreachable_unchecked() },
                    },
                    'a'..='z' | 'A'..='Z' | '_' | '-' => match self.lex_ident() {
                        TokenKind::Ident(i) => {
                            let mut s = String::from("-");
                            s.push_str(&i);
                            TokenKind::Ident(s)
                        }
                        TokenKind::Keyword(kw) => {
                            let mut s = String::from("-");
                            s.push_str(&kw.to_string());
                            TokenKind::Ident(s)
                        }
                        TokenKind::Symbol(Symbol::Minus) => TokenKind::Ident(String::from("--")),
                        _ => unsafe { std::hint::unreachable_unchecked() },
                    },
                    _ => TokenKind::Symbol(Symbol::Minus),
                }
            }
            '@' => self.lex_at_rule(),
            '0'..='9' => self.lex_num(),
            '.' => {
                self.buf.next();
                self.pos.next_char();
                match self.buf.peek().unwrap() {
                    '0'..='9' => match self.lex_num() {
                        TokenKind::Number(n) => {
                            let mut s = String::from("0.");
                            s.push_str(&n);
                            TokenKind::Number(s)
                        }
                        _ => unsafe { std::hint::unreachable_unchecked() },
                    },
                    _ => TokenKind::Symbol(Symbol::Period),
                }
            }
            '$' => self.lex_variable(),
            ':' => symbol!(self, Colon),
            ',' => symbol!(self, Comma),
            ';' => symbol!(self, SemiColon),
            '(' => symbol!(self, OpenParen),
            ')' => symbol!(self, CloseParen),
            '+' => symbol!(self, Plus),
            '=' => {
                self.buf.next();
                self.pos.next_char();
                match self.buf.peek() {
                    Some('=') => {
                        self.buf.next();
                        self.pos.next_char();
                        TokenKind::Op(Op::Equal)
                    }
                    _ => TokenKind::Symbol(Symbol::Equal),
                }
            }
            '?' => symbol!(self, QuestionMark),
            '\\' => symbol!(self, BackSlash),
            '~' => symbol!(self, Tilde),
            '\'' => symbol!(self, SingleQuote),
            '"' => symbol!(self, DoubleQuote),
            ' ' => whitespace!(self, Space),
            '\t' => whitespace!(self, Tab),
            '\n' | &FORM_FEED => {
                self.buf.next();
                self.pos.newline();
                TokenKind::Whitespace(Whitespace::Newline)
            }
            '\r' => {
                self.buf.next();
                TokenKind::Whitespace(Whitespace::Newline)
            }
            '#' => self.lex_hash(),
            '{' => symbol!(self, OpenCurlyBrace),
            '*' => symbol!(self, Mul),
            '}' => symbol!(self, CloseCurlyBrace),
            '&' => symbol!(self, BitAnd),
            '|' => symbol!(self, BitOr),
            '/' => self.lex_forward_slash(),
            '%' => symbol!(self, Percent),
            '[' => symbol!(self, OpenSquareBrace),
            ']' => symbol!(self, CloseSquareBrace),
            '!' => self.lex_exclamation(),
            '<' => symbol!(self, Lt),
            '>' => symbol!(self, Gt),
            '^' => symbol!(self, Xor),
            '`' => symbol!(self, BackTick),
            '\0' => return None,
            c if c.is_control() => {
                self.buf.next();
                TokenKind::Error("Expected expression.".into())
            },
            _ => self.lex_ident(),
        };
        self.pos.next_char();
        Some(Token {
            kind,
            pos: self.pos,
        })
    }
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
                TokenKind::Keyword(Keyword::Important)
            }
            Some('d') | Some('D') => {
                self.buf.next();
                assert_char!(self, 'e' 'f' 'a' 'u' 'l' 't');
                TokenKind::Keyword(Keyword::Default)
            }
            Some('g') | Some('G') => {
                self.buf.next();
                assert_char!(self, 'l' 'o' 'b' 'a' 'l');
                TokenKind::Keyword(Keyword::Global)
            }
            Some('=') => {
                self.buf.next();
                TokenKind::Op(Op::NotEqual)
            }
            _ => todo!("expected either `i` or `=` after `!`"),
        }
    }

    fn lex_at_rule(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
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

        TokenKind::AtRule(AtRuleKind::from(string.as_ref()))
    }

    fn lex_forward_slash(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        match self.buf.peek().expect("expected something after '/'") {
            '/' => {
                self.buf.by_ref().take_while(|x| x != &'\n').for_each(drop);
                self.pos.newline();
            }
            '*' => {
                self.buf.next();
                self.pos.next_char();
                let mut comment = String::new();
                while let Some(tok) = self.buf.next() {
                    match tok {
                        '\n' => self.pos.newline(),
                        FORM_FEED => {
                            self.pos.newline();
                            comment.push('\n');
                            continue;
                        }
                        '\r' => {
                            if self.buf.peek() == Some(&'\n') {
                                self.buf.next();
                            }
                            self.pos.newline();
                            comment.push('\n');
                            continue;
                        }
                        '*' if self.buf.peek() == Some(&'/') => {
                            self.buf.next();
                            break;
                        }
                        _ => self.pos.next_char(),
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
        let mut whole = String::new();
        while let Some(c) = self.buf.peek() {
            if !c.is_numeric() {
                break;
            }
            let tok = self.buf.next().unwrap();
            self.pos.next_char();
            whole.push(tok);
        }

        let mut dec = String::new();

        if self.buf.peek() == Some(&'.') {
            self.buf.next();
            dec.push('.');
            while let Some(c) = self.buf.peek() {
                if !c.is_numeric() {
                    break;
                }
                let tok = self.buf.next().unwrap();
                self.pos.next_char();
                dec.push(tok);
            }
        }

        if dec.len() == 1 {
            return TokenKind::Error("Expected digit.".into());
        }

        whole.push_str(&dec);

        TokenKind::Number(whole)
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

    fn lex_variable(&mut self) -> TokenKind {
        self.buf.next();
        self.pos.next_char();
        let mut name = String::with_capacity(99);
        if let Some(c) = self.buf.peek() {
            if c == &'=' {
                return TokenKind::Symbol(Symbol::Dollar);
            } else if !c.is_alphabetic() && c != &'-' && c != &'_' {
                return TokenKind::Error("Expected identifier.".into());
            } else {
                self.pos.next_char();
                name.push(*c);
            }
            self.buf.next();
        }
        while let Some(c) = self.buf.peek() {
            if !c.is_alphanumeric() && c != &'-' && c != &'_' {
                break;
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            name.push(tok);
        }
        if name.is_empty() {
            TokenKind::Symbol(Symbol::Dollar)
        } else {
            TokenKind::Variable(name)
        }
    }

    // TODO: handle weird characters that *are* ascii
    // e.g. how do we handle `color: ;`
    fn lex_ident(&mut self) -> TokenKind {
        let mut string = String::with_capacity(99);
        while let Some(c) = self.buf.peek() {
            // we know that the first char is alphabetic from peeking
            if !c.is_alphanumeric() && c != &'-' && c != &'_' && c.is_ascii() {
                break;
            }
            if !c.is_ascii() {
                IS_UTF8.store(true, Ordering::Relaxed);
            }
            let tok = self
                .buf
                .next()
                .expect("this is impossible because we have already peeked");
            self.pos.next_char();
            string.push(tok);
        }

        if let Ok(kw) = Keyword::try_from(string.as_ref()) {
            return TokenKind::Keyword(kw);
        }

        if string == "-" {
            return TokenKind::Symbol(Symbol::Minus);
        }

        TokenKind::Ident(string)
    }
}
