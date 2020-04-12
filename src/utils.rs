use std::iter::{Iterator, Peekable};

use codemap::Spanned;

use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

pub(crate) trait IsWhitespace {
    fn is_whitespace(&self) -> bool;
}

impl IsWhitespace for char {
    fn is_whitespace(&self) -> bool {
        self.is_ascii_whitespace()
    }
}

pub(crate) fn devour_whitespace<I: Iterator<Item = W>, W: IsWhitespace>(
    s: &mut Peekable<I>,
) -> bool {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if !w.is_whitespace() {
            break;
        }
        found_whitespace = true;
        s.next();
    }
    found_whitespace
}

pub(crate) trait IsComment {
    fn is_comment(&self) -> bool;
}

pub(crate) fn devour_whitespace_or_comment<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> SassResult<bool> {
    let mut found_whitespace = false;
    while let Some(tok) = toks.peek() {
        if tok.kind == '/' {
            let pos = toks.next().unwrap().pos();
            match toks.peek().unwrap().kind {
                '*' => {
                    eat_comment(toks, &Scope::new(), &Selector::new())?;
                }
                '/' => read_until_newline(toks),
                _ => return Err(("Expected expression.", pos).into()),
            };
            found_whitespace = true;
            continue;
        }
        if !tok.is_whitespace() {
            break;
        }
        found_whitespace = true;
        toks.next();
    }
    Ok(found_whitespace)
}

pub(crate) fn parse_interpolation<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Spanned<Value>> {
    let val = Value::from_vec(read_until_closing_curly_brace(toks), scope, super_selector)?;
    toks.next();
    Ok(Spanned {
        node: val.node.eval(val.span)?.node.unquote(),
        span: val.span,
    })
}

pub(crate) struct VariableDecl {
    pub val: Spanned<Value>,
    pub default: bool,
    pub global: bool,
}

impl VariableDecl {
    pub const fn new(val: Spanned<Value>, default: bool, global: bool) -> VariableDecl {
        VariableDecl {
            val,
            default,
            global,
        }
    }
}

// Eat tokens until an open curly brace
//
// Does not consume the open curly brace
pub(crate) fn read_until_open_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> Vec<Token> {
    let mut val = Vec::new();
    let mut n = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            '{' => n += 1,
            '}' => n -= 1,
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => val.push(next),
                };
                continue;
            }
            _ => {}
        }
        if n == 1 {
            break;
        }

        val.push(toks.next().unwrap());
    }
    val
}

pub(crate) fn read_until_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            q @ '"' | q @ '\'' => {
                t.push(toks.next().unwrap());
                t.extend(read_until_closing_quote(toks, q));
            }
            '{' => {
                nesting += 1;
                t.push(toks.next().unwrap());
            }
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(toks.next().unwrap());
                }
            }
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
}

pub(crate) fn read_until_closing_quote<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    q: char,
) -> Vec<Token> {
    let mut is_escaped = false;
    let mut t = Vec::new();
    for tok in toks {
        match tok.kind {
            '"' if !is_escaped && q == '"' => {
                t.push(tok);
                break;
            }
            '"' if is_escaped => {
                t.push(tok);
                is_escaped = false;
                continue;
            }
            '\'' if !is_escaped && q == '\'' => {
                t.push(tok);
                break;
            }
            '\'' if is_escaped => {
                t.push(tok);
                is_escaped = false;
                continue;
            }
            '\\' if !is_escaped => {
                t.push(tok);
                is_escaped = true
            }
            '\\' => {
                is_escaped = false;
                t.push(tok);
                continue;
            }
            _ if is_escaped => {
                is_escaped = false;
                t.push(tok);
            }
            _ => t.push(tok),
        }
    }
    t
}

pub(crate) fn read_until_semicolon_or_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            ';' => {
                break;
            }
            '\\' => {
                t.push(toks.next().unwrap());
                t.push(toks.next().unwrap());
            }
            '"' | '\'' => {
                let quote = toks.next().unwrap();
                t.push(quote.clone());
                t.extend(read_until_closing_quote(toks, quote.kind));
            }
            '{' => {
                nesting += 1;
                t.push(toks.next().unwrap());
            }
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(toks.next().unwrap());
                }
            }
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
}

pub(crate) fn read_until_semicolon_or_open_or_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> Vec<Token> {
    let mut t = Vec::new();
    let mut nesting = 0;
    while let Some(tok) = toks.peek() {
        match tok.kind {
            ';' => {
                break;
            }
            '\\' => {
                t.push(toks.next().unwrap());
                t.push(toks.next().unwrap());
            }
            '"' | '\'' => {
                let quote = toks.next().unwrap();
                t.push(quote.clone());
                t.extend(read_until_closing_quote(toks, quote.kind));
            }
            '#' => {
                t.push(toks.next().unwrap());
                match toks.peek().unwrap().kind {
                    '{' => nesting += 1,
                    ';' => break,
                    '}' => {
                        if nesting == 0 {
                            break;
                        } else {
                            nesting -= 1;
                        }
                    }
                    _ => {}
                }
                t.push(toks.next().unwrap());
            }
            '{' => break,
            '}' => {
                if nesting == 0 {
                    break;
                } else {
                    nesting -= 1;
                    t.push(toks.next().unwrap());
                }
            }
            '/' => {
                let next = toks.next().unwrap();
                match toks.peek().unwrap().kind {
                    '/' => read_until_newline(toks),
                    _ => t.push(next),
                };
                continue;
            }
            _ => t.push(toks.next().unwrap()),
        }
    }
    devour_whitespace(toks);
    t
}

pub(crate) fn eat_variable_value<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<VariableDecl> {
    devour_whitespace(toks);
    let mut default = false;
    let mut global = false;
    let mut raw = read_until_semicolon_or_closing_curly_brace(toks)
        .into_iter()
        .peekable();
    if toks.peek().is_some() && toks.peek().unwrap().kind == ';' {
        toks.next();
    }
    let mut val_toks = Vec::new();
    while let Some(tok) = raw.next() {
        match tok.kind {
            '!' => {
                let next = raw.next().unwrap();
                match next.kind {
                    'i' => todo!("!important"),
                    'g' => {
                        let s = eat_ident(&mut raw, scope, super_selector)?;
                        if s.node.to_ascii_lowercase().as_str() == "lobal" {
                            global = true;
                        } else {
                            return Err(("Invalid flag name.", s.span).into());
                        }
                    }
                    'd' => {
                        let s = eat_ident(&mut raw, scope, super_selector)?;
                        if s.to_ascii_lowercase().as_str() == "efault" {
                            default = true;
                        } else {
                            return Err(("Invalid flag name.", s.span).into());
                        }
                    }
                    _ => return Err(("Invalid flag name.", next.pos()).into()),
                }
            }
            _ => val_toks.push(tok),
        }
    }
    devour_whitespace(toks);

    let val = Value::from_vec(val_toks, scope, super_selector)?;
    Ok(VariableDecl::new(val, default, global))
}

pub(crate) fn eat_ident<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Spanned<String>> {
    let mut s = String::new();
    let mut span = toks.peek().unwrap().pos();
    while let Some(tok) = toks.peek() {
        span = span.merge(tok.pos());
        match tok.kind {
            '#' => {
                let tok = toks.next().unwrap();
                if toks.peek().ok_or(("Expected identifier.", tok.pos()))?.kind == '{' {
                    toks.next();
                    let interpolation = parse_interpolation(toks, scope, super_selector)?;
                    span = span.merge(interpolation.span);
                    s.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                } else {
                    return Err(("Expected identifier.", tok.pos()).into());
                }
            }
            _ if tok.kind.is_ascii_alphanumeric()
                || tok.kind == '-'
                || tok.kind == '_'
                || (!tok.kind.is_ascii() && !tok.kind.is_control()) =>
            {
                s.push(toks.next().unwrap().kind)
            }
            '\\' => {
                let span_start = toks.next().unwrap().pos();
                let mut n = String::new();
                while let Some(c) = toks.peek() {
                    if !c.kind.is_ascii_hexdigit() || n.len() > 6 {
                        break;
                    }
                    n.push(c.kind);
                    toks.next();
                }
                if n.is_empty() {
                    let c = toks.next().ok_or(("expected \"{\".", span_start))?.kind;
                    if (c == '-' && !s.is_empty()) || c.is_ascii_alphabetic() {
                        s.push(c);
                    } else {
                        s.push_str(&format!("\\{}", c));
                    }
                    continue;
                }
                devour_whitespace(toks);
                let c = std::char::from_u32(u32::from_str_radix(&n, 16).unwrap()).unwrap();
                if c.is_control() && c != '\t' {
                    s.push_str(&format!("\\{} ", n.to_ascii_lowercase()));
                } else if !c.is_ascii_alphanumeric() && s.is_empty() && c.is_ascii() {
                    s.push_str(&format!("\\{}", c));
                } else if c.is_numeric() && s.is_empty() {
                    s.push_str(&format!("\\{} ", n))
                } else {
                    s.push(c);
                };
            }
            _ => break,
        }
    }
    Ok(Spanned { node: s, span })
}

pub(crate) fn eat_ident_no_interpolation<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> SassResult<Spanned<String>> {
    let mut s = String::new();
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    while let Some(tok) = toks.peek() {
        span = span.merge(tok.pos());
        match tok.kind {
            '#' => {
                break;
            }
            _ if tok.kind.is_ascii_alphanumeric()
                || tok.kind == '-'
                || tok.kind == '_'
                || (!tok.kind.is_ascii() && !tok.kind.is_control()) =>
            {
                s.push(toks.next().unwrap().kind)
            }
            '\\' => {
                toks.next();
                let mut n = String::new();
                while let Some(c) = toks.peek() {
                    if !c.kind.is_ascii_hexdigit() || n.len() > 6 {
                        break;
                    }
                    n.push(c.kind);
                    toks.next();
                }
                if n.is_empty() {
                    let c = toks.next().unwrap().kind;
                    if (c == '-' && !s.is_empty()) || c.is_ascii_alphabetic() {
                        s.push(c);
                    } else {
                        s.push_str(&format!("\\{}", c));
                    }
                    continue;
                }
                devour_whitespace(toks);
                let c = std::char::from_u32(u32::from_str_radix(&n, 16).unwrap()).unwrap();
                if c.is_control() && c != '\t' {
                    s.push_str(&format!("\\{} ", n.to_ascii_lowercase()));
                } else if !c.is_ascii_alphanumeric() && s.is_empty() && c.is_ascii() {
                    s.push_str(&format!("\\{}", c));
                } else if c.is_numeric() && s.is_empty() {
                    s.push_str(&format!("\\{} ", n))
                } else {
                    s.push(c);
                };
            }
            _ => break,
        }
    }
    Ok(Spanned { node: s, span })
}

pub(crate) fn eat_number<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> SassResult<Spanned<String>> {
    let mut whole = String::new();
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    while let Some(c) = toks.peek() {
        if !c.kind.is_numeric() {
            break;
        }
        let tok = toks.next().unwrap();
        span = span.merge(tok.pos());
        whole.push(tok.kind);
    }

    if toks.peek().is_none() {
        return Ok(Spanned { node: whole, span });
    }

    let mut dec = String::new();

    let next_tok = toks.peek().unwrap().clone();

    if next_tok.kind == '.' {
        toks.next();
        dec.push('.');
        while let Some(c) = toks.peek() {
            if !c.kind.is_numeric() {
                break;
            }
            let tok = toks.next().unwrap();
            span = span.merge(tok.pos());
            dec.push(tok.kind);
        }
    }

    if dec.len() == 1 {
        return Err(("Expected digit.", next_tok.pos()).into());
    }

    whole.push_str(&dec);
    Ok(Spanned { node: whole, span })
}

/// Eat tokens until a newline
///
/// This exists largely to eat silent comments, "//"
/// We only have to check for \n as the lexing step normalizes all newline characters
///
/// The newline is consumed
pub(crate) fn read_until_newline<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) {
    for tok in toks {
        if tok.kind == '\n' {
            break;
        }
    }
}

/// Eat and return the contents of a comment.
///
/// This function assumes that the starting "/*" has already been consumed
/// The entirety of the comment, including the ending "*/" is consumed.
/// Note that the ending "*/" is not included in the output.
///
/// TODO: support interpolation within multiline comments
pub(crate) fn eat_comment<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    _scope: &Scope,
    _super_selector: &Selector,
) -> SassResult<Spanned<String>> {
    let mut comment = String::new();
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    while let Some(tok) = toks.next() {
        span = span.merge(tok.pos());
        if tok.kind == '*' && toks.peek().unwrap().kind == '/' {
            toks.next();
            break;
        }
        comment.push(tok.kind);
    }
    devour_whitespace(toks);
    Ok(Spanned {
        node: comment,
        span,
    })
}

pub(crate) fn parse_quoted_string<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    q: char,
    super_selector: &Selector,
) -> SassResult<Spanned<Value>> {
    let mut s = String::new();
    let mut is_escaped = false;
    let mut found_interpolation = false;
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    while let Some(tok) = toks.next() {
        span = span.merge(tok.pos());
        match tok.kind {
            '"' if !is_escaped && q == '"' => break,
            '"' if is_escaped => {
                s.push('"');
                is_escaped = false;
                continue;
            }
            '\'' if !is_escaped && q == '\'' => break,
            '\'' if is_escaped => {
                s.push('\'');
                is_escaped = false;
                continue;
            }
            '\\' if !is_escaped => is_escaped = true,
            '\\' => {
                is_escaped = false;
                s.push('\\');
                continue;
            }
            '#' if !is_escaped => {
                if toks.peek().unwrap().kind == '{' {
                    toks.next();
                    found_interpolation = true;
                    let interpolation = parse_interpolation(toks, scope, super_selector)?;
                    s.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                    continue;
                } else {
                    s.push('#');
                    continue;
                }
            }
            '\n' => return Err(("Expected \".", tok.pos()).into()),
            v if v.is_ascii_hexdigit() && is_escaped => {
                let mut n = v.to_string();
                while let Some(c) = toks.peek() {
                    if !c.kind.is_ascii_hexdigit() || n.len() > 6 {
                        break;
                    }
                    n.push(c.kind);
                    toks.next();
                }
                let c = std::char::from_u32(u32::from_str_radix(&n, 16).unwrap()).unwrap();
                if c.is_control() && c != '\t' && c != '\0' {
                    s.push_str(&format!("\\{}", n.to_ascii_lowercase()));
                } else if c == '\0' {
                    s.push('\u{FFFD}');
                } else {
                    s.push(c);
                }
                is_escaped = false;
                continue;
            }
            _ if is_escaped => {
                is_escaped = false;
            }
            _ => {}
        }
        if is_escaped && tok.kind != '\\' {
            is_escaped = false;
        }
        if tok.kind != '\\' {
            s.push_str(&tok.kind.to_string());
        }
    }
    let quotes = if found_interpolation {
        QuoteKind::Double
    } else {
        match q {
            '"' => QuoteKind::Double,
            '\'' => QuoteKind::Single,
            _ => unreachable!(),
        }
    };
    Ok(Spanned {
        node: Value::Ident(s, quotes),
        span,
    })
}

pub(crate) fn read_until_closing_paren<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> Vec<Token> {
    let mut v = Vec::new();
    let mut scope = 0;
    while let Some(tok) = toks.next() {
        match tok.kind {
            ')' => {
                if scope < 1 {
                    v.push(tok);
                    return v;
                } else {
                    scope -= 1;
                }
            }
            '(' => scope += 1,
            '"' | '\'' => {
                v.push(tok.clone());
                v.extend(read_until_closing_quote(toks, tok.kind));
                continue;
            }
            _ => {}
        }
        v.push(tok)
    }
    v
}

pub(crate) fn read_until_closing_square_brace<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> Vec<Token> {
    let mut v = Vec::new();
    let mut scope = 0;
    while let Some(tok) = toks.next() {
        match tok.kind {
            ']' => {
                if scope < 1 {
                    v.push(tok);
                    return v;
                } else {
                    scope -= 1;
                }
            }
            '[' => scope += 1,
            '"' | '\'' => {
                v.push(tok.clone());
                v.extend(read_until_closing_quote(toks, tok.kind));
                continue;
            }
            _ => {}
        }
        v.push(tok)
    }
    v
}

pub(crate) fn read_until_char<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    c: char,
) -> Vec<Token> {
    let mut v = Vec::new();
    while let Some(tok) = toks.next() {
        match tok.kind {
            '"' | '\'' => {
                v.push(tok.clone());
                v.extend(read_until_closing_quote(toks, tok.kind));
                continue;
            }
            t if t == c => break,
            _ => {}
        }
        v.push(tok)
    }
    v
}

pub(crate) fn is_ident_char(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || c == '\\' || (!c.is_ascii() && !c.is_control())
}
