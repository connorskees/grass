use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::{PeekMore, PeekMoreIterator};

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
    s: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
    q: char,
) -> Vec<Token> {
    let mut t = Vec::new();
    while let Some(tok) = toks.next() {
        match tok.kind {
            '"' if q == '"' => {
                t.push(tok);
                break;
            }
            '\'' if q == '\'' => {
                t.push(tok);
                break;
            }
            '\\' => {
                t.push(tok);
                t.push(toks.next().unwrap());
            }
            '#' => {
                t.push(tok);
                let next = toks.peek().unwrap();
                if next.kind == '{' {
                    t.push(toks.next().unwrap());
                    t.append(&mut read_until_closing_curly_brace(toks));
                }
            }
            _ => t.push(tok),
        }
    }
    t
}

pub(crate) fn read_until_semicolon_or_closing_curly_brace<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<VariableDecl> {
    devour_whitespace(toks);
    let mut default = false;
    let mut global = false;
    let mut raw = read_until_semicolon_or_closing_curly_brace(toks)
        .into_iter()
        .peekmore();
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

fn ident_body<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    unit: bool,
    mut span: Span,
) -> SassResult<Spanned<String>> {
    let mut text = String::new();
    while let Some(tok) = toks.peek() {
        span = span.merge(tok.pos());
        if unit && tok.kind == '-' {
            // Disallow `-` followed by a dot or a digit digit in units.
            let second = match toks.peek_forward(1) {
                Some(v) => v.clone(),
                None => break,
            };

            toks.peek_backward(1).unwrap();

            if second.kind == '.' || second.kind.is_ascii_digit() {
                break;
            }
            toks.next();
            text.push('-');
            text.push(toks.next().unwrap().kind);
        } else if is_name(tok.kind) {
            text.push(toks.next().unwrap().kind);
        } else if tok.kind == '\\' {
            toks.next();
            text.push_str(&escape(toks, false)?);
        } else {
            break;
        }
    }
    Ok(Spanned { node: text, span })
}

fn interpolated_ident_body<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    mut span: Span,
) -> SassResult<Spanned<String>> {
    let mut buf = String::new();
    while let Some(tok) = toks.peek() {
        if tok.kind == '_'
            || tok.kind.is_alphanumeric()
            || tok.kind == '-'
            || tok.kind as u32 >= 0x0080
        {
            span = span.merge(tok.pos());
            buf.push(toks.next().unwrap().kind);
        } else if tok.kind == '\\' {
            toks.next();
            buf.push_str(&escape(toks, false)?);
        } else if tok.kind == '#' {
            toks.next();
            let next = toks.next().unwrap();
            if next.kind == '{' {
                let interpolation = parse_interpolation(toks, scope, super_selector)?;
                buf.push_str(&interpolation.node.to_css_string(interpolation.span)?);
            }
        } else {
            break;
        }
    }
    Ok(Spanned { node: buf, span })
}

fn is_name(c: char) -> bool {
    is_name_start(c) || c.is_digit(10) || c == '-'
}

fn is_name_start(c: char) -> bool {
    // NOTE: in the dart-sass implementation, identifiers cannot start
    // with numbers. We explicitly differentiate from the reference
    // implementation here in order to support selectors beginning with numbers.
    // This can be considered a hack and in the future it would be nice to refactor
    // how this is handled.
    c == '_' || c.is_alphanumeric() || c as u32 >= 0x0080
}

fn escape<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    identifier_start: bool,
) -> SassResult<String> {
    let mut value = 0;
    let first = match toks.peek() {
        Some(t) => t,
        None => return Ok(String::new()),
    };
    if first.kind == '\n' {
        return Err(("Expected escape sequence.", first.pos()).into());
    } else if first.kind.is_ascii_hexdigit() {
        for _ in 0..6 {
            let next = match toks.peek() {
                Some(t) => t,
                None => break,
            };
            if !next.kind.is_ascii_hexdigit() {
                break;
            }
            value *= 16;
            value += as_hex(toks.next().unwrap().kind as u32)
        }
        if toks.peek().is_some() && toks.peek().unwrap().kind.is_whitespace() {
            toks.next();
        }
    } else {
        value = toks.next().unwrap().kind as u32;
    }

    // tabs are emitted literally
    // TODO: figure out where this check is done
    // in the source dart
    if value == 0x9 {
        return Ok("\\\t".to_string());
    }

    let c = std::char::from_u32(value).unwrap();
    if (identifier_start && is_name_start(c) && !c.is_digit(10))
        || (!identifier_start && is_name(c))
    {
        Ok(c.to_string())
    } else if value <= 0x1F || value == 0x7F || (identifier_start && c.is_digit(10)) {
        let mut buf = String::with_capacity(4);
        buf.push('\\');
        if value > 0xF {
            buf.push(hex_char_for(value >> 4));
        }
        buf.push(hex_char_for(value & 0xF));
        buf.push(' ');
        Ok(buf)
    } else {
        Ok(format!("\\{}", c))
    }
}

pub(crate) fn eat_ident<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Spanned<String>> {
    // TODO: take span as param because we use unwrap here
    let mut span = toks.peek().unwrap().pos();
    let mut text = String::new();
    if toks.peek().unwrap().kind == '-' {
        toks.next();
        text.push('-');
        if toks.peek().unwrap().kind == '-' {
            toks.next();
            text.push('-');
            text.push_str(&interpolated_ident_body(toks, scope, super_selector, span)?.node);
            return Ok(Spanned { node: text, span });
        }
    }

    let Token { kind: first, pos } = match toks.peek() {
        Some(v) => *v,
        None => return Err(("Expected identifier.", span).into()),
    };

    if is_name_start(first) {
        text.push(toks.next().unwrap().kind);
    } else if first == '\\' {
        toks.next();
        text.push_str(&escape(toks, true)?);
    // TODO: peekmore
    // (first == '#' && scanner.peekChar(1) == $lbrace)
    } else if first == '#' {
        toks.next();
        if toks.peek().is_none() {
            return Err(("Expected identifier.", pos).into());
        }
        let Token { kind, pos } = toks.peek().unwrap();
        if kind == &'{' {
            toks.next();
            text.push_str(
                &parse_interpolation(toks, scope, super_selector)?
                    .node
                    .to_css_string(span)?,
            );
        } else {
            return Err(("Expected identifier.", *pos).into());
        }
    } else {
        return Err(("Expected identifier.", pos).into());
    }

    let body = interpolated_ident_body(toks, scope, super_selector, pos)?;
    span = span.merge(body.span);
    text.push_str(&body.node);
    Ok(Spanned { node: text, span })
}

pub(crate) fn eat_ident_no_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    unit: bool,
) -> SassResult<Spanned<String>> {
    let mut span = toks.peek().unwrap().pos();
    let mut text = String::new();
    if toks.peek().unwrap().kind == '-' {
        toks.next();
        text.push('-');
        if toks.peek().unwrap().kind == '-' {
            toks.next();
            text.push('-');
            text.push_str(&ident_body(toks, unit, span)?.node);
            return Ok(Spanned { node: text, span });
        }
    }

    let first = match toks.peek() {
        Some(v) => v,
        None => return Err(("Expected identifier.", span).into()),
    };

    if is_name_start(first.kind) {
        text.push(toks.next().unwrap().kind);
    } else if first.kind == '\\' {
        toks.next();
        text.push_str(&escape(toks, true)?);
    } else {
        return Err(("Expected identifier.", first.pos()).into());
    }

    let body = ident_body(toks, unit, span)?;
    span = span.merge(body.span);
    text.push_str(&body.node);
    Ok(Spanned { node: text, span })
}

pub(crate) fn eat_number<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
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
pub(crate) fn read_until_newline<I: Iterator<Item = Token>>(toks: &mut PeekMoreIterator<I>) {
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
pub(crate) fn eat_comment<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
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
        } else if tok.kind == '#' && toks.peek().unwrap().kind == '{' {
            toks.next();
            comment
                .push_str(&parse_interpolation(toks, scope, super_selector)?.to_css_string(span)?);
            continue;
        }
        comment.push(tok.kind);
    }
    devour_whitespace(toks);
    Ok(Spanned {
        node: comment,
        span,
    })
}

fn as_hex(c: u32) -> u32 {
    if c <= '9' as u32 {
        c - '0' as u32
    } else if c <= 'F' as u32 {
        10 + c - 'A' as u32
    } else {
        10 + c - 'a' as u32
    }
}

pub(crate) fn parse_quoted_string<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    q: char,
    super_selector: &Selector,
) -> SassResult<Spanned<Value>> {
    let mut s = String::new();
    let mut span = if let Some(tok) = toks.peek() {
        tok.pos()
    } else {
        todo!()
    };
    while let Some(tok) = toks.next() {
        span = span.merge(tok.pos());
        match tok.kind {
            '"' if q == '"' => break,
            '\'' if q == '\'' => break,
            '#' => {
                if toks.peek().unwrap().kind == '{' {
                    toks.next();
                    let interpolation = parse_interpolation(toks, scope, super_selector)?;
                    s.push_str(&interpolation.node.to_css_string(interpolation.span)?);
                    continue;
                } else {
                    s.push('#');
                    continue;
                }
            }
            '\n' => return Err(("Expected \".", tok.pos()).into()),
            '\\' => {
                let first = match toks.peek() {
                    Some(c) => c,
                    None => {
                        s.push('\u{FFFD}');
                        continue;
                    }
                };

                if first.kind == '\n' {
                    return Err(("Expected escape sequence.", first.pos()).into());
                }

                if first.kind.is_ascii_hexdigit() {
                    let mut value = 0;
                    for _ in 0..6 {
                        let next = match toks.peek() {
                            Some(c) => c,
                            None => break,
                        };
                        if !next.kind.is_ascii_hexdigit() {
                            break;
                        }
                        value = (value << 4) + as_hex(toks.next().unwrap().kind as u32);
                    }

                    if toks.peek().is_some() && toks.peek().unwrap().kind.is_ascii_whitespace() {
                        toks.next();
                    }

                    if value == 0 || (value >= 0xD800 && value <= 0xDFFF) || value >= 0x10FFFF {
                        s.push('\u{FFFD}');
                    } else {
                        s.push(std::char::from_u32(value).unwrap());
                    }
                } else {
                    s.push(toks.next().unwrap().kind);
                }
            }
            _ => s.push(tok.kind),
        }
    }
    Ok(Spanned {
        node: Value::Ident(s, QuoteKind::Quoted),
        span,
    })
}

pub(crate) fn read_until_closing_paren<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
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
    toks: &mut PeekMoreIterator<I>,
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

pub(crate) fn hex_char_for(number: u32) -> char {
    assert!(number < 0x10);
    std::char::from_u32(if number < 0xA {
        0x30 + number
    } else {
        0x61 - 0xA + number
    })
    .unwrap()
}
