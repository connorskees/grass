use std::iter::{Iterator, Peekable};

use crate::common::QuoteKind;
use crate::error::SassResult;
use crate::lexer::Lexer;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

pub(crate) trait IsWhitespace {
    fn is_whitespace(&self) -> bool;
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
    s: &mut Peekable<I>,
) -> SassResult<bool> {
    let mut found_whitespace = false;
    while let Some(w) = s.peek() {
        if w.kind == '/' {
            s.next();
            match s.peek().unwrap().kind {
                '*' => {
                    eat_comment(s, &Scope::new(), &Selector::new())?;
                }
                '/' => read_until_newline(s),
                _ => return Err("Expected expression.".into()),
            };
            found_whitespace = true;
            continue;
        }
        if !w.is_whitespace() {
            break;
        }
        found_whitespace = true;
        s.next();
    }
    Ok(found_whitespace)
}

pub(crate) fn parse_interpolation<I: Iterator<Item = Token>>(
    tokens: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<Value> {
    let mut val = String::new();
    while let Some(tok) = tokens.next() {
        match tok.kind {
            '}' => break,
            '{' => todo!("invalid character in interpolation"),
            q @ '"' | q @ '\'' => {
                val.push_str(&parse_quoted_string(tokens, scope, q, super_selector)?.to_string())
            }
            '$' => val.push_str(
                &scope
                    .get_var(&eat_ident(tokens, scope, super_selector)?)?
                    .clone()
                    .unquote()
                    .to_string(),
            ),
            '#' => {
                if tokens.next().unwrap().kind == '{' {
                    val.push_str(
                        &Lexer::new(
                            &parse_interpolation(tokens, scope, super_selector)?.to_string(),
                        )
                        .map(|x| x.kind.to_string())
                        .collect::<String>(),
                    )
                } else {
                    return Err("Expected identifier.".into());
                }
            }
            _ => val.push_str(&tok.kind.to_string()),
        }
    }
    if val.trim().is_empty() {
        return Ok(Value::Ident(String::new(), QuoteKind::None));
    }
    Ok(
        Value::from_tokens(&mut Lexer::new(&val).peekable(), scope, super_selector)?
            .eval()?
            .unquote(),
    )
}

pub(crate) struct VariableDecl {
    pub val: Value,
    pub default: bool,
    pub global: bool,
}

impl VariableDecl {
    pub const fn new(val: Value, default: bool, global: bool) -> VariableDecl {
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
    if toks.peek().unwrap().kind == ';' {
        toks.next();
    }
    let mut x = Vec::new();
    while let Some(tok) = raw.next() {
        match tok.kind {
            '!' => {
                let next = raw.next().unwrap();
                match next.kind {
                    'i' => todo!("!important"),
                    'g' => {
                        if eat_ident(&mut raw, scope, super_selector)?
                            .to_ascii_lowercase()
                            .as_str()
                            == "lobal"
                        {
                            global = true;
                        } else {
                            return Err("Invalid flag name.".into());
                        }
                    }
                    'd' => {
                        if eat_ident(&mut raw, scope, super_selector)?
                            .to_ascii_lowercase()
                            .as_str()
                            == "efault"
                        {
                            default = true;
                        } else {
                            return Err("Invalid flag name.".into());
                        }
                    }
                    _ => return Err("Invalid flag name.".into()),
                }
            }
            _ => x.push(tok),
        }
    }
    devour_whitespace(toks);

    let val = Value::from_tokens(&mut x.into_iter().peekable(), scope, super_selector)?;
    Ok(VariableDecl::new(val, default, global))
}

pub(crate) fn eat_ident<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    super_selector: &Selector,
) -> SassResult<String> {
    let mut s = String::new();
    while let Some(tok) = toks.peek() {
        match tok.kind {
            '#' => {
                toks.next();
                if toks.peek().unwrap().kind == '{' {
                    toks.next();
                    s.push_str(&parse_interpolation(toks, scope, super_selector)?.to_string());
                } else {
                    return Err("Expected identifier.".into());
                }
            }
            'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' => s.push(toks.next().unwrap().kind),
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
    Ok(s)
}

pub(crate) fn eat_ident_no_interpolation<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
) -> SassResult<String> {
    let mut s = String::new();
    while let Some(tok) = toks.peek() {
        match tok.kind {
            '#' => {
                break;
            }
            'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' => s.push(toks.next().unwrap().kind),
            '\\' => {
                s.push('\\');
                toks.next();
                if let Some(tok) = toks.next() {
                    match tok.kind {
                        '+' => s.push('+'),
                        '\\' => s.push('\\'),
                        _ => todo!("value after \\"),
                    }
                } else {
                    todo!()
                }
            }
            _ => break,
        }
    }
    Ok(s)
}

pub(crate) fn eat_number<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) -> SassResult<String> {
    let mut whole = String::new();
    while let Some(c) = toks.peek() {
        if !c.kind.is_numeric() {
            break;
        }
        let tok = toks.next().unwrap();
        whole.push(tok.kind);
    }

    if toks.peek().is_none() {
        return Ok(whole);
    }

    let mut dec = String::new();

    if toks.peek().unwrap().kind == '.' {
        toks.next();
        dec.push('.');
        while let Some(c) = toks.peek() {
            if !c.kind.is_numeric() {
                break;
            }
            let tok = toks.next().unwrap();
            dec.push(tok.kind);
        }
    }

    if dec.len() == 1 {
        return Err("Expected digit.".into());
    }

    whole.push_str(&dec);
    Ok(whole)
}

/// Eat tokens until a newline
///
/// This exists largely to eat silent comments, "//"
/// We only have to check for \n as the lexing step normalizes all newline characters
///
/// The newline is consumed
pub(crate) fn read_until_newline<I: Iterator<Item = Token>>(toks: &mut Peekable<I>) {
    while let Some(tok) = toks.next() {
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
    toks: &mut Peekable<I>,
    _scope: &Scope,
    _super_selector: &Selector,
) -> SassResult<String> {
    let mut comment = String::new();
    while let Some(tok) = toks.next() {
        if tok.kind == '*' {
            if toks.peek().unwrap().kind == '/' {
                toks.next();
                break;
            }
        }
        comment.push(tok.kind);
    }
    devour_whitespace(toks);
    Ok(comment)
}

pub(crate) fn parse_quoted_string<I: Iterator<Item = Token>>(
    toks: &mut Peekable<I>,
    scope: &Scope,
    q: char,
    super_selector: &Selector,
) -> SassResult<Value> {
    let mut s = String::new();
    let mut is_escaped = false;
    let mut found_interpolation = false;
    while let Some(tok) = toks.next() {
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
                    s.push_str(&parse_interpolation(toks, scope, super_selector)?.to_string());
                    continue;
                } else {
                    s.push('#');
                }
            }
            '\n' => return Err("Expected \".".into()),
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
                    s.push_str(&format!("\\{} ", n.to_ascii_lowercase()));
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
    Ok(Value::Ident(s, quotes))
}
