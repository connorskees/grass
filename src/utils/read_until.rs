// use crate::{error::SassResult, lexer::Lexer, Token};

// use super::{devour_whitespace, read_until_newline};

// // Eat tokens until an open curly brace
// //
// // Does not consume the open curly brace
// pub(crate) fn read_until_open_curly_brace(toks: &mut Lexer) -> SassResult<Vec<Token>> {
//     let mut t = Vec::new();
//     let mut n = 0;
//     while let Some(tok) = toks.peek() {
//         match tok.kind {
//             '{' => n += 1,
//             '}' => n -= 1,
//             '/' => {
//                 let next = toks.next().unwrap();
//                 match toks.peek() {
//                     Some(Token { kind: '/', .. }) => read_until_newline(toks),
//                     _ => t.push(next),
//                 };
//                 continue;
//             }
//             '\\' => {
//                 t.push(toks.next().unwrap());
//                 t.push(match toks.next() {
//                     Some(tok) => tok,
//                     None => continue,
//                 });
//             }
//             q @ '"' | q @ '\'' => {
//                 t.push(toks.next().unwrap());
//                 t.extend(read_until_closing_quote(toks, q)?);
//                 continue;
//             }
//             _ => {}
//         }
//         if n == 1 {
//             break;
//         }

//         t.push(toks.next().unwrap());
//     }
//     Ok(t)
// }

// pub(crate) fn read_until_closing_curly_brace(toks: &mut Lexer) -> SassResult<Vec<Token>> {
//     let mut buf = Vec::new();
//     let mut nesting = 0;
//     while let Some(tok) = toks.peek() {
//         match tok.kind {
//             q @ '"' | q @ '\'' => {
//                 buf.push(toks.next().unwrap());
//                 buf.extend(read_until_closing_quote(toks, q)?);
//             }
//             '{' => {
//                 nesting += 1;
//                 buf.push(toks.next().unwrap());
//             }
//             '}' => {
//                 if nesting == 0 {
//                     break;
//                 }

//                 nesting -= 1;
//                 buf.push(toks.next().unwrap());
//             }
//             '/' => {
//                 let next = toks.next().unwrap();
//                 match toks.peek() {
//                     Some(Token { kind: '/', .. }) => {
//                         read_until_newline(toks);
//                         devour_whitespace(toks);
//                     }
//                     Some(..) | None => buf.push(next),
//                 };
//                 continue;
//             }
//             '(' => {
//                 buf.push(toks.next().unwrap());
//                 buf.extend(read_until_closing_paren(toks)?);
//             }
//             '\\' => {
//                 buf.push(toks.next().unwrap());
//                 buf.push(match toks.next() {
//                     Some(tok) => tok,
//                     None => continue,
//                 });
//             }
//             _ => buf.push(toks.next().unwrap()),
//         }
//     }
//     devour_whitespace(toks);
//     Ok(buf)
// }

// /// Read tokens into a vector until a matching closing quote is found
// ///
// /// The closing quote is included in the output
// pub(crate) fn read_until_closing_quote(toks: &mut Lexer, q: char) -> SassResult<Vec<Token>> {
//     let mut t = Vec::new();
//     while let Some(tok) = toks.next() {
//         match tok.kind {
//             '"' if q == '"' => {
//                 t.push(tok);
//                 break;
//             }
//             '\'' if q == '\'' => {
//                 t.push(tok);
//                 break;
//             }
//             '\\' => {
//                 t.push(tok);
//                 t.push(match toks.next() {
//                     Some(tok) => tok,
//                     None => return Err((format!("Expected {}.", q), tok.pos).into()),
//                 });
//             }
//             '#' => {
//                 t.push(tok);
//                 match toks.peek() {
//                     Some(tok @ Token { kind: '{', .. }) => {
//                         t.push(tok);
//                         toks.next();
//                         t.append(&mut read_until_closing_curly_brace(toks)?);
//                     }
//                     Some(..) => continue,
//                     None => return Err((format!("Expected {}.", q), tok.pos).into()),
//                 }
//             }
//             _ => t.push(tok),
//         }
//     }
//     if let Some(tok) = t.pop() {
//         if tok.kind != q {
//             return Err((format!("Expected {}.", q), tok.pos).into());
//         }
//         t.push(tok);
//     }
//     Ok(t)
// }

// pub(crate) fn read_until_semicolon_or_closing_curly_brace(
//     toks: &mut Lexer,
// ) -> SassResult<Vec<Token>> {
//     let mut t = Vec::new();
//     let mut nesting = 0;
//     while let Some(tok) = toks.peek() {
//         match tok.kind {
//             ';' => {
//                 break;
//             }
//             '\\' => {
//                 t.push(toks.next().unwrap());
//                 t.push(match toks.next() {
//                     Some(tok) => tok,
//                     None => continue,
//                 });
//             }
//             '"' | '\'' => {
//                 let quote = toks.next().unwrap();
//                 t.push(quote);
//                 t.extend(read_until_closing_quote(toks, quote.kind)?);
//             }
//             '{' => {
//                 nesting += 1;
//                 t.push(toks.next().unwrap());
//             }
//             '}' => {
//                 if nesting == 0 {
//                     break;
//                 }

//                 nesting -= 1;
//                 t.push(toks.next().unwrap());
//             }
//             '/' => {
//                 let next = toks.next().unwrap();
//                 match toks.peek() {
//                     Some(Token { kind: '/', .. }) => {
//                         read_until_newline(toks);
//                         devour_whitespace(toks);
//                     }
//                     _ => t.push(next),
//                 };
//                 continue;
//             }
//             _ => t.push(toks.next().unwrap()),
//         }
//     }
//     devour_whitespace(toks);
//     Ok(t)
// }

// pub(crate) fn read_until_closing_paren(toks: &mut Lexer) -> SassResult<Vec<Token>> {
//     let mut t = Vec::new();
//     let mut scope = 0;
//     while let Some(tok) = toks.next() {
//         match tok.kind {
//             ')' => {
//                 if scope < 1 {
//                     t.push(tok);
//                     return Ok(t);
//                 }

//                 scope -= 1;
//             }
//             '(' => scope += 1,
//             '"' | '\'' => {
//                 t.push(tok);
//                 t.extend(read_until_closing_quote(toks, tok.kind)?);
//                 continue;
//             }
//             '\\' => {
//                 t.push(match toks.next() {
//                     Some(tok) => tok,
//                     None => continue,
//                 });
//             }
//             _ => {}
//         }
//         t.push(tok);
//     }
//     Ok(t)
// }
