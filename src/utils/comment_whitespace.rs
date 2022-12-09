// use crate::lexer::Lexer;

// pub(crate) trait IsWhitespace {
//     fn is_whitespace(&self) -> bool;
// }

// impl IsWhitespace for char {
//     fn is_whitespace(&self) -> bool {
//         self.is_ascii_whitespace()
//     }
// }

// pub(crate) fn devour_whitespace(s: &mut Lexer) -> bool {
//     let mut found_whitespace = false;
//     while let Some(w) = s.peek() {
//         if !w.is_whitespace() {
//             break;
//         }
//         found_whitespace = true;
//         s.next();
//     }
//     found_whitespace
// }

// /// Eat tokens until a newline
// ///
// /// This exists largely to eat silent comments, "//"
// /// We only have to check for \n as the lexing step normalizes all newline characters
// ///
// /// The newline is consumed
// pub(crate) fn read_until_newline(toks: &mut Lexer) {
//     for tok in toks {
//         if tok.kind == '\n' {
//             return;
//         }
//     }
// }
