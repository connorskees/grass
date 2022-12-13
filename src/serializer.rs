// use crate::{Options, parse::Stmt};

// pub(crate) struct Serializer<'a> {
//     indentation: usize,
//     options: &'a Options<'a>,
//     inspect: bool,
//     indent_width: usize,
//     quote: bool,
//     buffer: Vec<u8>,
// }

// impl<'a> Serializer<'a> {
//     pub fn new(options: &'a Options<'a>) -> Self {
//         Self {
//             inspect: false,
//             quote: true,
//             indentation: 0,
//             indent_width: 2,
//             options,
//             buffer: Vec::new(),
//         }
//     }

//     fn is_invisible(&self, stmt: Stmt) -> bool {
//         !self.inspect && if self.options.is_compressed() {
//             todo!()
//         } else {
//             todo!()
//         }
//     }

//     pub fn visit_stylesheet(&mut self, stylesheet: Vec<Stmt>) -> SassResult<()> {
//         let mut previous: Option<Stmt> = None;

//         for child in stylesheet {
//             if self.is_invisible(&child) {
//                 continue;
//             }

//             if previous.is_some()
//         }

//         Ok(())
//     //     CssNode? previous;
//     // for (var child in node.children) {
//     //   if (_isInvisible(child)) continue;
//     //   if (previous != null) {
//     //     if (_requiresSemicolon(previous)) _buffer.writeCharCode($semicolon);
//     //     if (_isTrailingComment(child, previous)) {
//     //       _writeOptionalSpace();
//     //     } else {
//     //       _writeLineFeed();
//     //       if (previous.isGroupEnd) _writeLineFeed();
//     //     }
//     //   }u
//     //   previous = child;

//     //   child.accept(this);
//     // }

//     // if (previous != null && _requiresSemicolon(previous) && !_isCompressed) {
//     //   _buffer.writeCharCode($semicolon);
//     // }
//     }
// }
