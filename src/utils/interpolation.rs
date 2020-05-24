use std::iter::Iterator;

use codemap::{Span, Spanned};

use peekmore::PeekMoreIterator;

use crate::error::SassResult;
use crate::selector::Selector;
use crate::value::Value;
use crate::{Scope, Token};

use super::read_until_closing_curly_brace;

pub(crate) fn parse_interpolation<I: Iterator<Item = Token>>(
    toks: &mut PeekMoreIterator<I>,
    scope: &Scope,
    super_selector: &Selector,
    span_before: Span,
) -> SassResult<Spanned<Value>> {
    let val = Value::from_vec(
        read_until_closing_curly_brace(toks)?,
        scope,
        super_selector,
        span_before,
    )?;
    toks.next();
    Ok(Spanned {
        node: val.node.eval(val.span)?.node.unquote(),
        span: val.span,
    })
}
