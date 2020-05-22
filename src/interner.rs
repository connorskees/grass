use lasso::{Rodeo, Spur};

use std::cell::RefCell;
use std::fmt::{self, Display};

thread_local!(static STRINGS: RefCell<Rodeo<Spur>> = RefCell::new(Rodeo::default()));

use keywords::EMPTY_STRING;

pub(crate) mod keywords {
    use super::InternedString;
    use once_cell::sync::Lazy;
    macro_rules! keyword {
        ($ident:ident, $val:literal) => {
            thread_local!(pub(crate) static $ident: Lazy<InternedString> =
                Lazy::new(|| InternedString::get_or_intern($val)));
        };
    }

    keyword!(EMPTY_STRING, "");
    keyword!(TRUE, "true");
    keyword!(FALSE, "false");
    keyword!(AND, "and");
    keyword!(OR, "or");
    keyword!(NOT, "not");
    keyword!(NULL, "null");
    keyword!(CALC, "calc");
    keyword!(URL, "url");
    keyword!(PROGID, "progid");
    keyword!(ELEMENT, "element");
    keyword!(EXPRESSION, "expression");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct InternedString(Spur);

impl InternedString {
    pub fn get_or_intern<T: AsRef<str>>(s: T) -> Self {
        Self(STRINGS.with(|interner| interner.borrow_mut().get_or_intern(s)))
    }

    pub fn resolve(self) -> String {
        STRINGS.with(|interner| interner.borrow().resolve(&self.0).to_string())
    }

    pub fn is_empty(self) -> bool {
        EMPTY_STRING.with(|f| self == **f)
    }

    pub fn resolve_ref<'a>(self) -> &'a str {
        unsafe {
            STRINGS.with(|interner| &(*(interner.as_ptr()).as_ref().unwrap().resolve(&self.0)))
        }
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        STRINGS.with(|interner| write!(f, "{}", interner.borrow().resolve(&self.0)))
    }
}
