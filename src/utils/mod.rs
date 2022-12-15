pub(crate) use chars::*;
// pub(crate) use comment_whitespace::*;
// pub(crate) use number::*;
// pub(crate) use read_until::*;
pub(crate) use strings::*;

mod chars;
// mod comment_whitespace;
// mod number;
// mod read_until;
mod strings;

#[allow(clippy::case_sensitive_file_extension_comparisons)]
pub(crate) fn is_plain_css_import(url: &str) -> bool {
    if url.len() < 5 {
        return false;
    }

    let lower = url.to_ascii_lowercase();

    lower.ends_with(".css")
        || lower.starts_with("http://")
        || lower.starts_with("https://")
        || lower.starts_with("//")
}

pub(crate) fn opposite_bracket(b: char) -> char {
    debug_assert!(matches!(b, '(' | '{' | '[' | ')' | '}' | ']'));
    match b {
        '(' => ')',
        '{' => '}',
        '[' => ']',
        ')' => '(',
        '}' => '{',
        ']' => '[',
        _ => unreachable!(),
    }
}

pub(crate) fn is_special_function(s: &str) -> bool {
    s.starts_with("calc(")
        || s.starts_with("var(")
        || s.starts_with("env(")
        || s.starts_with("min(")
        || s.starts_with("max(")
        || s.starts_with("clamp(")
}
