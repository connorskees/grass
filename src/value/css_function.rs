pub(crate) fn is_special_function(s: &str) -> bool {
    s.starts_with("calc(")
        || s.starts_with("var(")
        || s.starts_with("env(")
        || s.starts_with("min(")
        || s.starts_with("max(")
        || s.starts_with("clamp(")
}
