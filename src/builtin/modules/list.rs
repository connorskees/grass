use crate::{
    args::CallArgs,
    builtin::{
        list::{append, index, is_bracketed, join, length, list_separator, nth, set_nth, zip},
        modules::Module,
    },
    error::SassResult,
    parse::Parser,
    value::Value,
};

pub(crate) fn declare(f: &mut Module) {
    f.insert_builtin("append", append);
    f.insert_builtin("index", index);
    f.insert_builtin("is-bracketed", is_bracketed);
    f.insert_builtin("join", join);
    f.insert_builtin("length", length);
    f.insert_builtin("separator", list_separator);
    f.insert_builtin("nth", nth);
    f.insert_builtin("set-nth", set_nth);
    f.insert_builtin("zip", zip);
}
