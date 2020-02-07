macro_rules! arg {
    ($args:ident, $idx:literal, $name:literal) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v,
            None => match $args.get($name) {
                Some(v) => v,
                None => panic!("missing variable"),
            },
        };
    };
    ($args:ident, $idx:literal, $name:literal=$default:literal) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v,
            None => match $args.get($name) {
                Some(v) => v,
                None => $default,
            },
        };
    };
}

macro_rules! decl {
    ($f:ident $name:literal, $body:expr) => {
        $f.insert($name.to_owned(), Box::new($body));
    };
}
