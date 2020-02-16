macro_rules! arg {
    ($args:ident, $idx:literal, $name:literal) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v,
            None => match $args.get($name) {
                Some(v) => v,
                None => return Err(concat!("Missing argument $", $name, ".").into()),
            },
        };
    };
    ($args:ident, $idx:literal, $name:literal=$default:expr) => {
        match $args.get(stringify!($idx)) {
            Some(v) => v.clone(),
            None => match $args.get($name) {
                Some(v) => v.clone(),
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

macro_rules! max_args {
    ($args:ident, $count:literal) => {
        if $args.len() > $count {
            return Err(format!("Only {} argument allowed, but {} were passed.", $count, $args.len()).into());
        }
    };
}