macro_rules! arg {
    ($args:ident, $scope:ident, $super_selector:ident, $idx:literal, $name:literal) => {
        match $args.get_positional($idx, $scope, $super_selector) {
            Some(v) => v?.node.eval($args.span())?.node,
            None => match $args.get_named($name.to_owned(), $scope, $super_selector) {
                Some(v) => v?.node.eval($args.span())?.node,
                None => {
                    return Err((concat!("Missing argument $", $name, "."), $args.span()).into())
                }
            },
        };
    };
    ($args:ident, $scope:ident, $super_selector:ident, $idx:literal, $name:literal=$default:expr) => {
        match $args.get_positional($idx, $scope, $super_selector) {
            Some(v) => v?.node.eval($args.span())?.node,
            None => match $args.get_named($name.to_owned(), $scope, $super_selector) {
                Some(v) => v?.node.eval($args.span())?.node,
                None => $default,
            },
        };
    };
}

macro_rules! named_arg {
    ($args:ident, $scope:ident, $super_selector:ident, $name:literal) => {
        match $args.get_named($name.to_owned(), $scope, $super_selector) {
            Some(v) => v?.node.eval($args.span())?.node,
            None => return Err((concat!("Missing argument $", $name, "."), $args.span()).into()),
        };
    };
    ($args:ident, $scope:ident, $super_selector:ident, $name:literal=$default:expr) => {
        match $args.get_named($name.to_owned(), $scope, $super_selector) {
            Some(v) => v?.node.eval($args.span())?.node,
            None => $default,
        };
    };
}

macro_rules! bound {
    ($args:ident, $name:literal, $arg:ident, $unit:ident, $low:literal, $high:literal) => {
        if $arg > Number::from($high) || $arg < Number::from($low) {
            return Err((
                format!(
                    "${}: Expected {}{} to be within {}{} and {}{}.",
                    $name, $arg, $unit, $low, $unit, $high, $unit,
                ),
                $args.span(),
            )
                .into());
        } else {
            $arg
        }
    };
    ($args:ident, $name:literal, $arg:ident, $unit:path, $low:literal, $high:literal) => {
        if $arg > Number::from($high) || $arg < Number::from($low) {
            return Err((
                format!(
                    "${}: Expected {}{} to be within {}{} and {}{}.",
                    $name, $arg, $unit, $low, $unit, $high, $unit,
                ),
                $args.span(),
            )
                .into());
        } else {
            $arg
        }
    };
}
