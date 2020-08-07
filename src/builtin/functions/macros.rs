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
