macro_rules! bound {
    ($args:ident, $name:literal, $arg:expr, $unit:expr, $low:literal, $high:literal) => {
        if !($arg <= Number::from($high) && $arg >= Number::from($low)) {
            return Err((
                format!(
                    "${}: Expected {}{} to be within {}{} and {}{}.",
                    $name,
                    $arg.inspect(),
                    $unit,
                    $low,
                    $unit,
                    $high,
                    $unit,
                ),
                $args.span(),
            )
                .into());
        } else {
            $arg
        }
    };
}
