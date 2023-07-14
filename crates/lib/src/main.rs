use std::{
    fs::OpenOptions,
    io::{stdin, stdout, Read, Write},
    path::Path,
};

use clap::{value_parser, Arg, ArgEnum, Command, PossibleValue};

use grass::{from_path, from_string, Options, OutputStyle};

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum Style {
    Expanded,
    Compressed,
}

impl ArgEnum for Style {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Expanded, Self::Compressed]
    }

    fn to_possible_value<'a>(&self) -> Option<PossibleValue<'a>> {
        Some(match self {
            Self::Expanded => PossibleValue::new("expanded"),
            Self::Compressed => PossibleValue::new("compressed"),
        })
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum SourceMapUrls {
    Relative,
    Absolute,
}

impl ArgEnum for SourceMapUrls {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Relative, Self::Absolute]
    }

    fn to_possible_value<'a>(&self) -> Option<PossibleValue<'a>> {
        Some(match self {
            Self::Relative => PossibleValue::new("relative"),
            Self::Absolute => PossibleValue::new("absolute"),
        })
    }
}

fn main() -> std::io::Result<()> {
    let matches = Command::new("grass")
        .version(env!("CARGO_PKG_VERSION"))
        .about("A near-feature-complete Sass compiler written purely in Rust")
        .mut_arg("version", |arg| arg.short('v'))
        .arg(
            Arg::new("STDIN")
                .long("stdin")
                .help("Read the stylesheet from stdin"),
        )
        .arg(
            Arg::new("INDENTED")
                .long("indented")
                .hide(true)
                .help("Use the indented syntax for input from stdin"),
        )
        .arg(
            Arg::new("LOAD_PATH")
                .short('I')
                .long("load-path")
                .help("A path to use when resolving imports. May be passed multiple times.")
                .multiple_occurrences(true)
                .takes_value(true)
                .value_parser(value_parser!(String))
                .number_of_values(1)
        )
        .arg(
            Arg::new("STYLE")
                // this is required for compatibility with ruby sass
                .short_alias('t')
                .short('s')
                .long("style")
                .help("Minified or expanded output")
                .default_value("expanded")
                .ignore_case(true)
                .takes_value(true)
                .value_parser(value_parser!(Style)),
        )
        .arg(
            Arg::new("NO_CHARSET")
                .long("no-charset")
                .help("Don't emit a @charset or BOM for CSS with non-ASCII characters."),
        )
        .arg(
            Arg::new("UPDATE")
                .long("update")
                .hide(true)
                .help("Only compile out-of-date stylesheets."),
        )
        .arg(
            Arg::new("NO_ERROR_CSS")
                .long("no-error-css")
                .hide(true)
                .help("When an error occurs, don't emit a stylesheet describing it."),
        )
        // Source maps
        .arg(
            Arg::new("NO_SOURCE_MAP")
                .long("no-source-map")
                .hide(true)
                .help("Whether to generate source maps."),
        )
        .arg(
            Arg::new("SOURCE_MAP_URLS")
                .long("source-map-urls")
                .hide(true)
                .help("How to link from source maps to source files.")
                .default_value("relative")
                .ignore_case(true)
                .takes_value(true)
                .value_parser(value_parser!(SourceMapUrls)),
        )
        .arg(
            Arg::new("EMBED_SOURCES")
                .long("embed-sources")
                .hide(true)
                .help("Embed source file contents in source maps."),
        )
        .arg(
            Arg::new("EMBED_SOURCE_MAP")
                .long("embed-source-map")
                .hide(true)
                .help("Embed source map contents in CSS."),
        )
        // Other
        .arg(
            Arg::new("WATCH")
                .long("watch")
                .hide(true)
                .help("Watch stylesheets and recompile when they change."),
        )
        .arg(
            Arg::new("POLL")
                .long("poll")
                .hide(true)
                .help("Manually check for changes rather than using a native watcher. Only valid with --watch.")
                .requires("WATCH"),
        )
        .arg(
            Arg::new("NO_STOP_ON_ERROR")
                .long("no-stop-on-error")
                .hide(true)
                .help("Continue to compile more files after error is encountered.")
        )
        .arg(
            Arg::new("INTERACTIVE")
                .short('i')
                .long("interactive")
                .hide(true)
                .help("Run an interactive SassScript shell.")
        )
        .arg(
            Arg::new("NO_COLOR")
                .short('c')
                .long("no-color")
                .hide(true)
                .help("Whether to use terminal colors for messages.")
        )
        .arg(
            Arg::new("VERBOSE")
                .long("verbose")
                .hide(true)
                .help("Print all deprecation warnings even when they're repetitive.")
        )
        .arg(
            Arg::new("NO_UNICODE")
                .long("no-unicode")
                .help("Whether to use Unicode characters for messages.")
        )
        .arg(
            Arg::new("QUIET")
                .short('q')
                .long("quiet")
                .help("Don't print warnings."),
        )
        .arg(
            Arg::new("INPUT")
                .value_parser(value_parser!(String))
                .required_unless_present("STDIN")
                .help("Sass files"),
        )
        .arg(
            Arg::new("OUTPUT")
                .help("Output CSS file")
        )

        // Hidden, legacy arguments
        .arg(
            Arg::new("PRECISION")
                .long("precision")
                .hide(true)
                .takes_value(true)
        )
        .get_matches();

    let load_paths = matches
        .get_many::<String>("LOAD_PATH")
        .map_or_else(Vec::new, |vals| vals.map(Path::new).collect());

    let style = match &matches.get_one::<Style>("STYLE").unwrap() {
        Style::Expanded => OutputStyle::Expanded,
        Style::Compressed => OutputStyle::Compressed,
    };

    let options = &Options::default()
        .load_paths(&load_paths)
        .style(style)
        .quiet(matches.is_present("QUIET"))
        .unicode_error_messages(!matches.is_present("NO_UNICODE"))
        .allows_charset(!matches.is_present("NO_CHARSET"));

    let (mut stdout_write, mut file_write);
    let buf_out: &mut dyn Write = if let Some(path) = matches.get_one::<&str>("OUTPUT") {
        file_write = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)?;
        &mut file_write
    } else {
        stdout_write = stdout();
        &mut stdout_write
    };

    buf_out.write_all(
        if let Some(name) = matches.get_one::<String>("INPUT") {
            from_path(name, options)
        } else if matches.is_present("STDIN") {
            from_string(
                {
                    let mut buffer = String::new();
                    stdin().read_to_string(&mut buffer)?;
                    buffer
                },
                options,
            )
        } else {
            unreachable!()
        }
        .unwrap_or_else(|e| {
            eprintln!("{}", e);
            std::process::exit(1)
        })
        .as_bytes(),
    )?;
    Ok(())
}
