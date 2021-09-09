use std::{
    fs::OpenOptions,
    io::{stdin, stdout, BufWriter, Read, Write},
    path::Path,
};

use clap::{arg_enum, App, AppSettings, Arg};

#[cfg(not(feature = "wasm"))]
use grass::{from_path, from_string, Options, OutputStyle};

// TODO remove this
arg_enum! {
    #[derive(PartialEq, Debug)]
    pub enum Style {
        Expanded,
        Compressed,
    }
}

arg_enum! {
    #[derive(PartialEq, Debug)]
    pub enum SourceMapUrls {
        Relative,
        Absolute,
    }
}

#[cfg(feature = "wasm")]
fn main() {}

#[cfg(not(feature = "wasm"))]
#[cfg_attr(feature = "profiling", inline(never))]
fn main() -> std::io::Result<()> {
    let matches = App::new("grass")
        .setting(AppSettings::ColoredHelp)
        .version(env!("CARGO_PKG_VERSION"))
        .about("A near-feature-complete Sass compiler written purely in Rust")
        .version_short("v")
        .arg(
            Arg::with_name("STDIN")
                .long("stdin")
                .help("Read the stylesheet from stdin"),
        )
        .arg(
            Arg::with_name("INDENTED")
                .long("indented")
                .hidden(true)
                .help("Use the indented syntax for input from stdin"),
        )
        .arg(
            Arg::with_name("LOAD_PATH")
                .short("I")
                .long("load-path")
                .help("A path to use when resolving imports. May be passed multiple times.")
                .multiple(true)
                .takes_value(true)
                .number_of_values(1)
        )
        .arg(
            Arg::with_name("STYLE")
                // this is required for compatibility with ruby sass
                .short("t") // FIXME change this to short_alias later
                .short("s")
                .long("style")
                .help("Minified or expanded output")
                .default_value("expanded")
                .case_insensitive(true)
                .possible_values(&Style::variants())
                .takes_value(true),
        )
        .arg(
            Arg::with_name("NO_CHARSET")
                .long("no-charset")
                .help("Don't emit a @charset or BOM for CSS with non-ASCII characters."),
        )
        .arg(
            Arg::with_name("UPDATE")
                .long("update")
                .hidden(true)
                .help("Only compile out-of-date stylesheets."),
        )
        .arg(
            Arg::with_name("NO_ERROR_CSS")
                .long("no-error-css")
                .hidden(true)
                .help("When an error occurs, don't emit a stylesheet describing it."),
        )
        // Source maps
        .arg(
            Arg::with_name("NO_SOURCE_MAP")
                .long("no-source-map")
                .hidden(true)
                .help("Whether to generate source maps."),
        )
        .arg(
            Arg::with_name("SOURCE_MAP_URLS")
                .long("source-map-urls")
                .hidden(true)
                .help("How to link from source maps to source files.")
                .default_value("relative")
                .case_insensitive(true)
                .possible_values(&SourceMapUrls::variants())
                .takes_value(true),
        )
        .arg(
            Arg::with_name("EMBED_SOURCES")
                .long("embed-sources")
                .hidden(true)
                .help("Embed source file contents in source maps."),
        )
        .arg(
            Arg::with_name("EMBED_SOURCE_MAP")
                .long("embed-source-map")
                .hidden(true)
                .help("Embed source map contents in CSS."),
        )
        // Other
        .arg(
            Arg::with_name("WATCH")
                .long("watch")
                .hidden(true)
                .help("Watch stylesheets and recompile when they change."),
        )
        .arg(
            Arg::with_name("POLL")
                .long("poll")
                .hidden(true)
                .help("Manually check for changes rather than using a native watcher. Only valid with --watch.")
                .requires("WATCH"),
        )
        .arg(
            Arg::with_name("NO_STOP_ON_ERROR")
                .long("no-stop-on-error")
                .hidden(true)
                .help("Continue to compile more files after error is encountered.")
        )
        .arg(
            Arg::with_name("INTERACTIVE")
                .short("i")
                .long("interactive")
                .hidden(true)
                .help("Run an interactive SassScript shell.")
        )
        .arg(
            Arg::with_name("NO_COLOR")
                .short("c")
                .long("no-color")
                .hidden(true)
                .help("Whether to use terminal colors for messages.")
        )
        .arg(
            Arg::with_name("NO_UNICODE")
                .long("no-unicode")
                .help("Whether to use Unicode characters for messages.")
        )
        .arg(
            Arg::with_name("QUIET")
                .short("q")
                .long("quiet")
                .help("Don't print warnings."),
        )
        .arg(
            Arg::with_name("INPUT")
                .required_unless("STDIN")
                .help("SCSS files"),
        )
        .arg(
            Arg::with_name("OUTPUT")
                .help("Output SCSS file")
        )

        // Hidden, legacy arguments
        .arg(
            Arg::with_name("PRECISION")
                .long("precision")
                .hidden(true)
                .takes_value(true)
        )
        .get_matches();

    let load_paths = matches
        .values_of("LOAD_PATH")
        .map_or_else(Vec::new, |vals| vals.map(Path::new).collect());

    let style = match &matches.value_of("STYLE").unwrap().to_lowercase() as &str {
        "expanded" => OutputStyle::Expanded,
        "compressed" => OutputStyle::Compressed,
        _ => unreachable!(),
    };

    let options = &Options::default()
        .load_paths(&load_paths)
        .style(style)
        .quiet(matches.is_present("QUIET"))
        .unicode_error_messages(!matches.is_present("NO_UNICODE"))
        .allows_charset(!matches.is_present("NO_CHARSET"));

    let (mut stdout_write, mut file_write);
    let buf_out: &mut dyn Write = if let Some(path) = matches.value_of("OUTPUT") {
        file_write = BufWriter::new(
            OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(path)?,
        );
        &mut file_write
    } else {
        stdout_write = BufWriter::new(stdout());
        &mut stdout_write
    };

    buf_out.write_all(
        if let Some(name) = matches.value_of("INPUT") {
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
