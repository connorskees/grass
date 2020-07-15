use std::{
    fs::File,
    io::{stdout, BufWriter, Write},
    path::Path,
};

use clap::{arg_enum, App, AppSettings, Arg};

#[cfg(not(feature = "wasm"))]
use grass::{from_path, Options};

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
        .about("SCSS Compiler in rust")
        .version_short("v")
        .arg(
            Arg::with_name("STDIN")
                .long("stdin")
                .hidden(true)
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
        )
        .arg(
            Arg::with_name("STYLE")
                .short("s")
                // this is required for compatibility with ruby sass
                .short("t")
                .long("style")
                .hidden(true)
                .help("Minified or expanded output")
                .default_value("expanded")
                .case_insensitive(true)
                .possible_values(&Style::variants())
                .takes_value(true),
        )
        .arg(
            Arg::with_name("NO_CHARSET")
                .long("no-charset")
                .hidden(true)
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
                .hidden(true)
                .help("Whether to use Unicode characters for messages.")
        )
        .arg(
            Arg::with_name("QUIET")
                .short("q")
                .long("quiet")
                .hidden(true)
                .help("Don't print warnings."),
        )
        .arg(
            Arg::with_name("INPUT")
                .required(true)
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

    let vals: Vec<&Path>;
    if let Some(load_paths) = matches.values_of("LOAD_PATH") {
        vals = load_paths.map(|p| Path::new(p)).collect();
    } else {
        vals = Vec::new();
    }

    let options = &Options::default().load_paths(&vals);

    if let Some(name) = matches.value_of("INPUT") {
        if let Some(path) = matches.value_of("OUTPUT") {
            let mut buf = BufWriter::new(File::open(path).unwrap_or(File::create(path)?));
            buf.write_all(
                from_path(name, &options)
                    .unwrap_or_else(|e| {
                        eprintln!("{}", e);
                        std::process::exit(1)
                    })
                    .as_bytes(),
            )?;
        } else {
            let mut stdout = BufWriter::new(stdout());
            stdout.write_all(
                from_path(name, &options)
                    .unwrap_or_else(|e| {
                        eprintln!("{}", e);
                        std::process::exit(1)
                    })
                    .as_bytes(),
            )?;
        }
    }
    Ok(())
}
