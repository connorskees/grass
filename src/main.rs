use std::{
    fs::{self, File, OpenOptions},
    io::{self, stdin, stdout, BufWriter, Read, Write},
    path::Path,
};

use clap::{arg_enum, App, AppSettings, Arg};
use walkdir::{DirEntry, WalkDir};

#[cfg(not(feature = "wasm"))]
use grass::{from_path, from_string, Options, Result};

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

/// Identify if file is Sass entrypoint.
fn is_entrypoint(entry: &DirEntry) -> bool {
    matches!(entry.file_name().to_str(), Some(s) if !s.starts_with('_') && is_xcssfile(s))
}

/// Check if string ends with Sass or Scss.
fn is_xcssfile(s: &str) -> bool {
    [".sass", ".scss"].iter().any(|ext| s.ends_with(ext))
}

/// Write output result to file or standard output or send error to standard error.
fn write_file(result: Result<String>, output: Option<&str>) -> io::Result<()> {
    let (mut stdout_write, mut file_write);
    let buf_out: &mut dyn Write = if let Some(path) = output {
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

    let output = result.unwrap_or_else(|e| {
        eprintln!("{}", e);
        std::process::exit(1)
    });
    buf_out.write_all(output.as_bytes())
}

#[cfg(feature = "wasm")]
fn main() {}

#[cfg(not(feature = "wasm"))]
#[cfg_attr(feature = "profiling", inline(never))]
fn main() -> io::Result<()> {
    let matches = App::new("grass")
        .setting(AppSettings::ColoredHelp)
        .version(env!("CARGO_PKG_VERSION"))
        .about("A near-feature-complete Sass compiler written purely in Rust")
        .version_short("v")
        .arg(
            Arg::with_name("STDIN")
                .long("stdin")
                .conflicts_with_all(&["INPUT", "UPDATE", "WATCH"])
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
                .help("Input SCSS file / directory"),
        )
        .arg(
            Arg::with_name("OUTPUT")
                .conflicts_with_all(&["UPDATE", "WATCH"])
                .help("Output SCSS file / directory")
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

    let options = &Options::default()
        .load_paths(&load_paths)
        .quiet(matches.is_present("QUIET"))
        .unicode_error_messages(!matches.is_present("NO_UNICODE"))
        .allows_charset(!matches.is_present("NO_CHARSET"));

    if matches.value_of("INPUT") == Some("-") || matches.is_present("STDIN") {
        let mut buffer = String::new();
        stdin().read_to_string(&mut buffer)?;
        write_file(from_string(buffer, options), matches.value_of("OUTPUT"))
    } else if let Some(input) = matches.value_of("INPUT") {
        if Path::new(input).is_file() {
            write_file(from_path(input, options), matches.value_of("OUTPUT"))
        } else {
            // input is a directory
            let output = match matches.value_of("OUTPUT") {
                Some(output) if is_xcssfile(output) => {
                    eprintln!("Output must be directory if input is directory.");
                    std::process::exit(1)
                }
                Some(output) => output,
                None => input,
            };

            for entry in WalkDir::new(input)
                .into_iter()
                .filter_entry(|e| e.file_type().is_dir() || is_entrypoint(e))
                .filter_map(|e| e.ok().filter(|e| e.file_type().is_file()))
            {
                let mut output = Path::new(output).join(entry.path().strip_prefix(input).unwrap());
                output.set_extension("css");
                let parent = output.parent().unwrap();
                fs::create_dir_all(parent).ok();

                // TODO prevent failure on invalid utf8 file name
                let input = entry.path().to_str().unwrap();
                let output = output.to_str().unwrap();

                let mut buffer = BufWriter::new(File::create(output)?);
                match from_path(input, options) {
                    Ok(output) => buffer.write_all(output.as_bytes())?,
                    Err(e) => eprintln!("{}", e),
                }
            }
            Ok(())
        }
    } else {
        unreachable!()
    }
}
