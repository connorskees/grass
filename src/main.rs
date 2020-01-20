use clap::{App, Arg};
use std::io::{BufWriter, stdout};

use grass::{SassResult, StyleSheet};

fn main() -> SassResult<()> {
    let matches = App::new("grass")
        .version(env!("CARGO_PKG_VERSION"))
        .about("SCSS Compiler in rust")
        .version_short("v")
        .arg(
            Arg::with_name("stdin")
                .long("stdin")
                .help("Read the stylesheet from stdin"),
        )
        .arg(
            Arg::with_name("indented")
                .long("indented")
                .help("Use the indented syntax for input from stdin"),
        )
        .arg(
            Arg::with_name("LOAD_PATH")
                .short("I")
                .long("load-path")
                .help("A path to use when resolving imports")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("style")
                // dart sass uses -s
                .short("s")
                // ruby sass uses -t
                .short("t")
                .long("style")
                .help("Expanded (default) or compressed output")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("update")
                .long("update")
                .help("Only compile out-of-date stylesheets"),
        )
        .arg(
            Arg::with_name("precision")
                .long("precision")
                .help("Number of digits to emit for floats")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("quiet")
                .short("q")
                .long("quiet")
                .help("Number of digits to emit for floats"),
        )
        .arg(
            Arg::with_name("INPUT")
                .required(true)
                .multiple(true)
                .help("SCSS files"),
        )
        .get_matches();

    let mut stdout = BufWriter::new(stdout());
    if let Some(inputs) = matches.values_of("INPUT") {
        for name in inputs {
            StyleSheet::from_path(name)?.print_as_css(&mut stdout)?;
        }
    }
    Ok(())
}
