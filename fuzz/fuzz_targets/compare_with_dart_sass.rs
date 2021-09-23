#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let options = grass::Options::default();

        let grass_result = grass::from_string(s.to_owned(), &options);
        let sass_process_result = run_dart_sass(s);

        if let Ok(sass_result) = sass_process_result {
            if sass_result.status.success() != grass_result.is_ok() {
                panic!(format!(
                    r#"Difference in output between grass and sass for:

{:?}

dart sass succeeded:{}
    grass succeeded:{}"#,
                    s,
                    sass_result.status.success(),
                    grass_result.is_ok()
                ));
            }
        }
    }
});

fn run_dart_sass(input: &str) -> Result<std::process::Output, Box<dyn std::error::Error>> {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new("sass")
        .arg("--stdin")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    {
        let child_stdin = child.stdin.as_mut().unwrap();
        child_stdin.write_all(input.as_bytes())?;
    }

    Ok(child.wait_with_output()?)
}
