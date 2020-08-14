#![no_main]
use libfuzzer_sys::fuzz_target;

use grass::{from_path, from_string, Options};

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let options = Options::default();

        grass::from_string(
            s.to_owned(),
            &options
        );
    }
});
