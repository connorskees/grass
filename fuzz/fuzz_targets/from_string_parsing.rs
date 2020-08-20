#![no_main]
use libfuzzer_sys::fuzz_target;


fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let options = grass::Options::default();

        let _ = grass::from_string(
            s.to_owned(),
            &options
        );
    }
});
