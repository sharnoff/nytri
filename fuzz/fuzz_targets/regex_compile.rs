#![no_main]
use libfuzzer_sys::fuzz_target;
use nytri::Regex;

fuzz_target!(|data: &str| {
    let _ = Regex::new(data);
});
