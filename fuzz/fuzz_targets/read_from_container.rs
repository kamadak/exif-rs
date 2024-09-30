#![no_main]

use std::io::Cursor;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let reader = exif::Reader::new();
    let mut container = Cursor::new(data);
    let _ = reader.read_from_container(&mut container);
});
