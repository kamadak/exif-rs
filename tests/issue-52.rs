//! Test of issue 52: while exiftool can read metadata from PNGs written by some programs (GIMP, Darktable),
//! exif-rs can't.
//!
//! This uses a PNG written by Darktable (image-52.png) to reproduce and regression-test the issue.
//! The image is a photograph taken by Charles Eckman, released to this project for the purposes of
//! this test.

use std::{fs::File, io::BufReader};

use exif::Reader;

extern crate exif;

#[test]
fn issue_52_regression() {
    let file = File::open("tests/issue-52.png").unwrap();
    let mut bufreader = BufReader::new(&file);
    let exif1 = Reader::new()
        .read_from_container(&mut bufreader)
        .expect("can decode EXIF values from PNG");
    let copyright = exif1
        .get_field(exif::Tag::Copyright, exif::In::PRIMARY)
        .unwrap();

    let copyright = copyright.display_value().to_string();
    assert_eq!(&copyright, "\"Charles Eckman\"");
}
