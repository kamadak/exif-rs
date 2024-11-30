//
// Copyright (c) 2017 KAMADA Ken'ichi.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.
//

//! Read, write, re-read, and compare the results.
//!
//! This test can be also compiled as a command-line program.

extern crate exif;

use std::env;
use std::fs::File;
use std::io::{BufReader, Cursor};
use std::path::Path;

#[cfg(not(test))]
use exif::Error;
use exif::{Exif, In, Reader, Value, Tag};
use exif::experimental::Writer;

#[test]
fn exif_heic() {
    rwr_compare("tests/exif.heic");
}

#[test]
fn exif_jpg() {
    rwr_compare("tests/exif.jpg");
}

#[test]
fn exif_png() {
    rwr_compare("tests/exif.png");
}

#[test]
fn exif_tif() {
    rwr_compare("tests/exif.tif");
}

#[test]
fn exif_bif() {
    rwr_compare("tests/exif.bif");
}

#[test]
fn exif_webp() {
    rwr_compare("tests/exif.webp");
}

fn main() {
    for path in env::args_os().skip(1) {
        rwr_compare(&path);
    }
}

fn rwr_compare<P>(path: P) where P: AsRef<Path> {
    let path = path.as_ref();

    // Read.
    let file = File::open(path).unwrap();
    let mut bufreader = BufReader::new(&file);
    #[cfg(test)]
    let exif1 = Reader::new().read_from_container(&mut bufreader).unwrap();
    #[cfg(not(test))]
    let exif1 = match Reader::new().read_from_container(&mut bufreader) {
        Ok(exif) => exif,
        Err(e) => {
            println!("{}: {}: Skipped", path.display(), e);
            return;
        },
    };
    let strips = get_strips(&exif1, In::PRIMARY);
    let tn_strips = get_strips(&exif1, In::THUMBNAIL);
    let tiles = get_tiles(&exif1, In::PRIMARY);
    let tn_jpeg = get_jpeg(&exif1, In::THUMBNAIL);

    // Write.
    let mut writer = Writer::new();
    for f in exif1.fields() {
        writer.push_field(f);
    }
    if let Some(ref strips) = strips {
        writer.set_strips(strips, In::PRIMARY);
    }
    if let Some(ref tn_strips) = tn_strips {
        writer.set_strips(tn_strips, In::THUMBNAIL);
    }
    if let Some(ref tiles) = tiles {
        writer.set_tiles(tiles, In::PRIMARY);
    }
    if let Some(tn_jpeg) = tn_jpeg {
        writer.set_jpeg(tn_jpeg, In::THUMBNAIL);
    }
    let mut out = Cursor::new(Vec::new());
    #[cfg(test)]
    writer.write(&mut out, exif1.little_endian()).unwrap();
    #[cfg(not(test))]
    match writer.write(&mut out, exif1.little_endian()) {
        Ok(_) => {},
        Err(Error::NotSupported(_)) => {
            println!("{}: Contains unknown type", path.display());
            return;
        },
        e => e.unwrap(),
    }
    let out = out.into_inner();

    // Re-read.
    let exif2 = Reader::new().read_raw(out).unwrap();

    // Sort the fields (some files have wrong tag order).
    let mut fields1 = exif1.fields().collect::<Vec<_>>();
    fields1.sort_by_key(|f| (f.ifd_num, f.tag));
    let mut fields2 = exif2.fields().collect::<Vec<_>>();
    fields2.sort_by_key(|f| (f.ifd_num, f.tag));

    // Compare.
    if path.display().to_string() == "tests/exif.bif" {
        // TODO: Support IFD8
        eprintln!("{} fields1: {:?}", path.display(), fields1);
        eprintln!("{} fields2: {:?}", path.display(), fields2);
        assert_eq!(fields1.len() - 2, fields2.len());
        return;
    }
    assert_eq!(fields1.len(), fields2.len());
    for (f1, f2) in fields1.iter().zip(fields2.iter()) {
        assert_eq!(f1.tag, f2.tag);
        assert_eq!(f1.ifd_num, f2.ifd_num);
        match f1.tag {
            Tag::StripOffsets | Tag::TileOffsets |
            Tag::JPEGInterchangeFormat => continue,
            _ => {},
        }
        compare_field_value(&f1.value, &f2.value);
    }
    assert_eq!(get_strips(&exif2, In::PRIMARY), strips);
    assert_eq!(get_strips(&exif2, In::THUMBNAIL), tn_strips);
    assert_eq!(get_tiles(&exif2, In::PRIMARY), tiles);
    assert_eq!(get_jpeg(&exif2, In::THUMBNAIL), tn_jpeg);
}

// Compare field values.
fn compare_field_value(value1: &Value, value2: &Value) {
    // The TIFF standard requires that BYTE, SHORT, or LONG values should
    // be accepted for any unsigned integer field.
    if let (Some(it1), Some(it2)) = (value1.iter_uint(), value2.iter_uint()) {
        assert!(it1.eq(it2));
        return;
    }
    // Compare other fields strictly.
    match (value1, value2) {
        (Value::Ascii(v1), Value::Ascii(v2)) =>
            assert_eq!(v1, v2),
        (Value::Rational(v1), Value::Rational(v2)) => {
            assert_eq!(v1.len(), v2.len());
            for (r1, r2) in v1.iter().zip(v2.iter()) {
                assert_eq!(r1.num, r2.num);
                assert_eq!(r1.denom, r2.denom);
            }
        },
        (Value::SByte(v1), Value::SByte(v2)) =>
            assert_eq!(v1, v2),
        (Value::Undefined(v1, _), Value::Undefined(v2, _)) =>
            assert_eq!(v1, v2),
        (Value::SShort(v1), Value::SShort(v2)) =>
            assert_eq!(v1, v2),
        (Value::SLong(v1), Value::SLong(v2)) =>
            assert_eq!(v1, v2),
        (Value::SRational(v1), Value::SRational(v2)) => {
            assert_eq!(v1.len(), v2.len());
            for (r1, r2) in v1.iter().zip(v2.iter()) {
                assert_eq!(r1.num, r2.num);
                assert_eq!(r1.denom, r2.denom);
            }
        },
        (Value::Float(v1), Value::Float(v2)) =>
            assert_eq!(v1, v2),
        (Value::Double(v1), Value::Double(v2)) =>
            assert_eq!(v1, v2),
        _ => panic!("{:?} != {:?}", value1, value2),
    }
}

fn get_strips(exif: &Exif, ifd_num: In) -> Option<Vec<&[u8]>> {
    let offsets = exif.get_field(Tag::StripOffsets, ifd_num)
        .and_then(|f| f.value.iter_uint());
    let counts = exif.get_field(Tag::StripByteCounts, ifd_num)
        .and_then(|f| f.value.iter_uint());
    let (offsets, counts) = match (offsets, counts) {
        (Some(offsets), Some(counts)) => (offsets, counts),
        (None, None) => return None,
        _ => panic!("inconsistent strip offsets and byte counts"),
    };
    let buf = exif.buf();
    assert_eq!(offsets.len(), counts.len());
    let strips = offsets.zip(counts).map(
        |(ofs, cnt)| &buf[ofs as usize .. (ofs + cnt) as usize]).collect();
    Some(strips)
}

fn get_tiles(exif: &Exif, ifd_num: In) -> Option<Vec<&[u8]>> {
    let offsets = exif.get_field(Tag::TileOffsets, ifd_num)
        .and_then(|f| f.value.iter_uint());
    let counts = exif.get_field(Tag::TileByteCounts, ifd_num)
        .and_then(|f| f.value.iter_uint());
    let (offsets, counts) = match (offsets, counts) {
        (Some(offsets), Some(counts)) => (offsets, counts),
        (None, None) => return None,
        _ => panic!("inconsistent tile offsets and byte counts"),
    };
    assert_eq!(offsets.len(), counts.len());
    let buf = exif.buf();
    let strips = offsets.zip(counts).map(
        |(ofs, cnt)| &buf[ofs as usize .. (ofs + cnt) as usize]).collect();
    Some(strips)
}

fn get_jpeg(exif: &Exif, ifd_num: In) -> Option<&[u8]> {
    let offset = exif.get_field(Tag::JPEGInterchangeFormat, ifd_num)
        .and_then(|f| f.value.get_uint(0));
    let len = exif.get_field(Tag::JPEGInterchangeFormatLength, ifd_num)
        .and_then(|f| f.value.get_uint(0));
    let (offset, len) = match (offset, len) {
        (Some(offset), Some(len)) => (offset as usize, len as usize),
        (None, None) => return None,
        _ => panic!("inconsistent JPEG offset and length"),
    };
    let buf = exif.buf();
    Some(&buf[offset..offset+len])
}
