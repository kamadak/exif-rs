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

extern crate exif;

use std::fs::File;
use std::io::BufReader;

use exif::{DateTime, Reader, Value, Tag};

fn main() {
    let file = File::open("tests/exif.jpg").unwrap();
    let reader = Reader::new(&mut BufReader::new(&file)).unwrap();

    // To obtain a string representation, `Value::display_as`
    // or `Field::display_value` can be used.  To display a value with its
    // unit, call `with_unit` on the return value of `Field::display_value`.
    let tag_list = [Tag::ExifVersion,
                    Tag::PixelXDimension,
                    Tag::XResolution,
                    Tag::ImageDescription,
                    Tag::DateTime];
    for &tag in tag_list.iter() {
        if let Some(field) = reader.get_field(tag, false) {
            println!("{}: {}",
                     field.tag, field.display_value().with_unit(&reader));
        }
    }

    // To get unsigned integer value(s) from either of BYTE, SHORT,
    // or LONG, `Value::get_uint` or `Value::iter_uint` can be used.
    if let Some(field) = reader.get_field(Tag::PixelXDimension, false) {
        if let Some(width) = field.value.get_uint(0) {
            println!("Valid width of the image is {}.", width);
        }
    }

    // To convert a Rational or SRational to an f64, `Rational::to_f64`
    // or `SRational::to_f64` can be used.
    if let Some(field) = reader.get_field(Tag::XResolution, false) {
        match field.value {
            Value::Rational(ref vec) if !vec.is_empty() =>
                println!("X resolution is {}.", vec[0].to_f64()),
            _ => {},
        }
    }

    // To parse a DateTime-like field, `DateTime::from_ascii` can be used.
    if let Some(field) = reader.get_field(Tag::DateTime, false) {
        match field.value {
            Value::Ascii(ref vec) if !vec.is_empty() => {
                if let Ok(datetime) = DateTime::from_ascii(vec[0]) {
                    println!("Year of DateTime is {}.", datetime.year);
                }
            },
            _ => {},
        }
    }
}
