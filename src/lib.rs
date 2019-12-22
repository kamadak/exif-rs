//
// Copyright (c) 2016 KAMADA Ken'ichi.
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

//! This is a pure-Rust library to parse Exif data.
//! This library can parse TIFF and JPEG images and extract Exif
//! attributes.
//!
//! # Examples
//!
//! An example to parse JPEG/TIFF files:
//!
//! ```
//! for path in &["tests/exif.jpg", "tests/exif.tif"] {
//!     let file = std::fs::File::open(path).unwrap();
//!     let reader = exif::Reader::new(
//!         &mut std::io::BufReader::new(&file)).unwrap();
//!     for f in reader.fields() {
//!         println!("{} {} {}",
//!                  f.tag, f.ifd_num, f.display_value().with_unit(&reader));
//!     }
//! }
//! ```
//!
//! # Upgrade Guide (0.3.x to 0.4.x)
//!
//! ## API compatibilities
//!
//! * Use struct `In` instead of `bool` to indicate primary/thumbnail images.
//!   - On `Reader::get_field`, the old code in 0.3.x:
//!     ```ignore
//!     reader.get_field(Tag::DateTime, false)
//!     ```
//!     The new code in 0.4.x:
//!     ```
//!     # use exif::{In, Reader, Tag};
//!     # let file = std::fs::File::open("tests/exif.tif").unwrap();
//!     # let reader = Reader::new(
//!     #     &mut std::io::BufReader::new(&file)).unwrap();
//!     reader.get_field(Tag::DateTime, In::PRIMARY)
//!     # ;
//!     ```
//!     As an additional feature, access to the 2nd or further IFD,
//!     which some RAW formats may have, is also possible in 0.4.x:
//!     ```
//!     # use exif::{In, Reader, Tag};
//!     # let file = std::fs::File::open("tests/exif.tif").unwrap();
//!     # let reader = Reader::new(
//!     #     &mut std::io::BufReader::new(&file)).unwrap();
//!     reader.get_field(Tag::ImageWidth, In(2))
//!     # ;
//!     ```
//!   - On `Field`, the old code in 0.3.x:
//!     ```ignore
//!     if field.thumbnail {
//!         // for the thumbnail image
//!     } else {
//!         // for the primary image
//!     }
//!     ```
//!     The new code in 0.4.x:
//!     ```
//!     # use exif::{In, Reader};
//!     # let file = std::fs::File::open("tests/exif.tif").unwrap();
//!     # let reader = Reader::new(
//!     #     &mut std::io::BufReader::new(&file)).unwrap();
//!     # let field = reader.fields().next().unwrap();
//!     match field.ifd_num {
//!         In::PRIMARY => {},   // for the primary image
//!         In::THUMBNAIL => {}, // for the thumbnail image
//!         _ => {},
//!     }
//!     ```
//! * `Reader::fields` now returns an iterator instead of a slice.
//! * Enum variants of `Context` and `Error` are no longer exhaustive.
//!   You need a wildcard arm (`_`) in a match expression.
//! * The associated value of `Value::Undefined` and `Value::Ascii` has
//!   been changed from a slice to a `Vec`.
//!
//! ## Other new features
//!
//! * `Field::display_value` has been introduced.
//!   It is usually handier than `Value::display_as`.
//!   ```
//!   # let file = std::fs::File::open("tests/exif.tif").unwrap();
//!   # let reader = exif::Reader::new(
//!   #     &mut std::io::BufReader::new(&file)).unwrap();
//!   # let field = reader.fields().next().unwrap();
//!   assert_eq!(field.display_value().to_string(),
//!              field.value.display_as(field.tag).to_string());
//!   ```
//! * Displaying a value with its unit is supported.
//!   ```
//!   # let file = std::fs::File::open("tests/exif.tif").unwrap();
//!   # let reader = exif::Reader::new(
//!   #     &mut std::io::BufReader::new(&file)).unwrap();
//!   # let field = reader.fields().next().unwrap();
//!   // Display the value only.
//!   println!("{}", field.display_value());
//!   // Display the value with its unit.  If the unit depends on another
//!   // field, it is taken from the reader.
//!   println!("{}", field.display_value().with_unit(&reader));
//!   ```
//! * `Value` and `Field` are self-contained and no longer borrow the raw
//!   buffer or `Reader` (thanks to the change of `Value::Undefined`
//!   and `Value::Ascii` described above).

pub use error::Error;
pub use jpeg::get_exif_attr as get_exif_attr_from_jpeg;
pub use reader::Reader;
pub use tag::{Context, Tag};
pub use tiff::{DateTime, Field, In};
pub use tiff::parse_exif_compat03 as parse_exif;
pub use value::Value;
pub use value::{Rational, SRational};

/// The interfaces in this module are experimental and unstable.
pub mod experimental {
    pub use crate::writer::Writer;
}

#[cfg(test)]
#[macro_use]
mod tmacro;

mod endian;
mod error;
mod jpeg;
mod reader;
mod tag;
mod tiff;
mod util;
mod value;
mod writer;
