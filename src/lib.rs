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
//!
//! This library parses Exif attributes in a raw Exif data block.
//! It can also read Exif data directly from some image formats
//! including TIFF, JPEG, HEIF, PNG, and WebP.
//!
//! # Examples
//!
//! To parse Exif attributes in an image file,
//! use `Reader::read_from_container`.
//! To convert a field value to a string, use `Field::display_value`.
//!
//! ```
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! for path in &["tests/exif.jpg", "tests/exif.tif"] {
//!     let file = std::fs::File::open(path)?;
//!     let mut bufreader = std::io::BufReader::new(&file);
//!     let exifreader = exif::Reader::new();
//!     let exif = exifreader.read_from_container(&mut bufreader)?;
//!     for f in exif.fields() {
//!         println!("{} {} {}",
//!                  f.tag, f.ifd_num, f.display_value().with_unit(&exif));
//!     }
//! }
//! # Ok(()) }
//! ```
//!
//! To process a field value programmatically in your application,
//! use the value itself (associated value of enum `Value`)
//! rather than a stringified one.
//!
//! ```
//! # use exif::{In, Reader, Tag, Value};
//! # let file = std::fs::File::open("tests/exif.tif").unwrap();
//! # let exif = Reader::new().read_from_container(
//! #     &mut std::io::BufReader::new(&file)).unwrap();
//! # macro_rules! eprintln { ($($tt:tt)*) => (panic!($($tt)*)) }
//! // Orientation is stored as a SHORT.
//! match exif.get_field(Tag::Orientation, In::PRIMARY) {
//!     Some(orientation) =>
//!         // You could match `orientation.value` against `Value::Short`,
//!         // but the standard recommends that BYTE, SHORT, or LONG should
//!         // be accepted.  `Value::get_uint` is provided for that purpose.
//!         match orientation.value.get_uint(0) {
//!             Some(v @ 1..=8) => {
//!                 println!("Orientation {}", v);
//!                 # assert_eq!(v, 1);
//!             },
//!             _ => eprintln!("Orientation value is broken"),
//!         },
//!     None => eprintln!("Orientation tag is missing"),
//! }
//! // XResolution is stored as a RATIONAL.
//! match exif.get_field(Tag::XResolution, In::PRIMARY) {
//!     Some(xres) =>
//!         match xres.value {
//!             Value::Rational(ref v) if !v.is_empty() => {
//!                 println!("XResolution {}", v[0]);
//!                 # assert_eq!(v[0].num, 72);
//!                 # assert_eq!(v[0].denom, 1);
//!             },
//!             _ => eprintln!("XResolution value is broken"),
//!         },
//!     None => eprintln!("XResolution tag is missing"),
//! }
//! ```
//!
//! # Upgrade Guide
//!
//! See the [upgrade guide](doc/upgrade/index.html) for API incompatibilities.

pub use error::Error;
pub use jpeg::get_exif_attr as get_exif_attr_from_jpeg;
pub use reader::{Exif, Reader};
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

pub mod doc;
mod endian;
mod error;
mod isobmff;
mod jpeg;
mod png;
mod reader;
mod tag;
mod tiff;
mod util;
mod value;
mod webp;
mod writer;
