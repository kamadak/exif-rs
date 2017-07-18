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
//!         println!("{} {} {:?}", f.tag, f.thumbnail, f.value);
//!     }
//! }
//! ```
//!
//! # Compatibility
//!
//! Major changes between 0.2.3 and 0.3 are listed below.
//!
//! * Enum Error has two new variants: TooBig and NotSupported.

pub use error::Error;
pub use jpeg::get_exif_attr as get_exif_attr_from_jpeg;
pub use reader::Reader;
pub use tag_priv::{Context, Tag};
pub use tag_priv::constants as tag;
pub use tiff::{DateTime, Field};
pub use tiff::parse_exif;
pub use value::Value;
pub use value::{Rational, SRational};

/// The interfaces in this module is experimental and unstable.
pub mod experimental {
    pub use writer::Writer;
}

#[cfg(test)]
#[macro_use]
mod tmacro;

mod endian;
mod error;
mod jpeg;
mod reader;
#[path = "tag.rs"]
mod tag_priv;
mod tiff;
mod util;
mod value;
mod writer;
