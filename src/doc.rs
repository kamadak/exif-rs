//
// Copyright (c) 2020 KAMADA Ken'ichi.
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

//! Documentation

// Rust 1.54 stabilized invoking function-like macros in attributes.
// We will use it after bumping MSRV.
// #![feature(extended_key_value_attributes)],
// #[doc = include_str!("../NEWS")]
// pub mod news {}
macro_rules! doc_module_with_external_source {
    ($( #[$attr:meta] )*
     $name: ident, $doc: expr) => {
        $( #[$attr] )*
        #[doc = ""]
        #[doc = $doc]
        pub mod $name {}
    }
}
doc_module_with_external_source!(
    /// # News
    news, include_str!("../NEWS"));

/// # Upgrade Guide
///
/// ## Upgrade from 0.5.x to 0.6.x
///
/// ### API compatibilities
///
/// ## Upgrade from 0.4.x to 0.5.x
///
/// ### API compatibilities
///
/// * `Reader` has been split into two: `Reader` and `Exif`.
///   `Reader` is now the builder for `Exif`, and `Exif` provides
///   access to `Field`s via `get_field`, `fields`, and other methods.
///   Old code `Reader::new(data)` should be changed to
///   `Reader::new().read_raw(data)` or
///   `Reader::new().read_from_container(data)`.
///
///   The old code using 0.4.x:
///   ```ignore
///   # use exif::Reader;
///   # let file = std::fs::File::open("tests/exif.jpg").unwrap();
///   # let mut bufreader = std::io::BufReader::new(&file);
///   let reader = Reader::new(&mut bufreader).unwrap();
///   for f in reader.fields() { /* do something */ }
///   ```
///   The new code using 0.5.x:
///   ```
///   # use exif::{Exif, Reader};
///   # let file = std::fs::File::open("tests/exif.jpg").unwrap();
///   # let mut bufreader = std::io::BufReader::new(&file);
///   let exif = Reader::new().read_from_container(&mut bufreader).unwrap();
///   for f in exif.fields() { /* do something */ }
///   ```
///
/// ### Other new features
///
/// * Support for parsing Exif in HEIF (HEIC/AVIF) has been added.
///
/// ## Upgrade from 0.3.x to 0.4.x
///
/// ### API compatibilities
///
/// * Use struct `In` instead of `bool` to indicate primary/thumbnail images.
///   - On `Reader::get_field`, the old code using 0.3.x:
///     ```ignore
///     reader.get_field(Tag::DateTime, false)
///     ```
///     The new code using 0.4.x:
///     ```ignore
///     # use exif::{In, Reader, Tag};
///     # let file = std::fs::File::open("tests/exif.tif").unwrap();
///     # let reader = Reader::new(
///     #     &mut std::io::BufReader::new(&file)).unwrap();
///     reader.get_field(Tag::DateTime, In::PRIMARY)
///     # ;
///     ```
///     As an additional feature, access to the 2nd or further IFD,
///     which some TIFF-based RAW formats may have, is also possible
///     with 0.4.x:
///     ```ignore
///     # use exif::{In, Reader, Tag};
///     # let file = std::fs::File::open("tests/exif.tif").unwrap();
///     # let reader = Reader::new(
///     #     &mut std::io::BufReader::new(&file)).unwrap();
///     reader.get_field(Tag::ImageWidth, In(2))
///     # ;
///     ```
///   - On `Field`, the old code using 0.3.x:
///     ```ignore
///     if field.thumbnail {
///         // for the thumbnail image
///     } else {
///         // for the primary image
///     }
///     ```
///     The new code using 0.4.x:
///     ```
///     # use exif::{In, Reader};
///     # let file = std::fs::File::open("tests/exif.tif").unwrap();
///     # let exif = Reader::new().read_from_container(
///     #     &mut std::io::BufReader::new(&file)).unwrap();
///     # let field = exif.fields().next().unwrap();
///     match field.ifd_num {
///         In::PRIMARY => {},   // for the primary image
///         In::THUMBNAIL => {}, // for the thumbnail image
///         _ => {},
///     }
///     ```
/// * `Reader::fields` now returns an iterator instead of a slice.
/// * Enum variants of `Context` and `Error` are no longer exhaustive.
///   You need a wildcard arm (`_`) in a match expression.
/// * The associated value of `Value::Undefined` and `Value::Ascii` has
///   been changed from a slice to a `Vec`.
///
/// ### Other new features
///
/// * `Field::display_value` has been introduced.
///   It is usually handier than `Value::display_as`.
///   ```
///   # let file = std::fs::File::open("tests/exif.tif").unwrap();
///   # let exif = exif::Reader::new().read_from_container(
///   #     &mut std::io::BufReader::new(&file)).unwrap();
///   # let field = exif.fields().next().unwrap();
///   assert_eq!(field.display_value().to_string(),
///              field.value.display_as(field.tag).to_string());
///   ```
/// * Displaying a value with its unit is supported.
///   ```ignore
///   # let file = std::fs::File::open("tests/exif.tif").unwrap();
///   # let reader = exif::Reader::new(
///   #     &mut std::io::BufReader::new(&file)).unwrap();
///   # let field = reader.fields().next().unwrap();
///   // Display the value only.
///   println!("{}", field.display_value());
///   // Display the value with its unit.  If the unit depends on another
///   // field, it is taken from the reader.
///   println!("{}", field.display_value().with_unit(&reader));
///   ```
/// * `Value` and `Field` are self-contained and no longer borrow the raw
///   buffer or `Reader` (thanks to the change of `Value::Undefined`
///   and `Value::Ascii` described above).
pub mod upgrade {}
