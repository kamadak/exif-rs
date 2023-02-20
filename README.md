# exif-rs
[![Crates.io](https://img.shields.io/crates/v/kamadak-exif.svg)](https://crates.io/crates/kamadak-exif)
[![Rust](https://github.com/kamadak/exif-rs/actions/workflows/rust.yml/badge.svg)](https://github.com/kamadak/exif-rs/actions)
## Exif parsing library written in pure Rust

   This is a pure-Rust library to parse Exif data.
   This library parses Exif attributes in a raw Exif data block.
   It can also read Exif data directly from some image formats.

   Supported formats are:
   -  TIFF and some RAW image formats based on it
   -  JPEG
   -  HEIF and coding-specific variations including HEIC and AVIF
   -  PNG
   -  WebP

## Usage

   Add a dependency entry to your Cargo.toml.  Specify "kamadak-exif"
   if you use crates.io.  The canonical name of this crate is "exif",
   but it is renamed on crates.io to avoid a naming conflict.

      [dependencies]
      kamadak-exif = "*"

   Add the following to your crate root (before Rust 2018).

      extern crate exif;

   Run "cargo doc" in the source directory to generate the API reference.
   It is also available online at <https://docs.rs/kamadak-exif>.

   See examples directory for sample codes.

## MSRV

   Rust 1.60 or later is required to build.

## Standards

   -  Exif Version 2.32
   -  DCF Version 2.0 (Edition 2010)
   -  TIFF Revision 6.0
   -  ISO/IEC 14496-12:2015
   -  ISO/IEC 23008-12:2017
   -  PNG Specification, Version 1.2
   -  Extensions to the PNG 1.2 Specification, version 1.5.0
   -  WebP Container Specification, committed on 2018-04-20
