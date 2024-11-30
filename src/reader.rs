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

use std::collections::HashMap;
use std::io;
use std::io::Read;

use crate::error::{Error, PartialResult};
use crate::isobmff;
use crate::jpeg;
use crate::png;
use crate::tag::Tag;
use crate::tiff;
use crate::tiff::{Field, IfdEntry, In, ProvideUnit};
use crate::webp;

/// A struct to parse the Exif attributes and
/// create an `Exif` instance that holds the results.
///
/// # Examples
/// ```
/// # use std::fmt::{Display, Formatter, Result};
/// # #[derive(Debug)] struct Error(&'static str);
/// # impl std::error::Error for Error {}
/// # impl Display for Error {
/// #     fn fmt(&self, f: &mut Formatter) -> Result { f.write_str(self.0) }
/// # }
/// # fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
/// use exif::{In, Reader, Tag};
/// let file = std::fs::File::open("tests/exif.jpg")?;
/// let exif = Reader::new()
///     .read_from_container(&mut std::io::BufReader::new(&file))?;
/// let xres = exif.get_field(Tag::XResolution, In::PRIMARY)
///     .ok_or(Error("tests/exif.jpg must have XResolution"))?;
/// assert_eq!(xres.display_value().with_unit(&exif).to_string(),
///            "72 pixels per inch");
/// # Ok(()) }
/// ```
pub struct Reader {
    continue_on_error: bool,
}

impl Default for Reader {
    fn default() -> Self {
        Self::new()
    }
}

impl Reader {
    /// Constructs a new `Reader`.
    pub fn new() -> Self {
        Self {
            continue_on_error: false,
        }
    }

    /// Sets the option to continue parsing on non-fatal errors.
    ///
    /// When this option is enabled, the parser will not stop on non-fatal
    /// errors and returns the results as far as they can be parsed.
    /// In such a case, `read_raw` and `read_from_container`
    /// return `Error::PartialResult`.
    /// The partial result and ignored errors can be obtained by
    /// [`Error::distill_partial_result`] or [`PartialResult::into_inner`].
    ///
    /// Note that a hard error (other than `Error::PartialResult`) may be
    /// returned even if this option is enabled.
    ///
    /// # Examples
    /// ```
    /// # use std::fmt::{Display, Formatter, Result};
    /// # fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    /// use exif::Reader;
    /// let file = std::fs::File::open("tests/exif.jpg")?;
    /// let exif = Reader::new()
    ///     .continue_on_error(true)
    ///     .read_from_container(&mut std::io::BufReader::new(&file))
    ///     .or_else(|e| e.distill_partial_result(|errors| {
    ///         errors.iter().for_each(|e| eprintln!("Warning: {}", e));
    ///     }))?;
    /// # Ok(()) }
    /// ```
    pub fn continue_on_error(&mut self, continue_on_error: bool) -> &mut Self {
        self.continue_on_error = continue_on_error;
        self
    }

    /// Parses the Exif attributes from raw Exif data.
    /// If an error occurred, `exif::Error` is returned.
    pub fn read_raw(&self, data: Vec<u8>) -> Result<Exif, Error> {
        let mut parser = tiff::Parser::new();
        parser.continue_on_error = self.continue_on_error.then(Vec::new);
        parser.parse(&data)?;
        let entry_map = parser.entries.iter().enumerate()
            .map(|(i, e)| (e.ifd_num_tag(), i)).collect();
        let exif = Exif {
            buf: data,
            entries: parser.entries,
            entry_map,
            little_endian: parser.little_endian,
            bigtiff: parser.bigtiff,
        };
        match parser.continue_on_error {
            Some(v) if !v.is_empty() =>
                Err(Error::PartialResult(PartialResult::new(exif, v))),
            _ => Ok(exif),
        }
    }

    /// Reads an image file and parses the Exif attributes in it.
    /// If an error occurred, `exif::Error` is returned.
    ///
    /// Supported formats are:
    /// - TIFF and some RAW image formats based on it
    /// - JPEG
    /// - HEIF and coding-specific variations including HEIC and AVIF
    /// - PNG
    /// - WebP
    ///
    /// This method is provided for the convenience even though
    /// parsing containers is basically out of the scope of this library.
    pub fn read_from_container<R>(&self, reader: &mut R) -> Result<Exif, Error>
    where R: io::BufRead + io::Seek {
        let mut buf = Vec::new();
        reader.by_ref().take(4096).read_to_end(&mut buf)?;
        if tiff::is_tiff(&buf) {
            reader.read_to_end(&mut buf)?;
        } else if jpeg::is_jpeg(&buf) {
            buf = jpeg::get_exif_attr(&mut buf.chain(reader))?;
        } else if png::is_png(&buf) {
            buf = png::get_exif_attr(&mut buf.chain(reader))?;
        } else if isobmff::is_heif(&buf) {
            reader.seek(io::SeekFrom::Start(0))?;
            buf = isobmff::get_exif_attr(reader)?;
        } else if webp::is_webp(&buf) {
            buf = webp::get_exif_attr(&mut buf.chain(reader))?;
        } else {
            return Err(Error::InvalidFormat("Unknown image format"));
        }

        self.read_raw(buf)
    }
}

/// A struct that holds the parsed Exif attributes.
///
/// # Examples
/// ```
/// # fn main() { sub(); }
/// # fn sub() -> Option<()> {
/// # use exif::{In, Reader, Tag};
/// # let file = std::fs::File::open("tests/exif.jpg").unwrap();
/// # let exif = Reader::new().read_from_container(
/// #     &mut std::io::BufReader::new(&file)).unwrap();
/// // Get a specific field.
/// let xres = exif.get_field(Tag::XResolution, In::PRIMARY)?;
/// assert_eq!(xres.display_value().with_unit(&exif).to_string(),
///            "72 pixels per inch");
/// // Iterate over all fields.
/// for f in exif.fields() {
///     println!("{} {} {}", f.tag, f.ifd_num, f.display_value());
/// }
/// # Some(()) }
/// ```
pub struct Exif {
    // TIFF data.
    buf: Vec<u8>,
    // Exif fields.  Vec is used to keep the ability to enumerate all fields
    // even if there are duplicates.
    entries: Vec<IfdEntry>,
    // HashMap to the index of the Vec for faster random access.
    entry_map: HashMap<(In, Tag), usize>,
    // True if the TIFF data is little endian.
    little_endian: bool,
    /// True if the TIFF data is in the BigTIFF format.
    bigtiff: bool,
}

impl Exif {
    /// Returns the slice that contains the TIFF data.
    #[inline]
    pub fn buf(&self) -> &[u8] {
        &self.buf[..]
    }

    /// Returns an iterator of Exif fields.
    #[inline]
    pub fn fields(&self) -> impl ExactSizeIterator<Item = &Field> {
        self.entries.iter()
            .map(move |e| {
                e.ref_field(&self.buf, self.little_endian)
        })
    }

    /// Returns true if the Exif data (TIFF structure) is in the
    /// little-endian byte order.
    #[inline]
    pub fn little_endian(&self) -> bool {
        self.little_endian
    }

    /// Returns true if the Exif data (TIFF structure) is in BigTIFF format.
    #[inline]
    pub fn bigtiff(&self) -> bool {
        self.bigtiff
    }

    /// Returns a reference to the Exif field specified by the tag
    /// and the IFD number.
    #[inline]
    pub fn get_field(&self, tag: Tag, ifd_num: In) -> Option<&Field> {
        self.entry_map.get(&(ifd_num, tag))
            .map(|&i| self.entries[i].ref_field(&self.buf, self.little_endian))
    }
}

impl<'a> ProvideUnit<'a> for &'a Exif {
    fn get_field(self, tag: Tag, ifd_num: In) -> Option<&'a Field> {
        self.get_field(tag, ifd_num)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::BufReader;
    use crate::tag::Context;
    use crate::value::Value;
    use super::*;

    #[test]
    fn get_field() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let exif = Reader::new().read_from_container(
            &mut BufReader::new(&file)).unwrap();
        match exif.get_field(Tag::ImageDescription, In(0)).unwrap().value {
            Value::Ascii(ref vec) => assert_eq!(vec, &[b"Test image"]),
            ref v => panic!("wrong variant {:?}", v)
        }
        match exif.get_field(Tag::ImageDescription, In(1)).unwrap().value {
            Value::Ascii(ref vec) => assert_eq!(vec, &[b"Test thumbnail"]),
            ref v => panic!("wrong variant {:?}", v)
        }
        match exif.get_field(Tag::ImageDescription, In(2)).unwrap().value {
            Value::Ascii(ref vec) => assert_eq!(vec, &[b"Test 2nd IFD"]),
            ref v => panic!("wrong variant {:?}", v)
        }
    }

    #[test]
    fn display_value_with_unit() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let exif = Reader::new().read_from_container(
            &mut BufReader::new(&file)).unwrap();
        // No unit.
        let exifver = exif.get_field(Tag::ExifVersion, In::PRIMARY).unwrap();
        assert_eq!(exifver.display_value().with_unit(&exif).to_string(),
                   "2.31");
        // Fixed string.
        let width = exif.get_field(Tag::ImageWidth, In::PRIMARY).unwrap();
        assert_eq!(width.display_value().with_unit(&exif).to_string(),
                   "17 pixels");
        // Unit tag (with a non-default value).
        let gpsalt = exif.get_field(Tag::GPSAltitude, In::PRIMARY).unwrap();
        assert_eq!(gpsalt.display_value().with_unit(&exif).to_string(),
                   "0.5 meters below sea level");
        // Unit tag is missing but the default is specified.
        let xres = exif.get_field(Tag::XResolution, In::PRIMARY).unwrap();
        assert_eq!(xres.display_value().with_unit(&exif).to_string(),
                   "72 pixels per inch");
        // Unit tag is missing and the default is not specified.
        let gpslat = exif.get_field(Tag::GPSLatitude, In::PRIMARY).unwrap();
        assert_eq!(gpslat.display_value().with_unit(&exif).to_string(),
                   "10 deg 0 min 0 sec [GPSLatitudeRef missing]");
    }

    #[test]
    fn yaminabe() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let be = Reader::new().read_from_container(
            &mut BufReader::new(&file)).unwrap();
        let file = File::open("tests/yaminale.tif").unwrap();
        let le = Reader::new().read_from_container(
            &mut BufReader::new(&file)).unwrap();
        assert!(!be.little_endian());
        assert!(le.little_endian());
        for exif in &[be, le] {
            assert_eq!(exif.fields().len(), 26);
            let f = exif.get_field(Tag::ImageWidth, In(0)).unwrap();
            assert_eq!(f.display_value().to_string(), "17");
            let f = exif.get_field(Tag::Humidity, In(0)).unwrap();
            assert_eq!(f.display_value().to_string(), "65");
            let f = exif.get_field(Tag(Context::Tiff, 65000), In(0)).unwrap();
            match f.value {
                Value::Float(ref v) => assert_eq!(v[0], std::f32::MIN),
                _ => panic!(),
            }
            let f = exif.get_field(Tag(Context::Tiff, 65001), In(0)).unwrap();
            match f.value {
                Value::Double(ref v) => assert_eq!(v[0], std::f64::MIN),
                _ => panic!(),
            }
        }
    }

    #[test]
    fn heif() {
        let file = std::fs::File::open("tests/exif.heic").unwrap();
        let exif = Reader::new().read_from_container(
            &mut std::io::BufReader::new(&file)).unwrap();
        assert_eq!(exif.fields().len(), 2);
        let exifver = exif.get_field(Tag::ExifVersion, In::PRIMARY).unwrap();
        assert_eq!(exifver.display_value().to_string(), "2.31");
    }

    #[test]
    fn png() {
        let file = std::fs::File::open("tests/exif.png").unwrap();
        let exif = Reader::new().read_from_container(
            &mut std::io::BufReader::new(&file)).unwrap();
        assert_eq!(exif.fields().len(), 6);
        let exifver = exif.get_field(Tag::ExifVersion, In::PRIMARY).unwrap();
        assert_eq!(exifver.display_value().to_string(), "2.32");
    }

    #[test]
    fn webp() {
        let file = std::fs::File::open("tests/exif.webp").unwrap();
        let exif = Reader::new().read_from_container(
            &mut std::io::BufReader::new(&file)).unwrap();
        assert_eq!(exif.fields().len(), 6);
        let exifver = exif.get_field(Tag::ExifVersion, In::PRIMARY).unwrap();
        assert_eq!(exifver.display_value().to_string(), "2.32");
        let desc = exif.get_field(Tag::ImageDescription, In::PRIMARY).unwrap();
        assert_eq!(desc.display_value().to_string(), "\"WebP test\"");
    }

    #[test]
    fn continue_on_error() {
        let data = b"MM\0\x2a\0\0\0\x08\
                     \0\x02\x01\x00\0\x03\0\0\0\x01\0\x14\0\0\
                           \x01\x01\0\x03\0\0\0\x01\0\x15\0";
        let result = Reader::new()
            .continue_on_error(true)
            .read_raw(data.to_vec());
        if let Err(Error::PartialResult(partial)) = result {
            let (exif, errors) = partial.into_inner();
            assert_pat!(exif.fields().collect::<Vec<_>>().as_slice(),
                        [Field { tag: Tag::ImageWidth, ifd_num: In(0),
                                 value: Value::Short(_) }]);
            assert_pat!(&errors[..], [Error::InvalidFormat("Truncated IFD")]);
        } else {
            panic!("partial result expected");
        }
    }
}
