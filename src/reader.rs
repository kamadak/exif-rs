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

use crate::error::Error;
use crate::isobmff;
use crate::jpeg;
use crate::tag::Tag;
use crate::tiff;
use crate::tiff::{Field, IfdEntry, In, ProvideUnit};

/// The `Reader` struct reads a JPEG or TIFF image,
/// parses the Exif attributes in it, and holds the results.
///
/// # Examples
/// ```
/// use exif::{In, Reader, Tag};
/// let file = std::fs::File::open("tests/exif.jpg").unwrap();
/// let reader = Reader::new(&mut std::io::BufReader::new(&file)).unwrap();
/// let xres = reader.get_field(Tag::XResolution, In::PRIMARY).unwrap();
/// assert_eq!(xres.display_value().with_unit(&reader).to_string(),
///            "72 pixels per inch");
/// ```
pub struct Reader {
    // TIFF data.
    buf: Vec<u8>,
    // Exif fields.  Vec is used to keep the ability to enumerate all fields
    // even if there are duplicates.
    entries: Vec<IfdEntry>,
    // HashMap to the index of the Vec for faster random access.
    entry_map: HashMap<(In, Tag), usize>,
    // True if the TIFF data is little endian.
    little_endian: bool,
}

impl Reader {
    /// Reads a JPEG or TIFF image and parses the Exif attributes in it.
    /// If an error occurred, `exif::Error` is returned.
    pub fn new<R>(reader: &mut R)
                  -> Result<Reader, Error> where R: io::BufRead {
        // Parse the data.
        let mut buf = Vec::new();
        reader.by_ref().take(4).read_to_end(&mut buf)?;
        if jpeg::is_jpeg(&buf) {
            let exif_buf = jpeg::get_exif_attr(
                &mut buf.as_mut_slice().chain(reader))?;
            buf = exif_buf;
        } else if tiff::is_tiff(&buf) {
            reader.read_to_end(&mut buf)?;
        } else {
            return Err(Error::InvalidFormat("Unknown image format"));
        }

        let (entries, le) = tiff::parse_exif(&buf)?;
        let entry_map = entries.iter().enumerate()
            .map(|(i, e)| (e.ifd_num_tag(), i)).collect();

        Ok(Reader {
            buf: buf,
            entries: entries,
            entry_map: entry_map,
            little_endian: le,
        })
    }

    /// Reads an image file and parses the Exif attributes in it.
    /// If an error occurred, `exif::Error` is returned.
    ///
    /// Supported formats are HEIF, JPEG, and TIFF.
    ///
    /// This method is provided for the convenience even though
    /// parsing containers is basically out of the scope of this library.
    pub fn read_from_container<R>(reader: &mut R) -> Result<Reader, Error>
    where R: io::BufRead + io::Seek {
        let mut buf = Vec::new();
        reader.by_ref().take(4096).read_to_end(&mut buf)?;
        if tiff::is_tiff(&buf) {
            reader.read_to_end(&mut buf)?;
        } else if jpeg::is_jpeg(&buf) {
            buf = jpeg::get_exif_attr(&mut buf.chain(reader))?;
        } else if isobmff::is_heif(&buf) {
            reader.seek(io::SeekFrom::Start(0))?;
            buf = isobmff::get_exif_attr(reader)?;
        } else {
            return Err(Error::InvalidFormat("Unknown image format"));
        }

        let (entries, le) = tiff::parse_exif(&buf)?;
        let entry_map = entries.iter().enumerate()
            .map(|(i, e)| (e.ifd_num_tag(), i)).collect();

        Ok(Reader {
            buf: buf,
            entries: entries,
            entry_map: entry_map,
            little_endian: le,
        })
    }

    /// Returns the slice that contains the TIFF data.
    #[inline]
    pub fn buf(&self) -> &[u8] {
        &self.buf[..]
    }

    /// Returns a slice of Exif fields.
    #[inline]
    pub fn fields(&self) -> impl ExactSizeIterator<Item = &Field> {
        self.entries.iter()
            .map(move |e| e.ref_field(&self.buf, self.little_endian))
    }

    /// Returns true if the TIFF data is in the little-endian byte order.
    #[inline]
    pub fn little_endian(&self) -> bool {
        self.little_endian
    }

    /// Returns a reference to the Exif field specified by the tag
    /// and the IFD number.
    #[inline]
    pub fn get_field(&self, tag: Tag, ifd_num: In) -> Option<&Field> {
        self.entry_map.get(&(ifd_num, tag))
            .map(|&i| self.entries[i].ref_field(&self.buf, self.little_endian))
    }
}

impl<'a> ProvideUnit<'a> for &'a Reader {
    fn get_field(self, tag: Tag, ifd_num: In) -> Option<&'a Field> {
        self.get_field(tag, ifd_num)
    }
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::BufReader;
    use crate::value::Value;
    use super::*;

    #[test]
    fn get_field() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let reader = Reader::new(&mut BufReader::new(&file)).unwrap();
        match reader.get_field(Tag::ImageDescription, In(0)).unwrap().value {
            Value::Ascii(ref vec) => assert_eq!(vec, &[b"Test image"]),
            ref v => panic!("wrong variant {:?}", v)
        }
        match reader.get_field(Tag::ImageDescription, In(1)).unwrap().value {
            Value::Ascii(ref vec) => assert_eq!(vec, &[b"Test thumbnail"]),
            ref v => panic!("wrong variant {:?}", v)
        }
        match reader.get_field(Tag::ImageDescription, In(2)).unwrap().value {
            Value::Ascii(ref vec) => assert_eq!(vec, &[b"Test 2nd IFD"]),
            ref v => panic!("wrong variant {:?}", v)
        }
    }

    #[test]
    fn display_value_with_unit() {
        let file = File::open("tests/yaminabe.tif").unwrap();
        let reader = Reader::new(&mut BufReader::new(&file)).unwrap();
        // No unit.
        let exifver = reader.get_field(Tag::ExifVersion, In::PRIMARY).unwrap();
        assert_eq!(exifver.display_value().with_unit(&reader).to_string(),
                   "2.31");
        // Fixed string.
        let width = reader.get_field(Tag::ImageWidth, In::PRIMARY).unwrap();
        assert_eq!(width.display_value().with_unit(&reader).to_string(),
                   "17 pixels");
        // Unit tag (with a non-default value).
        let gpsalt = reader.get_field(Tag::GPSAltitude, In::PRIMARY).unwrap();
        assert_eq!(gpsalt.display_value().with_unit(&reader).to_string(),
                   "0.5 meters below sea level");
        // Unit tag is missing but the default is specified.
        let xres = reader.get_field(Tag::XResolution, In::PRIMARY).unwrap();
        assert_eq!(xres.display_value().with_unit(&reader).to_string(),
                   "72 pixels per inch");
        // Unit tag is missing and the default is not specified.
        let gpslat = reader.get_field(Tag::GPSLatitude, In::PRIMARY).unwrap();
        assert_eq!(gpslat.display_value().with_unit(&reader).to_string(),
                   "10 deg 0 min 0 sec [GPSLatitudeRef missing]");
    }

    #[test]
    fn heif() {
        let file = std::fs::File::open("tests/exif.heic").unwrap();
        let reader = Reader::read_from_container(
            &mut std::io::BufReader::new(&file)).unwrap();
        assert_eq!(reader.fields().len(), 2);
        let exifver = reader.get_field(Tag::ExifVersion, In::PRIMARY).unwrap();
        assert_eq!(exifver.display_value().to_string(), "2.31");
    }
}
