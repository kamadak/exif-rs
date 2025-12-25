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

use crate::tag::Tag;
use crate::tiff::{Field, IfdEntry, In, ProvideUnit};
#[cfg(feature = "make_note")]
use crate::make_note::maker_tag::{MakerNoteField, MakerNoteVendor, MakerTag};

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
    // MakerNote fields parsed by vendor-specific parser.
    // HashMap for quick access by (vendor, tag_number).
    #[cfg(feature = "make_note")]
    maker_note_fields: HashMap<MakerTag, MakerNoteField>,
    // MakerNote vendor detected from the data, or error if not found.
    #[cfg(feature = "make_note")]
    maker_note_vendor: Result<MakerNoteVendor, crate::Error>,
}

impl Exif {
    /// Constructs a new `Exif`.
    pub(crate) fn new(buf: Vec<u8>,
                      entries: Vec<IfdEntry>, little_endian: bool) -> Self {
        let entry_map = entries.iter().enumerate()
            .map(|(i, e)| (e.ifd_num_tag(), i)).collect();

        // Try to parse MakerNote if present
        #[cfg(feature = "make_note")]
        let (maker_note_fields, maker_note_vendor) = Self::parse_maker_note_internal(&buf, &entries, little_endian);

        Self {
            buf: buf,
            entries: entries,
            entry_map: entry_map,
            little_endian: little_endian,
            #[cfg(feature = "make_note")]
            maker_note_fields,
            #[cfg(feature = "make_note")]
            maker_note_vendor,
        }
    }

    /// Internal helper to parse MakerNote data
    #[cfg(feature = "make_note")]
    fn parse_maker_note_internal(
        buf: &[u8],
        entries: &[IfdEntry],
        little_endian: bool,
    ) -> (HashMap<MakerTag, MakerNoteField>, Result<MakerNoteVendor, crate::Error>) {
        // Find MakerNote field
        let maker_note_entry = entries.iter()
            .find(|e| e.ifd_num_tag().1 == Tag::MakerNote);

        let Some(maker_note_entry) = maker_note_entry else {
            return (HashMap::new(), Err(crate::Error::MakerNoteNotFound));
        };

        // Get MakerNote field value
        let field = maker_note_entry.ref_field(buf, little_endian);
        let crate::value::Value::Undefined(ref data, offset) = field.value else {
            return (HashMap::new(), Err(crate::Error::MakerNoteNotFound));
        };

        // Get Make field for vendor detection
        let make = entries.iter()
            .find(|e| e.ifd_num_tag().1 == Tag::Make)
            .and_then(|e| {
                let field = e.ref_field(buf, little_endian);
                if let crate::value::Value::Ascii(ref vec) = field.value {
                    vec.first()
                        .and_then(|s| std::str::from_utf8(s).ok())
                } else {
                    None
                }
            });

        // Parse MakerNote with vendor detection
        #[cfg(feature = "make_note")]
        {
            match crate::make_note::parse_make_note_with_vendor(data, offset, make) {
                Ok((fields, vendor, _le)) => {
                    let map = fields.into_iter()
                        .map(|f| (f.tag, f))
                        .collect();
                    (map, Ok(vendor))
                }
                Err(e) => (HashMap::new(), Err(e))
            }
        }
        #[cfg(not(feature = "make_note"))]
        {
            (HashMap::new(), Err(crate::Error::NotFound("MakerNote parsing disabled")))
        }
    }

    /// Returns the slice that contains the TIFF data.
    #[inline]
    pub fn buf(&self) -> &[u8] {
        &self.buf[..]
    }

    /// Returns an iterator of Exif fields.
    #[inline]
    pub fn fields(&self) -> impl ExactSizeIterator<Item = &Field> {
        self.entries.iter()
            .map(move |e| e.ref_field(&self.buf, self.little_endian))
    }

    /// Returns true if the Exif data (TIFF structure) is in the
    /// little-endian byte order.
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

    /// Returns the detected MakerNote vendor.
    ///
    /// Returns `Ok(vendor)` if a MakerNote was found and the vendor was detected,
    /// or `Err(Error::MakerNoteNotFound)` if no MakerNote field exists,
    /// or another error if parsing failed.
    #[inline]
    #[cfg(feature = "make_note")]
    pub fn maker_note_vendor(&self) -> Result<MakerNoteVendor, &crate::Error> {
        self.maker_note_vendor.as_ref().copied()
    }

    /// Returns a reference to a MakerNote field by tag.
    ///
    /// # Arguments
    /// * `tag` - The MakerTag identifying the vendor-specific field
    ///
    /// # Examples
    /// ```
    /// # use exif::{Reader, make_note::maker_tag::{MakerTag, MakerNoteVendor}};
    /// # let file = std::fs::File::open("tests/exif.jpg").unwrap();
    /// # let exif = Reader::new().read_from_container(
    /// #     &mut std::io::BufReader::new(&file)).unwrap();
    /// // Get a specific MakerNote field
    /// let vendor = exif.maker_note_vendor().ok().copied()?;
    /// let tag = MakerTag::new(vendor, 0x0002);
    /// let field = exif.get_maker_note_field(&tag)?;
    /// println!("{}: {}", field.tag, field.display_value());
    /// # Some(()) }
    /// ```
    #[inline]
    #[cfg(feature = "make_note")]
    pub fn get_maker_note_field(&self, tag: &MakerTag) -> Option<&MakerNoteField> {
        self.maker_note_fields.get(tag)
    }

    /// Returns an iterator over all parsed MakerNote fields.
    ///
    /// # Examples
    /// ```
    /// # fn main() { sub(); }
    /// # fn sub() -> Option<()> {
    /// # use exif::Reader;
    /// # let file = std::fs::File::open("tests/exif.jpg").unwrap();
    /// # let exif = Reader::new().read_from_container(
    /// #     &mut std::io::BufReader::new(&file)).unwrap();
    /// // Iterate over all MakerNote fields
    /// for field in exif.maker_note_fields() {
    ///     println!("{}: {}", field.tag, field.display_value());
    /// }
    /// # Some(()) }
    /// ```
    #[inline]
    #[cfg(feature = "make_note")]
    pub fn maker_note_fields(&self) -> impl Iterator<Item = &MakerNoteField> {
        self.maker_note_fields.values()
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
    use crate::reader::Reader;
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
}
