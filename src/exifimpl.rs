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
}

impl Exif {
    /// Constructs a new `Exif`.
    pub(crate) fn new(buf: Vec<u8>,
                      entries: Vec<IfdEntry>, little_endian: bool) -> Self {
        let entry_map = entries.iter().enumerate()
            .map(|(i, e)| (e.ifd_num_tag(), i)).collect();
        Self {
            buf: buf,
            entries: entries,
            entry_map: entry_map,
            little_endian: little_endian,
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

    /// Returns the decoded fields of the directly referenced TIFF SubIFDs.
    ///
    /// This experimental API keeps each child image in a separate vector,
    /// in the order of the SubIFDs tag (330). Within each child, `In::PRIMARY`
    /// identifies that child image, not the parent or a thumbnail. Existing
    /// [`Self::fields`] and [`Self::get_field`] results are unchanged.
    ///
    /// Nested SubIFDs and next-IFD chains are not followed. The TIFF buffer is
    /// shared for parsing, not copied for each child. A missing SubIFDs tag
    /// returns an empty vector; malformed pointers or child directories fail.
    pub fn sub_ifds(&self, ifd_num: In) -> Result<Vec<Vec<Field>>, crate::Error> {
        let tag = Tag(crate::Context::Tiff, 330);
        match self.get_field(tag, ifd_num) {
            None => Ok(Vec::new()),
            Some(field) => match &field.value {
                crate::Value::Long(offsets) =>
                    crate::tiff::parse_sub_ifds(&self.buf, self.little_endian, offsets),
                _ => Err(crate::Error::InvalidFormat("Invalid SubIFDs pointer type")),
            },
        }
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
    use crate::reader::Reader;
    use crate::value::Value;
    use super::*;

    fn subifd_fixture(typecode: u16, offsets: &[u32]) -> Vec<u8> {
        let mut data = b"MM\0\x2a\0\0\0\x08".to_vec();
        let entry = |data: &mut Vec<u8>, tag: u16, typ: u16, count: u32, value: u32| {
            data.extend_from_slice(&tag.to_be_bytes());
            data.extend_from_slice(&typ.to_be_bytes());
            data.extend_from_slice(&count.to_be_bytes());
            data.extend_from_slice(&value.to_be_bytes());
        };
        data.extend_from_slice(&3u16.to_be_bytes());
        entry(&mut data, 256, 4, 1, 100);
        entry(&mut data, 257, 4, 1, 75);
        entry(&mut data, 330, typecode, offsets.len() as u32,
              if offsets.len() == 1 { offsets[0] } else { 50 });
        data.extend_from_slice(&0u32.to_be_bytes());
        if offsets.len() != 1 {
            for offset in offsets { data.extend_from_slice(&offset.to_be_bytes()); }
        }
        for width in [4000, 2000] {
            data.extend_from_slice(&2u16.to_be_bytes());
            entry(&mut data, 256, 4, 1, width);
            entry(&mut data, 257, 4, 1, width / 2);
            data.extend_from_slice(&0u32.to_be_bytes());
        }
        data
    }

    #[test]
    fn sub_ifds_preserve_primary_fields_and_sibling_boundaries() {
        for typ in [4, 13] {
            let exif = Reader::new().read_raw(subifd_fixture(typ, &[58, 88])).unwrap();
            assert_eq!(exif.fields().len(), 3);
            assert_eq!(exif.get_field(Tag::ImageWidth, In::PRIMARY).unwrap().value.get_uint(0), Some(100));
            assert!(exif.get_field(Tag::ImageWidth, In::THUMBNAIL).is_none());
            let children = exif.sub_ifds(In::PRIMARY).unwrap();
            assert_eq!(children.len(), 2);
            for (fields, width) in children.iter().zip([4000, 2000]) {
                assert_eq!(fields.len(), 2);
                assert!(fields.iter().all(|f| f.ifd_num == In::PRIMARY));
                assert_eq!(fields.iter().find(|f| f.tag == Tag::ImageWidth).unwrap().value.get_uint(0), Some(width));
            }
            assert_eq!(exif.fields().len(), 3);
            assert!(exif.sub_ifds(In::THUMBNAIL).unwrap().is_empty());
        }
    }

    #[test]
    fn sub_ifds_reject_truncated_child_and_invalid_pointer_type() {
        let exif = Reader::new().read_raw(subifd_fixture(4, &[u32::MAX])).unwrap();
        assert!(exif.sub_ifds(In::PRIMARY).is_err());
        let exif = Reader::new().read_raw(subifd_fixture(3, &[50])).unwrap();
        assert!(exif.sub_ifds(In::PRIMARY).is_err());
    }

    #[test]
    fn sub_ifds_do_not_follow_nested_pointers_or_next_chains() {
        let exif = Reader::new().read_raw(subifd_fixture(4, &[8])).unwrap();
        let children = exif.sub_ifds(In::PRIMARY).unwrap();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].len(), 3);
        let mut data = subifd_fixture(4, &[50]);
        data[76..80].copy_from_slice(&50u32.to_be_bytes());
        let exif = Reader::new().read_raw(data).unwrap();
        assert_eq!(exif.sub_ifds(In::PRIMARY).unwrap()[0].len(), 2);
    }

    #[test]
    fn sub_ifds_little_endian_and_truncated_pointer_array() {
        let data = b"II\x2a\0\x08\0\0\0\
                     \x01\0\x4a\x01\x0d\0\x01\0\0\0\x1a\0\0\0\
                     \0\0\0\0\
                     \x01\0\0\x01\x04\0\x01\0\0\0\xa0\x0f\0\0\
                     \0\0\0\0";
        let exif = Reader::new().read_raw(data.to_vec()).unwrap();
        let children = exif.sub_ifds(In::PRIMARY).unwrap();
        assert_eq!(children[0][0].value.get_uint(0), Some(4000));
        let mut data = subifd_fixture(13, &[58, 88]);
        data[42..46].copy_from_slice(&u32::MAX.to_be_bytes());
        assert!(Reader::new().read_raw(data).is_err());
    }

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
