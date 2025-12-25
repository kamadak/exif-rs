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
use crate::subimg::EmbeddedSubImage;
#[cfg(feature = "make_note")]
use crate::make_note::maker_tag::{MakerNoteField, MakerNoteVendor, MakerTag};
#[cfg(feature = "mpf")]
use crate::mpf::mpf_tag::{MpfField, MpfTag};
use crate::value::Value;

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
    // MPF fields parsed from APP2 segment.
    // HashMap for quick access by tag number.
    #[cfg(feature = "mpf")]
    mpf_fields: HashMap<MpfTag, MpfField>,
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
            #[cfg(feature = "mpf")]
            mpf_fields: HashMap::new(),
        }
    }

    /// Constructs a new `Exif` with MPF data.
    #[cfg(feature = "mpf")]
    pub(crate) fn new_with_mpf(
        buf: Vec<u8>,
        entries: Vec<IfdEntry>,
        little_endian: bool,
        mpf_buf: Vec<u8>,
        mpf_app2_offset: u64,
    ) -> Self {
        let entry_map = entries.iter().enumerate()
            .map(|(i, e)| (e.ifd_num_tag(), i)).collect();

        // Try to parse MakerNote if present
        #[cfg(feature = "make_note")]
        let (maker_note_fields, maker_note_vendor) = Self::parse_maker_note_internal(&buf, &entries, little_endian);

        // Parse MPF fields
        let mpf_fields = Self::parse_mpf_internal(&mpf_buf, mpf_app2_offset, little_endian);

        Self {
            buf,
            entries,
            entry_map,
            little_endian,
            #[cfg(feature = "make_note")]
            maker_note_fields,
            #[cfg(feature = "make_note")]
            maker_note_vendor,
            mpf_fields,
        }
    }

    /// Internal helper to parse MPF data
    #[cfg(feature = "mpf")]
    fn parse_mpf_internal(
        mpf_buf: &[u8],
        _mpf_app2_offset: u64,
        little_endian: bool,
    ) -> HashMap<MpfTag, MpfField> {
        // Parse TIFF structure in MPF buffer
        let mut parser = crate::tiff::Parser::new();
        if parser.parse(mpf_buf).is_err() {
            return HashMap::new();
        }

        // Convert IfdEntries to MpfFields
        let mut mpf_fields = HashMap::new();
        for entry in parser.entries {
            let field = entry.into_field(mpf_buf, little_endian);
            let mpf_tag = MpfTag(field.tag.number());
            mpf_fields.insert(mpf_tag, MpfField::new(mpf_tag, field.value));
        }

        mpf_fields
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

    /// Returns a reference to the MPF field specified by the tag.
    ///
    /// Only available when the `mpf` feature is enabled.
    #[cfg(feature = "mpf")]
    #[inline]
    pub fn get_mpf_field(&self, tag: MpfTag) -> Option<&MpfField> {
        self.mpf_fields.get(&tag)
    }

    /// Returns information about embedded sub-images (thumbnails and previews).
    ///
    /// This method returns metadata for sub-images embedded in the Exif data:
    /// - IFD1 thumbnail (JPEG format, typically 10-20 KB)
    /// - MakerNote preview images (if `make_note` feature is enabled and vendor supports it)
    /// - MPF (Multi-Picture Format) images (if `mpf` feature is enabled and data exists)
    ///
    /// The returned offsets are relative to the start of the TIFF data (Exif segment),
    /// not the file start. For JPEG files, you need to account for the APP1 marker offset.
    ///
    /// # Examples
    /// ```no_run
    /// # use exif::Reader;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let file = std::fs::File::open("image.jpg")?;
    /// let exif = Reader::new().read_from_container(
    ///     &mut std::io::BufReader::new(&file))?;
    ///
    /// for img_info in exif.thumbnails() {
    ///     println!("Source: {}, Size: {} bytes, Offset: {}",
    ///              img_info.source.name(), img_info.length, img_info.offset);
    /// }
    /// # Ok(()) }
    /// ```
    pub fn thumbnails(&self) -> Vec<EmbeddedSubImage> {
        let mut images = Vec::new();

        // Try to get IFD1 thumbnail
        if let (Some(offset_field), Some(length_field)) = (
            self.get_field(Tag::JPEGInterchangeFormat, In::THUMBNAIL),
            self.get_field(Tag::JPEGInterchangeFormatLength, In::THUMBNAIL),
        ) {
            if let (Value::Long(ref offset_val), Value::Long(ref length_val)) =
                (&offset_field.value, &length_field.value)
            {
                if !offset_val.is_empty() && !length_val.is_empty() {
                    let tiff_offset = offset_val[0] as u64;
                    let length = length_val[0];
                    if length > 0 {
                        // JPEGInterchangeFormat offset is relative to TIFF structure start
                        // For JPEG files: add 12 bytes (SOI + APP1 marker + length + "Exif\0\0")
                        // For TIFF files: use offset as-is
                        //
                        // To detect JPEG vs TIFF: In TIFF files, buf contains the entire file,
                        // so thumbnail data would be within buf. In JPEG files, buf only contains
                        // the Exif segment, and thumbnail is elsewhere in the file.
                        let offset = tiff_offset + 12;
                        images.push(EmbeddedSubImage::new_thumbnail(length, offset));
                    }
                }
            }
        }

        // Try to get MakerNote preview images
        #[cfg(feature = "make_note")]
        {
            let maker_note_images = crate::subimg::extract_maker_note_preview_info(
                &self.maker_note_fields,
                &self.maker_note_vendor,
            );
            images.extend(maker_note_images);
        }

        // Try to get MPF images from MPF fields
        #[cfg(feature = "mpf")]
        {
            // MPEntry tag (0xb002) contains image metadata with absolute offsets
            if let Some(mp_entry_field) = self.get_mpf_field(MpfTag::MPEntry) {
                if let crate::value::Value::Undefined(ref data, ..) = mp_entry_field.value {
                    // Parse MPEntry data using the MPF parser
                    let entries = crate::mpf::mpf_tag::parse_mp_entry(data, self.little_endian);

                    for entry in entries.iter() {
                        if entry.image_size > 0 {
                            // Determine image classification from ImageAttr field
                            // - is_primary(): BaselinePrimary type WITHOUT representative flag (e.g., RAW/TIFF data)
                            // - is_representative(): Has representative flag (e.g., JPEG preview)
                            // - Others: MPF images (panorama, multi-frame, etc.)
                            if !entry.is_primary() && (entry.is_representative() || entry.is_thumbnail()) {
                                // Representative preview or other MPF images
                                images.push(EmbeddedSubImage::new_mpf(entry.image_size, entry.image_data_offset));
                            }
                        }
                    }
                }
            }
        }

        images
    }

    /// Returns metadata for the primary image described in the MPF (APP2) segment.
    ///
    /// This method extracts information about the primary image entry from the
    /// MPF (Multi-Picture Format) data, if present. The MPF `MPEntry` tag contains
    /// image metadata including absolute offsets and lengths for each embedded image.
    ///
    /// The returned image corresponds to the MPF entry marked as the primary image.
    /// Note that this is distinct from the Exif primary image (APP1) and represents
    /// the MPF-defined main image when multiple images are present.
    ///
    /// The returned offset is relative to the start of the JPEG file (SOI),
    /// as defined by the MPF specification.
    ///
    /// # Examples
    /// ```no_run
    /// # use exif::Reader;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let file = std::fs::File::open("image.jpg")?;
    /// let exif = Reader::new().read_from_container(
    ///     &mut std::io::BufReader::new(&file))?;
    ///
    /// if let Some(img) = exif.primary_image() {
    ///     println!(
    ///         "Primary MPF image: size={} bytes, offset={}",
    ///         img.length, img.offset
    ///     );
    /// }
    /// # Ok(()) }
    /// ```
    #[cfg(feature = "mpf")]
    pub fn primary_image(&self) -> Option<EmbeddedSubImage> {
        self.get_mpf_field(MpfTag::MPEntry)
            .and_then(|field| {
                if let crate::value::Value::Undefined(ref data, ..) = field.value {
                    crate::mpf::mpf_tag::parse_mp_entry(data, self.little_endian)
                        .iter()
                        .find(|e| e.is_primary())
                        .map(|e| {
                            EmbeddedSubImage::new_primary(
                                e.image_size,
                                e.image_data_offset,
                            )
                        })
                } else {
                    None
                }
            })
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
