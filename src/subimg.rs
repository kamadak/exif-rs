//
// Copyright (c) 2025 Jinwoo Park (pmnxis@gmail.com).
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

use std::io::{BufRead, Seek};

use crate::error::Error;
#[cfg(feature = "make_note")]
use crate::make_note::maker_tag::{MakerNoteVendor, MakerTag};
#[cfg(feature = "make_note")]
use crate::value::Value;

/// Source of an embedded sub-image (thumbnail or preview)
///
/// This enum identifies where a sub-image was extracted from within the file.
/// Explicit discriminant values ensure compatibility across versions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum EmbeddedSubImageSource {
    /// TIFF original primary image
    Primary = 0,

    /// IFD1 thumbnail image (always available)
    Thumbnail = 1,

    /// MPF (Multi-Picture Format) embedded image
    #[cfg(feature = "mpf")]
    Mpf = 2,

    /// MakerNote preview image 1 (Olympus: PreviewImageStart 0x0088 / PreviewImageLength 0x0089)
    #[cfg(feature = "make_note")]
    MakerNotePreview1 = 3,

    /// MakerNote preview image 2 (Olympus: PreviewImageStart2 0x1036 / PreviewImageLength2 0x1037)
    #[cfg(feature = "make_note")]
    MakerNotePreview2 = 4,

    /// MakerNote preview image 3 (Olympus: PreviewImageStart3 CameraSettings 0x0101 / PreviewImageLength3 0x0102)
    #[cfg(feature = "make_note")]
    MakerNotePreview3 = 5,
}

impl EmbeddedSubImageSource {
    /// Get a human-readable name for the image source
    pub fn name(&self) -> &'static str {
        match self {
            EmbeddedSubImageSource::Primary => "Primary",
            EmbeddedSubImageSource::Thumbnail => "Thumbnail",
            #[cfg(feature = "mpf")]
            EmbeddedSubImageSource::Mpf => "MPF",
            #[cfg(feature = "make_note")]
            EmbeddedSubImageSource::MakerNotePreview1 => "MakerNotePreview1",
            #[cfg(feature = "make_note")]
            EmbeddedSubImageSource::MakerNotePreview2 => "MakerNotePreview2",
            #[cfg(feature = "make_note")]
            EmbeddedSubImageSource::MakerNotePreview3 => "MakerNotePreview3",
        }
    }
}

/// Information about an embedded sub-image
///
/// This structure provides unified metadata for sub-images from various sources
/// (IFD1 thumbnail, MPF, or MakerNote). The actual image data can be
/// extracted on demand using `extract_data()`.
#[derive(Debug, Clone, Copy)]
pub struct EmbeddedSubImage {
    /// Source of this sub-image
    pub source: EmbeddedSubImageSource,

    /// Size of the image in bytes
    pub length: u32,

    /// Absolute offset from the start of the file
    pub offset: u64,
}

impl EmbeddedSubImage {
    /// Create a new EmbeddedSubImage for a primary image
    pub fn new_primary(length: u32, offset: u64) -> Self {
        Self {
            source: EmbeddedSubImageSource::Primary,
            length,
            offset,
        }
    }

    /// Create a new EmbeddedSubImage for a thumbnail
    pub fn new_thumbnail(length: u32, offset: u64) -> Self {
        Self {
            source: EmbeddedSubImageSource::Thumbnail,
            length,
            offset,
        }
    }

    /// Create a new EmbeddedSubImage for an MPF image
    #[cfg(feature = "mpf")]
    pub fn new_mpf(length: u32, offset: u64) -> Self {
        Self {
            source: EmbeddedSubImageSource::Mpf,
            length,
            offset,
        }
    }

    /// Create a new EmbeddedSubImage for a MakerNote preview image
    #[cfg(feature = "make_note")]
    pub fn new_maker_note_preview(
        source: EmbeddedSubImageSource,
        length: u32,
        offset: u64,
    ) -> Self {
        Self {
            source,
            length,
            offset,
        }
    }

    /// Extract the image data from a reader
    ///
    /// # Arguments
    /// * `reader` - A reader positioned at the start of the file
    ///
    /// # Returns
    /// The raw image data as a Vec<u8>
    pub fn extract_data<R>(&self, reader: &mut R) -> Result<Vec<u8>, Error>
    where
        R: BufRead + Seek,
    {
        use std::io::SeekFrom;

        // Seek to the image start position
        reader.seek(SeekFrom::Start(self.offset))?;

        // Read the image data
        let mut data = vec![0u8; self.length as usize];
        reader.read_exact(&mut data)?;

        Ok(data)
    }

    /// Extract and save the image data to a file
    ///
    /// # Arguments
    /// * `reader` - A reader positioned at the start of the file
    /// * `path` - The file path to save the image data
    ///
    /// # Returns
    /// The number of bytes written
    ///
    /// # Examples
    /// ```no_run
    /// # use exif::Reader;
    /// # use std::fs::File;
    /// # use std::io::BufReader;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let file = File::open("image.jpg")?;
    /// let mut reader = BufReader::new(&file);
    /// let exif = Reader::new().read_from_container(&mut reader)?;
    ///
    /// for img in exif.thumbnails() {
    ///     let mut file = File::open("image.jpg")?;
    ///     let mut reader = BufReader::new(&file);
    ///     img.save_to_file(&mut reader, format!("thumbnail_{}.jpg", img.source.name()))?;
    /// }
    /// # Ok(()) }
    /// ```
    pub fn save_to_file<R, P>(&self, reader: &mut R, path: P) -> Result<usize, Error>
    where
        R: BufRead + Seek,
        P: AsRef<std::path::Path>,
    {
        let data = self.extract_data(reader)?;
        std::fs::write(path, &data)?;
        Ok(data.len())
    }
}

/// Helper function to extract preview image from MakerNote tag pairs
#[cfg(feature = "make_note")]
fn try_extract_preview(
    maker_note_fields: &std::collections::HashMap<
        MakerTag,
        crate::make_note::maker_tag::MakerNoteField,
    >,
    start_tag: &MakerTag,
    length_tag: &MakerTag,
    source: EmbeddedSubImageSource,
) -> Option<EmbeddedSubImage> {
    let start_field = maker_note_fields.get(start_tag)?;
    let length_field = maker_note_fields.get(length_tag)?;

    if let (Value::Long(ref start_val), Value::Long(ref length_val)) =
        (&start_field.value, &length_field.value)
    {
        if !start_val.is_empty() && !length_val.is_empty() {
            let offset = start_val[0] as u64;
            let length = length_val[0];
            if length > 0 {
                return Some(EmbeddedSubImage::new_maker_note_preview(
                    source, length, offset,
                ));
            }
        }
    }
    None
}

/// Extract preview image info from MakerNote data
///
/// Currently supports:
/// - Olympus: PreviewImageStart/PreviewImageLength tags (3 variants)
#[cfg(feature = "make_note")]
pub(crate) fn extract_maker_note_preview_info(
    maker_note_fields: &std::collections::HashMap<
        MakerTag,
        crate::make_note::maker_tag::MakerNoteField,
    >,
    maker_note_vendor: &Result<MakerNoteVendor, Error>,
) -> Vec<EmbeddedSubImage> {
    let mut images = Vec::new();
    let vendor = match maker_note_vendor.as_ref() {
        Ok(v) => v,
        Err(_) => return images,
    };

    match vendor {
        MakerNoteVendor::Olympus => {
            use crate::make_note::olympus;

            // Olympus has multiple preview image locations:
            // 1. PreviewImageStart (0x0088) + PreviewImageLength (0x0089)
            // 2. PreviewImageStart2 (0x1036) + PreviewImageLength2 (0x1037)
            // 3. PreviewImageStart3 (CameraSettings 0x0101) + PreviewImageLength3 (CameraSettings 0x0102)

            // Try PreviewImageStart/Length
            if let Some(img) = try_extract_preview(
                maker_note_fields,
                &olympus::tags::PreviewImageStart,
                &olympus::tags::PreviewImageLength,
                EmbeddedSubImageSource::MakerNotePreview1,
            ) {
                images.push(img);
            }

            // Try PreviewImageStart2/Length2
            if let Some(img) = try_extract_preview(
                maker_note_fields,
                &olympus::tags::PreviewImageStart2,
                &olympus::tags::PreviewImageLength2,
                EmbeddedSubImageSource::MakerNotePreview2,
            ) {
                images.push(img);
            }

            // TODO: PreviewImageStart3/Length3 from CameraSettings subdirectory (0x2020)
            // This requires parsing the CameraSettings IFD structure
        }
        _ => {}
    }

    images
}
