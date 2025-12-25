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

//
// MPF Tag definitions and field structure
// Based on CIPA DC-007-2009 Multi-Picture Format standard
//

use std::fmt;
use crate::Value;

/// MPF tag identifier
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MpfTag(pub u16);

macro_rules! define_mpf_tags {
    (
        $(
            $( #[$attr:meta] )*
            ($name:ident, $num:expr, $desc:expr)
        ),+ $(,)?
    ) => {
        impl MpfTag {
            $(
                $( #[$attr] )*
                #[allow(non_upper_case_globals)]
                pub const $name: MpfTag = MpfTag($num);
            )+
        }

        impl fmt::Display for MpfTag {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    $(
                        MpfTag::$name => f.write_str(stringify!($name)),
                    )+
                    _ => write!(f, "Unknown(0x{:04x})", self.0),
                }
            }
        }

        impl MpfTag {
            /// Returns the tag description
            pub fn description(&self) -> &'static str {
                match *self {
                    $(
                        MpfTag::$name => $desc,
                    )+
                    _ => "Unknown MPF tag",
                }
            }
        }
    }
}

// Define all MPF tags using the macro
define_mpf_tags!(
    // Index IFD tags
    /// MPF Version (UNDEFINED, 4 bytes)
    (MPFVersion, 0xb000, "MPF version"),

    /// Number of images (LONG, 1)
    (NumberOfImages, 0xb001, "Number of images"),

    /// MPF Individual Image Entry (UNDEFINED, variable, 16 bytes per entry)
    (MPEntry, 0xb002, "MP entry"),

    /// Unique ID list (UNDEFINED, variable)
    (ImageUIDList, 0xb003, "Image UID list"),

    /// Total number of captured frames (LONG, 1)
    (TotalFrames, 0xb004, "Total frames"),

    // Attribute IFD tags
    /// MPF Individual Image Format (LONG, 1)
    (MPFVersion2, 0xb200, "MPF version (attribute)"),

    /// MPF Individual Image Number (LONG, 1)
    (MPIndividualNum, 0xb201, "MP individual number"),

    /// Panorama Scanning Orientation (LONG, 1)
    (PanOrientation, 0xb202, "Panorama orientation"),

    /// Panorama Horizontal Overlap (RATIONAL, 1)
    (PanOverlapH, 0xb203, "Panorama horizontal overlap"),

    /// Panorama Vertical Overlap (RATIONAL, 1)
    (PanOverlapV, 0xb204, "Panorama vertical overlap"),

    /// Base Viewpoint Number (LONG, 1)
    (BaseViewpointNum, 0xb205, "Base viewpoint number"),

    /// Convergence Angle (SRATIONAL, 1)
    (ConvergenceAngle, 0xb206, "Convergence angle"),

    /// Baseline Length (RATIONAL, 1)
    (BaselineLength, 0xb207, "Baseline length"),

    /// Vertical Divergence (SRATIONAL, 1)
    (VerticalDivergence, 0xb208, "Vertical divergence"),

    /// Axis Distance X (SRATIONAL, 1)
    (AxisDistanceX, 0xb209, "Axis distance X"),

    /// Axis Distance Y (SRATIONAL, 1)
    (AxisDistanceY, 0xb20a, "Axis distance Y"),

    /// Axis Distance Z (SRATIONAL, 1)
    (AxisDistanceZ, 0xb20b, "Axis distance Z"),

    /// Yaw Angle (SRATIONAL, 1)
    (YawAngle, 0xb20c, "Yaw angle"),

    /// Pitch Angle (SRATIONAL, 1)
    (PitchAngle, 0xb20d, "Pitch angle"),

    /// Roll Angle (SRATIONAL, 1)
    (RollAngle, 0xb20e, "Roll angle"),
);

/// MPF field containing tag and value
#[derive(Debug, Clone)]
pub struct MpfField {
    pub tag: MpfTag,
    pub value: Value,
}

impl MpfField {
    pub fn new(tag: MpfTag, value: Value) -> Self {
        Self { tag, value }
    }
}

impl fmt::Display for MpfField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.tag, self.value)
    }
}

/// Parse MPEntry data to extract individual image information
pub fn parse_mp_entry(data: &[u8], little_endian: bool) -> Vec<MpImageEntry> {
    use crate::endian::{Endian, BigEndian, LittleEndian};

    let mut entries = Vec::new();
    let entry_size = 16;

    for i in (0..data.len()).step_by(entry_size) {
        if i + entry_size > data.len() {
            break;
        }

        let entry_data = &data[i..i + entry_size];

        let (attr, size, offset, dep1, dep2) = if little_endian {
            (
                LittleEndian::loadu32(entry_data, 0),
                LittleEndian::loadu32(entry_data, 4),
                LittleEndian::loadu32(entry_data, 8),
                LittleEndian::loadu16(entry_data, 12),
                LittleEndian::loadu16(entry_data, 14),
            )
        } else {
            (
                BigEndian::loadu32(entry_data, 0),
                BigEndian::loadu32(entry_data, 4),
                BigEndian::loadu32(entry_data, 8),
                BigEndian::loadu16(entry_data, 12),
                BigEndian::loadu16(entry_data, 14),
            )
        };

        entries.push(MpImageEntry {
            image_attr: attr,
            image_size: size,
            image_data_offset: offset as u64,
            _dep_entry1: dep1,
            _dep_entry2: dep2,
        });
    }

    entries
}

/// MPF Individual Image Type
/// Extracted from bits 24-27 of ImageAttr field
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum MpImageType {
    /// Baseline MP Primary Image (main/original image)
    BaselinePrimary = 0x00,
    /// Large Thumbnail VGA
    LargeThumbnailVGA = 0x01,
    /// Large Thumbnail Full HD
    LargeThumbnailFullHD = 0x02,
    /// Multi-Frame Image (Panorama/Disparity/Multi-Angle)
    MultiFrame = 0x03,
    /// Unknown or unsupported type
    Unknown,
}

impl MpImageType {
    /// Parse image type from ImageAttr value
    pub fn from_attr(attr: u32) -> Self {
        match (attr >> 24) & 0x0f {
            0x00 => MpImageType::BaselinePrimary,
            0x01 => MpImageType::LargeThumbnailVGA,
            0x02 => MpImageType::LargeThumbnailFullHD,
            0x03 => MpImageType::MultiFrame,
            _ => MpImageType::Unknown,
        }
    }

    /// Returns true if this is a primary/representative image
    pub fn is_primary(&self) -> bool {
        matches!(self, MpImageType::BaselinePrimary)
    }

    /// Returns true if this is a thumbnail image
    pub fn is_thumbnail(&self) -> bool {
        matches!(self, MpImageType::LargeThumbnailVGA | MpImageType::LargeThumbnailFullHD)
    }
}

/// MPF Image Attribute Flags
/// Extracted from ImageAttr field (CIPA DC-007-2009)
#[derive(Debug, Clone, Copy)]
pub struct MpImageAttr {
    pub image_type: MpImageType,
    pub is_representative: bool,
    pub _is_dependent_parent: bool,
    pub _is_dependent_child: bool,
}

impl MpImageAttr {
    /// Parse ImageAttr field value
    pub fn from_attr(attr: u32) -> Self {
        Self {
            image_type: MpImageType::from_attr(attr),
            is_representative: (attr & (1 << 30)) != 0,  // Bit 30
            _is_dependent_parent: (attr & (1 << 29)) != 0, // Bit 29
            _is_dependent_child: (attr & (1 << 28)) != 0,  // Bit 28
        }
    }
}

/// MPF image entry
#[derive(Debug, Clone)]
pub struct MpImageEntry {
    image_attr: u32,
    pub image_size: u32,
    pub image_data_offset: u64,
    _dep_entry1: u16,
    _dep_entry2: u16,
}

impl MpImageEntry {
    /// Returns the parsed image attributes
    pub fn attributes(&self) -> MpImageAttr {
        MpImageAttr::from_attr(self.image_attr)
    }

    /// Returns the image type
    #[allow(dead_code)]
    pub fn image_type(&self) -> MpImageType {
        self.attributes().image_type
    }

    /// Returns true if this is a primary image (NOT representative)
    /// In MPF, the first entry with BaselinePrimary type and NO representative flag
    /// is the original primary image (e.g., RAW data embedded as TIFF)
    pub fn is_primary(&self) -> bool {
        let attr = self.attributes();
        attr.image_type.is_primary() && !attr.is_representative
    }

    /// Returns true if this is a representative image (preview/thumbnail for display)
    /// This is typically a JPEG preview of the primary image
    pub fn is_representative(&self) -> bool {
        self.attributes().is_representative
    }

    /// Returns true if this is a thumbnail image
    pub fn is_thumbnail(&self) -> bool {
        self.attributes().image_type.is_thumbnail()
    }

    /// Returns the raw image attributes
    #[allow(dead_code)]
    pub fn image_attr(&self) -> u32 {
        self.image_attr
    }
}
