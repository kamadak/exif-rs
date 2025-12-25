//
// MakerNote Tag definitions
// Based on https://exiv2.org/makernote.html
//

use std::fmt;

use crate::Value;

/// Trait for structured MakerNote data parsing
///
/// This trait provides a unified interface for parsing vendor-specific
/// binary structures in MakerNote tags. Implementations can use techniques
/// like `#[repr(C)]` zero-copy transmute or manual parsing.
pub trait StructuredMakerNoteData: Sized + fmt::Display {
    /// Parse from raw bytes with optional endianness hint
    ///
    /// # Arguments
    /// * `data` - Raw binary data to parse
    /// * `le` - Optional endianness: Some(true) for little-endian, Some(false) for big-endian, None for auto-detect
    fn raw_parse(data: &[u8], le: Option<bool>) -> Option<Self>;

    /// Extract from Value type with optional endianness hint
    ///
    /// # Arguments
    /// * `value` - EXIF value containing the data
    /// * `le` - Optional endianness: Some(true) for little-endian, Some(false) for big-endian, None for auto-detect
    fn from_value(value: &Value, le: Option<bool>) -> Option<Self> {
        match value {
            Value::Undefined(data, _) => Self::raw_parse(data, le),
            Value::Byte(data) => Self::raw_parse(data, le),
            _ => None,
        }
    }

    /// Convert to display string for tag display function
    fn from_value_to_string(value: &Value, le: Option<bool>) -> String {
        Self::from_value(value, le)
            .map(|v| v.to_string())
            .unwrap_or_else(|| String::from("<invalid>"))
    }
}

/// Camera manufacturer/vendor identifier for MakerNote data.
///
/// Reference: https://exiv2.org/makernote.html
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, strum::Display)]
#[non_exhaustive]
pub enum MakerNoteVendor {
    /// Panasonic / Lumix cameras
    /// Header: "Panasonic\0\0\0" (12 bytes)
    /// IFD offset: 12 (relative to MakerNote field start)
    Panasonic,

    /// Nikon cameras (Type 3: E5400/D70/D200)
    /// Header: "Nikon\0" + version + TIFF header (18 bytes total)
    /// IFD offset: relative to TIFF header start
    Nikon,

    /// Canon cameras
    Canon,

    /// Sony cameras
    Sony,

    /// Olympus cameras
    Olympus,

    /// OM System cameras
    OMSystem,

    /// Fujifilm cameras
    /// Uses relative offsets
    Fujifilm,

    /// Leica cameras
    /// Header: "LEICA\0" + 2-byte version (8 bytes total)
    Leica,

    /// Samsung cameras (NX series)
    /// No proprietary header, starts directly with IFD
    Samsung,

    /// Apple cameras (iPhone, iPad)
    /// No proprietary header, starts directly with IFD
    Apple,

    /// Sigma cameras
    /// Header: "SIGMA\0\0\0" or "FOVEON\0\0" (8 bytes)
    Sigma,

    /// Ricoh cameras some 
    /// Header: "RIOCH\0" or no header, starts directly with IFD
    Ricoh,

    // todo - AocPentax with starting with "AOC\0"

    /// Pentax/Ricoh cameras
    /// Header: Older model "AOC\0", starts directly with IFD
    Pentax,

    /// Olympus Equipment subdirectory (0x2010)
    OlympusEquipment,

    /// Olympus CameraSettings subdirectory (0x2020)
    OlympusCameraSettings,

    /// Olympus RawDevelopment subdirectory (0x2030)
    OlympusRawDevelopment,

    /// Olympus ImageProcessing subdirectory (0x2040)
    OlympusImageProcessing,

    /// Olympus FocusInfo subdirectory (0x2050)
    OlympusFocusInfo,

    /// Olympus RawInfo subdirectory (0x3000)
    OlympusRawInfo,

    /// Unknown or unsupported vendor
    Unknown,
}

impl MakerNoteVendor {
    /// Detect vendor from MakerNote header data and optional Make field.
    ///
    /// Based on https://exiv2.org/makernote.html header signatures.
    ///
    /// # Arguments
    /// * `data` - The MakerNote data
    /// * `make` - Optional Make field from EXIF (e.g., "Canon", "SONY")
    pub fn from_header(data: &[u8], make: Option<&str>) -> Self {
        // Panasonic: "Panasonic\0\0\0"
        if data.starts_with(b"Panasonic") {
            MakerNoteVendor::Panasonic
        }
        // Nikon: "Nikon\0" + version bytes
        else if data.starts_with(b"Nikon\x00") {
            MakerNoteVendor::Nikon
        }
        // Sony: "SONY DSC "
        else if data.starts_with(b"SONY") {
            MakerNoteVendor::Sony
        }
        // Olympus: "OLYMPUS\0" or "OLYMP\0"
        else if data.starts_with(b"OLYMPUS") || data.starts_with(b"OLYMP\x00") {
            MakerNoteVendor::Olympus
        }
        // OM System: "OM SYSTEM\0\0\0"
        else if data.starts_with(b"OM SYSTEM") {
            MakerNoteVendor::OMSystem
        }
        // Fujifilm
        else if data.starts_with(b"FUJIFILM") {
            MakerNoteVendor::Fujifilm
        }
        // Leica: "LEICA\0"
        else if data.starts_with(b"LEICA\x00") {
            MakerNoteVendor::Leica
        }
        // Apple: "Apple iOS\0"
        else if data.starts_with(b"Apple iOS") {
            MakerNoteVendor::Apple
        }
        // Sigma: "SIGMA\0\0\0" or "FOVEON\0\0"
        else if data.starts_with(b"SIGMA\x00") || data.starts_with(b"FOVEON\x00") {
            MakerNoteVendor::Sigma
        }
        // Ricoh: "RICOH\0" header
        else if data.starts_with(b"RICOH\x00") {
            MakerNoteVendor::Ricoh
        }
        // Pentax: "PENTAX \0" header
        else if data.starts_with(b"PENTAX \x00") {
            MakerNoteVendor::Pentax
        }
        // todo - AOC style ricoh and pentax
        // else if data.starts_with(b"AOC\x00")
        //
        // Canon/Samsung/Pentax/Ricoh: No header, detect from Make field
        else if let Some(make_str) = make {
            if make_str.starts_with("Canon") {
                MakerNoteVendor::Canon
            } else if make_str.starts_with("SAMSUNG") {
                MakerNoteVendor::Samsung
            } else if make_str.starts_with("PENTAX") || make_str.starts_with("RICOH") {
                MakerNoteVendor::Pentax
            } else {
                MakerNoteVendor::Unknown
            }
        } else {
            MakerNoteVendor::Unknown
        }
    }


    /// Returns the proprietary header size (bytes to skip before IFD).
    ///
    /// Based on https://exiv2.org/makernote.html structure definitions.
    pub const fn header_size(&self) -> usize {
        match self {
            MakerNoteVendor::Panasonic => 12, // "Panasonic\0\0\0"
            MakerNoteVendor::Nikon => 10,     // "Nikon\0" + 2 version bytes + 2 padding
            MakerNoteVendor::Canon => 0,      // No header
            MakerNoteVendor::Sony => 12,      // "SONY DSC \0\0\0"
            MakerNoteVendor::Olympus => 12,   // "OLYMPUS\0" + "II/MM" + version (2 bytes)
            MakerNoteVendor::OMSystem => 16,  // "OM SYSTEM\0\0\0" + "II/MM" + version (2 bytes)
            MakerNoteVendor::Fujifilm => 12,  // "FUJIFILM" + 4 bytes
            MakerNoteVendor::Leica => 8,      // "LEICA\0" + version (2 bytes)
            MakerNoteVendor::Samsung => 0,    // No header, starts directly with IFD
            MakerNoteVendor::Apple => 14,     // "Apple iOS\0" (10) + version (2) + "II/MM" (2) = 14 bytes
            MakerNoteVendor::Sigma => 10,     // "SIGMA\0\0\0" or "FOVEON\0\0" (8) + version (2) = 10 bytes
            MakerNoteVendor::Ricoh => 8,      // "RICOH\0II"  (8 ) + "II/MM"
            MakerNoteVendor::Pentax => 10,    // "PENTAX \0II" (10) + "II/MM"
            // Subdirectories don't have headers (they're already inside parsed data)
            MakerNoteVendor::OlympusEquipment
            | MakerNoteVendor::OlympusCameraSettings
            | MakerNoteVendor::OlympusRawDevelopment
            | MakerNoteVendor::OlympusImageProcessing
            | MakerNoteVendor::OlympusFocusInfo
            | MakerNoteVendor::OlympusRawInfo => 0,
            _ => 0,
        }
    }

    pub const fn offset_correction(&self) -> i32 {
        if self.has_tiff_header() {
            // Vendor has TIFF header - only removed proprietary header
            // Nikon: removed 10 bytes (proprietary header), TIFF header remains
            // Offsets in Nikon MakerNote are relative to TIFF header start (after proprietary header)
            // So no correction needed - offsets already point to the right location
            // tested with Nikon Zf
            0
        } else {
            // Removed header_size bytes (proprietary header)
            // No TIFF header added anymore - offsets point directly to data after removed header
            // Panasonic: header_size=12, so offset_correction = 12
            // Canon: header_size=0, so offset_correction = 0
            // Sony: header_size=12, so offset_correction = 12
            // Olympus: header_size=12, so offset_correction = 12
            // OM System: header_size=16, so offset_correction = 16
            // Fujifilm: header_size=12, so offset_correction = 12
            // Leica: header_size=8, so offset_correction = 8
            // Samsung: header_size=0, so offset_correction = 0
            // Apple: header_size=14, so offset_correction = 14
            // Sigma: header_size=10, so offset_correction = 10
            // tested with Lumix S1R2 (Panasonic), ILCE-7M5 (Sony), EOS R6 Mark III (Canon), LEICA Q2 (Leica)
            self.header_size() as i32
        }
    }

    pub const fn consider_tiff_offset(&self) -> bool {
        match self {
            // Panasonic, Canon, Sony, Leica, Sigma use TIFF-relative offsets
            MakerNoteVendor::Panasonic | MakerNoteVendor::Canon | MakerNoteVendor::Sony |
            MakerNoteVendor::Leica | MakerNoteVendor::Sigma => true,
            // Nikon, Olympus, OM System, Fujifilm, Samsung, Apple, Pentax - use MakerNote-relative offsets
            MakerNoteVendor::Nikon | MakerNoteVendor::Olympus | MakerNoteVendor::OMSystem |
            MakerNoteVendor::Fujifilm | MakerNoteVendor::Samsung | MakerNoteVendor::Apple |
            MakerNoteVendor::Ricoh | MakerNoteVendor::Pentax => false,
            // Olympus subdirectories use MakerNote-relative offsets (inherited from parent)
            MakerNoteVendor::OlympusEquipment
            | MakerNoteVendor::OlympusCameraSettings
            | MakerNoteVendor::OlympusRawDevelopment
            | MakerNoteVendor::OlympusImageProcessing
            | MakerNoteVendor::OlympusFocusInfo
            | MakerNoteVendor::OlympusRawInfo => false,
            // Default
            MakerNoteVendor::Unknown => false,
        }
    }

    /// Some vendors use non-standard TIFF magic number in their TIFF-like header.
    /// Pentax/Ricoh uses 0x0057 instead of standard 0x002A.
    pub const fn has_nonstandard_tiff_magic(&self) -> bool {
        matches!(self, MakerNoteVendor::Pentax)
    }

    /// Returns whether this vendor already has a TIFF header after the proprietary header.
    ///
    /// If true, the data after proprietary header already contains a valid TIFF header,
    /// so we should not add a dummy TIFF header.
    pub const fn has_tiff_header(&self) -> bool {
        match self {
            // Nikon Type 3 already has TIFF header after "Nikon\0" + version
            MakerNoteVendor::Nikon => true,
            // Pentax/Ricoh has custom header without proper TIFF header
            // "PENTAX \0II" or "RICOH\0II" + "II/MM" + data
            MakerNoteVendor::Ricoh | MakerNoteVendor::Pentax => false,
            // All other vendors either have no header or need a TIFF header added
            MakerNoteVendor::Panasonic | MakerNoteVendor::Canon | MakerNoteVendor::Sony |
            MakerNoteVendor::Olympus | MakerNoteVendor::OMSystem | MakerNoteVendor::Fujifilm |
            MakerNoteVendor::Leica | MakerNoteVendor::Samsung | MakerNoteVendor::Apple |
            MakerNoteVendor::Sigma => false,
            // Subdirectories don't need TIFF headers (already inside parsed data)
            MakerNoteVendor::OlympusEquipment
            | MakerNoteVendor::OlympusCameraSettings
            | MakerNoteVendor::OlympusRawDevelopment
            | MakerNoteVendor::OlympusImageProcessing
            | MakerNoteVendor::OlympusFocusInfo
            | MakerNoteVendor::OlympusRawInfo => false,
            // Unknown vendors - assume TIFF header exists (for generic parsing and tests)
            MakerNoteVendor::Unknown => true,
        }
    }
}

/// A tag for MakerNote data, vendor-specific.
///
/// Unlike standard EXIF tags, MakerNote tags are manufacturer-specific.
/// The same tag number has completely different meanings for different vendors.
///
/// # Examples
/// ```ignore
/// use exif::make_note::{MakerTag, MakerNoteVendor};
///
/// // Panasonic tag 0x0001 is "ImageQuality"
/// let panasonic_tag = MakerTag::new(MakerNoteVendor::Panasonic, 0x0001);
///
/// // Nikon tag 0x0001 is "MakerNoteVersion" (completely different!)
/// let nikon_tag = MakerTag::new(MakerNoteVendor::Nikon, 0x0001);
///
/// assert_ne!(panasonic_tag, nikon_tag);
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MakerTag {
    /// The camera vendor/manufacturer
    pub vendor: MakerNoteVendor,
    /// The tag number (manufacturer-specific meaning)
    pub number: u16,
}

impl MakerTag {
    /// Creates a new MakerTag.
    pub const fn new(vendor: MakerNoteVendor, number: u16) -> Self {
        MakerTag { vendor, number }
    }

    /// Returns the vendor.
    #[inline]
    pub fn vendor(self) -> MakerNoteVendor {
        self.vendor
    }

    /// Returns the tag number.
    #[inline]
    pub fn number(self) -> u16 {
        self.number
    }

    /// Returns the tag name if known, otherwise None.
    pub fn name(&self) -> Option<&'static str> {
        match self.vendor {
            MakerNoteVendor::Panasonic => super::panasonic::tag_name(self.number),
            MakerNoteVendor::Nikon => super::nikon::tag_name(self.number),
            MakerNoteVendor::Canon => super::canon::tag_name(self.number),
            MakerNoteVendor::Sony => super::sony::tag_name(self.number),
            MakerNoteVendor::Olympus | MakerNoteVendor::OMSystem => super::olympus::tag_name(self.number),
            MakerNoteVendor::Fujifilm => super::fujifilm::tag_name(self.number),
            MakerNoteVendor::Leica => super::panasonic::tag_name(self.number),
            MakerNoteVendor::Samsung => super::samsung::tag_name(self.number),
            MakerNoteVendor::Apple => super::apple::tag_name(self.number),
            MakerNoteVendor::Sigma => super::sigma::tag_name(self.number),
            MakerNoteVendor::Ricoh | MakerNoteVendor::Pentax => super::pentax::tag_name(self.number),
            MakerNoteVendor::OlympusEquipment => super::olympus::olympus_equipment_tag_name(self.number),
            MakerNoteVendor::OlympusCameraSettings => super::olympus::olympus_camera_settings_tag_name(self.number),
            MakerNoteVendor::OlympusRawDevelopment => super::olympus::olympus_raw_development_tag_name(self.number),
            MakerNoteVendor::OlympusImageProcessing => super::olympus::olympus_image_processing_tag_name(self.number),
            MakerNoteVendor::OlympusFocusInfo => super::olympus::olympus_focus_info_tag_name(self.number),
            MakerNoteVendor::OlympusRawInfo => super::olympus::olympus_raw_info_tag_name(self.number),
            _ => None,
        }
    }

    /// Returns the tag description if known, otherwise None.
    pub fn description(&self) -> Option<&'static str> {
        match self.vendor {
            MakerNoteVendor::Panasonic => super::panasonic::tag_description(self.number),
            MakerNoteVendor::Nikon => super::nikon::tag_description(self.number),
            MakerNoteVendor::Canon => super::canon::tag_description(self.number),
            MakerNoteVendor::Sony => super::sony::tag_description(self.number),
            MakerNoteVendor::Olympus | MakerNoteVendor::OMSystem => super::olympus::tag_description(self.number),
            MakerNoteVendor::Fujifilm => super::fujifilm::tag_description(self.number),
            MakerNoteVendor::Leica => super::panasonic::tag_description(self.number),
            MakerNoteVendor::Samsung => super::samsung::tag_description(self.number),
            MakerNoteVendor::Apple => super::apple::tag_description(self.number),
            MakerNoteVendor::Sigma => super::sigma::tag_description(self.number),
            MakerNoteVendor::Ricoh | MakerNoteVendor::Pentax => super::pentax::tag_description(self.number),
            MakerNoteVendor::OlympusEquipment => super::olympus::olympus_equipment_tag_description(self.number),
            MakerNoteVendor::OlympusCameraSettings => super::olympus::olympus_camera_settings_tag_description(self.number),
            MakerNoteVendor::OlympusRawDevelopment => super::olympus::olympus_raw_development_tag_description(self.number),
            MakerNoteVendor::OlympusImageProcessing => super::olympus::olympus_image_processing_tag_description(self.number),
            MakerNoteVendor::OlympusFocusInfo => super::olympus::olympus_focus_info_tag_description(self.number),
            MakerNoteVendor::OlympusRawInfo => super::olympus::olympus_raw_info_tag_description(self.number),
            _ => None,
        }
    }
}

impl fmt::Display for MakerTag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.name() {
            Some(name) => write!(f, "{}:{}", self.vendor, name),
            None => write!(f, "{}:0x{:04X}", self.vendor, self.number),
        }
    }
}

/// A field in MakerNote data with vendor-specific tag.
///
/// Similar to the standard `Field` but uses `MakerTag` instead of `Tag`.
#[derive(Debug, Clone)]
pub struct MakerNoteField {
    /// The tag representing the meaning of this field
    pub tag: MakerTag,
    /// IFD number (usually 0 for MakerNote)
    pub ifd_num: super::In,
    /// The value of the field
    pub value: crate::value::Value,
    /// Custom display string (if vendor-specific display is needed)
    pub custom_display: Option<String>,
}

impl MakerNoteField {
    /// Creates a new MakerNoteField.
    pub fn new(tag: MakerTag, ifd_num: super::In, value: crate::value::Value) -> Self {
        // Try to get vendor-specific custom display
        let custom_display = match tag.vendor {
            MakerNoteVendor::Panasonic => super::panasonic::display_value(tag.number, &value),
            MakerNoteVendor::Nikon => super::nikon::display_value(tag.number, &value),
            MakerNoteVendor::Canon => super::canon::display_value(tag.number, &value),
            MakerNoteVendor::Sony => super::sony::display_value(tag.number, &value),
            MakerNoteVendor::Olympus | MakerNoteVendor::OMSystem => super::olympus::display_value(tag.number, &value),
            MakerNoteVendor::Fujifilm => super::fujifilm::display_value(tag.number, &value),
            MakerNoteVendor::Leica => super::panasonic::display_value(tag.number, &value),
            MakerNoteVendor::Samsung => super::samsung::display_value(tag.number, &value),
            MakerNoteVendor::Apple => super::apple::display_value(tag.number, &value),
            MakerNoteVendor::Sigma => super::sigma::display_value(tag.number, &value),
            MakerNoteVendor::Ricoh | MakerNoteVendor::Pentax => super::pentax::display_value(tag.number, &value),
            MakerNoteVendor::OlympusEquipment => super::olympus::olympus_equipment_display_value(tag.number, &value),
            MakerNoteVendor::OlympusCameraSettings => super::olympus::olympus_camera_settings_display_value(tag.number, &value),
            MakerNoteVendor::OlympusRawDevelopment => super::olympus::olympus_raw_development_display_value(tag.number, &value),
            MakerNoteVendor::OlympusImageProcessing => super::olympus::olympus_image_processing_display_value(tag.number, &value),
            MakerNoteVendor::OlympusFocusInfo => super::olympus::olympus_focus_info_display_value(tag.number, &value),
            MakerNoteVendor::OlympusRawInfo => super::olympus::olympus_raw_info_display_value(tag.number, &value),
            _ => None,
        };

        MakerNoteField { tag, ifd_num, value, custom_display }
    }

    /// Returns a display value for this field.
    pub fn display_value(&self) -> MakerNoteDisplayValue<'_> {
        MakerNoteDisplayValue { field: self }
    }
}

/// Helper struct for displaying MakerNoteField values
pub struct MakerNoteDisplayValue<'a> {
    field: &'a MakerNoteField,
}

impl fmt::Display for MakerNoteDisplayValue<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref custom_str) = self.field.custom_display {
            write!(f, "{}", custom_str)
        } else {
            // Fall back to default display
            let standard_tag = crate::tag::Tag(crate::tag::Context::Tiff, self.field.tag.number);
            self.field.value.display_as(standard_tag).fmt(f)
        }
    }
}

/// Display Undefined value as null-terminated string (with ignored endianness parameter for trait compatibility)
pub(crate) fn d_undef_as_string(value: &Value, _le: Option<bool>) -> String {
    match value {
        Value::Undefined(bytes, _) => {
            let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
            std::str::from_utf8(&bytes[..end])
                .unwrap_or("<invalid UTF-8>")
                .to_string()
        }
        Value::Ascii(vec) => {
            // For Ascii, take the first non-empty string
            vec.iter()
                .find(|s| !s.is_empty())
                .and_then(|s| std::str::from_utf8(s).ok())
                .unwrap_or("")
                .to_string()
        }
        Value::Byte(bytes) => {
            // For Byte, treat as null-terminated string
            let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
            std::str::from_utf8(&bytes[..end])
                .unwrap_or("<invalid UTF-8>")
                .to_string()
        }
        _ => format!("{:?}", value),
    }
}

/// Extract null-terminated string from byte array
#[inline]
pub(crate) fn extract_string(bytes: &[u8]) -> String {
    let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    std::str::from_utf8(&bytes[..end])
        .unwrap_or("")
        .to_string()
}

/// Extract optional null-terminated string (returns None if empty)
#[inline]
pub(crate) fn extract_optional_string(bytes: &[u8]) -> Option<String> {
    let s = extract_string(bytes);
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}
