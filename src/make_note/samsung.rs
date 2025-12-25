//
// Samsung MakerNote Tag definitions
// Based on NX500 EXIF tags
// MakerNote data was observed in files produced by Samsung NX mirrorless cameras.
// However No EXIF MakerNote was detected in images taken with Samsung Galaxy smartphones;
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use crate::Value;
use strum::{Display, FromRepr};

/// Samsung Color Matrix (3x3) parsed structure
/// Based on https://exiftool.org/TagNames/Samsung.html
#[derive(Debug, Clone, PartialEq)]
pub struct SamsungColorMatrix {
    pub matrix: [[i32; 3]; 3],
}

impl std::fmt::Display for SamsungColorMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, row) in self.matrix.iter().enumerate() {
            if i > 0 {
                write!(f, "; ")?;
            }
            write!(f, "{}, {}, {}", row[0], row[1], row[2])?;
        }
        write!(f, "]")
    }
}

impl StructuredMakerNoteData for SamsungColorMatrix {
    /// Parse Samsung Color Matrix from raw bytes
    /// Based on https://exiftool.org/TagNames/Samsung.html
    ///
    /// Samsung uses little-endian by default
    fn raw_parse(data: &[u8], le: Option<bool>) -> Option<SamsungColorMatrix> {
        let le = le.unwrap_or(true); // Default to little-endian for Samsung
        // ColorMatrix is int32s[9]
        if data.len() < 36 {
            return None;
        }

        let mut matrix = [[0i32; 3]; 3];
        for i in 0..9 {
            let offset = i * 4;
            let value = if le {
                i32::from_le_bytes([
                    data[offset],
                    data[offset + 1],
                    data[offset + 2],
                    data[offset + 3],
                ])
            } else {
                i32::from_be_bytes([
                    data[offset],
                    data[offset + 1],
                    data[offset + 2],
                    data[offset + 3],
                ])
            };
            matrix[i / 3][i % 3] = value;
        }

        Some(SamsungColorMatrix { matrix })
    }

    /// Custom from_value to handle SLong type directly
    fn from_value(value: &Value, le: Option<bool>) -> Option<Self> {
        match value {
            Value::Undefined(data, _) => Self::raw_parse(data, le),
            Value::Byte(data) => Self::raw_parse(data, le),
            Value::SLong(values) => {
                if values.len() != 9 {
                    return None;
                }
                let mut matrix = [[0i32; 3]; 3];
                for i in 0..9 {
                    matrix[i / 3][i % 3] = values[i];
                }
                Some(SamsungColorMatrix { matrix })
            }
            _ => None,
        }
    }
}

/// Samsung Picture Wizard Mode (Tag 0x0a01)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum SamsungPictureWizardMode {
    #[strum(serialize = "Standard")]
    Standard = 0,
    #[strum(serialize = "Vivid")]
    Vivid = 1,
    #[strum(serialize = "Portrait")]
    Portrait = 2,
    #[strum(serialize = "Landscape")]
    Landscape = 3,
    #[strum(serialize = "Forest")]
    Forest = 4,
    #[strum(serialize = "Retro")]
    Retro = 5,
    #[strum(serialize = "Cool")]
    Cool = 6,
    #[strum(serialize = "Calm")]
    Calm = 7,
    #[strum(serialize = "Classic")]
    Classic = 8,
    #[strum(serialize = "Custom1")]
    Custom1 = 9,
    #[strum(serialize = "Custom2")]
    Custom2 = 10,
    #[strum(serialize = "Custom3")]
    Custom3 = 11,
    #[strum(serialize = "n/a")]
    NA = 255,
}

impl_simple_enum_make_note_raw_parse!(SamsungPictureWizardMode, u16);

/// Samsung Smart Album Color (Tag 0x0a00)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum SamsungSmartAlbumColor {
    #[strum(serialize = "Red")]
    Red = 0,
    #[strum(serialize = "Yellow")]
    Yellow = 1,
    #[strum(serialize = "Green")]
    Green = 2,
    #[strum(serialize = "Blue")]
    Blue = 3,
    #[strum(serialize = "Magenta")]
    Magenta = 4,
    #[strum(serialize = "Black")]
    Black = 5,
    #[strum(serialize = "White")]
    White = 6,
    #[strum(serialize = "Various")]
    Various = 7,
}

impl_simple_enum_make_note_raw_parse!(SamsungSmartAlbumColor, u16);

/// Samsung Color Space (Tag 0x0a26)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum SamsungColorSpace {
    #[strum(serialize = "sRGB")]
    SRGB = 0,
    #[strum(serialize = "Adobe RGB")]
    AdobeRGB = 1,
}

impl_simple_enum_make_note_raw_parse!(SamsungColorSpace, u16);

/// Detect byte order from Samsung MakerNote IFD structure.
///
/// Samsung MakerNote always starts with an IFD that contains specific tags.
/// We check the first few tag numbers to determine the correct byte order.
///
/// # Arguments
/// * `data` - The MakerNote data starting with IFD entry count
///
/// # Returns
/// `true` if little-endian, `false` if big-endian
pub(crate) fn detect_samsung_byte_order(data: &[u8]) -> bool {
    // Need at least: entry_count(2) + first_entry(12)
    if data.len() < 14 {
        return true; // Default to little-endian if not enough data
    }

    // IFD structure: [entry_count:u16][entries...][next_ifd:u32]
    // Each entry: [tag:u16][type:u16][count:u32][value_offset:u32]

    // Read first entry's tag number in both endiannesses
    let first_tag_le = u16::from_le_bytes([data[2], data[3]]);
    let first_tag_be = u16::from_be_bytes([data[2], data[3]]);

    // Samsung MakerNote typically starts with tags 0x0001, 0x0002, 0x0003
    // Check if either interpretation matches expected tag numbers
    let le_matches = matches!(first_tag_le, 0x0001 | 0x0002 | 0x0003 | 0x0004);
    let be_matches = matches!(first_tag_be, 0x0001 | 0x0002 | 0x0003 | 0x0004);

    // If we have enough data, check second entry too for confirmation
    if data.len() >= 26 {
        let second_tag_le = u16::from_le_bytes([data[14], data[15]]);
        let second_tag_be = u16::from_be_bytes([data[14], data[15]]);

        let le_matches_2 = matches!(second_tag_le, 0x0001 | 0x0002 | 0x0003 | 0x0004 | 0x000a | 0x000e);
        let be_matches_2 = matches!(second_tag_be, 0x0001 | 0x0002 | 0x0003 | 0x0004 | 0x000a | 0x000e);

        // Both entries should match
        if le_matches && le_matches_2 {
            return true;
        }
        if be_matches && be_matches_2 {
            return false;
        }
    }

    // Fall back to single tag check
    if le_matches && !be_matches {
        true
    } else if be_matches && !le_matches {
        false
    } else {
        // If both or neither match, use entry count heuristic as last resort
        let count_le = u16::from_le_bytes([data[0], data[1]]);
        let count_be = u16::from_be_bytes([data[0], data[1]]);
        // Most Samsung MakerNotes have 20-50 entries
        count_le < count_be && count_le < 200
    }
}

generate_maker_tags! {
    vendor: Samsung,
    tags: [
    (MakerNoteVersion, 0x0001, "Maker Note Version"),
    (DeviceType, 0x0002, "Device Type"),
    (SamsungModelID, 0x0003, "Samsung Model ID"),
    (CameraTemperature, 0x0043, "Camera Temperature"),
    (FaceDetect, 0x0100, "Face Detect"),
    (FaceRecognition, 0x0120, "Face Recognition"),
    (FaceName, 0x0123, "Face Name"),
    (SmartAlbumColor, 0x0a00, "Smart Album Color"),
    (PictureWizardMode, 0x0a01, "Picture Wizard Mode"),
    (PictureWizardColor, 0x0a02, "Picture Wizard Color"),
    (PictureWizardSaturation, 0x0a03, "Picture Wizard Saturation"),
    (PictureWizardSharpness, 0x0a04, "Picture Wizard Sharpness"),
    (PictureWizardContrast, 0x0a05, "Picture Wizard Contrast"),
    (WhiteBalanceSetup, 0x0a08, "White Balance Setup"),
    (RawDataByteOrder, 0x0a09, "Raw Data Byte Order"),
    (RawDataCFAPattern, 0x0a0a, "Raw Data CFA Pattern"),
    (FirmwareName, 0x0a20, "Firmware Name"),
    (SerialNumber, 0x0a21, "Serial Number"),
    (LensType, 0x0a22, "Lens Type"),
    (LensFirmware, 0x0a23, "Lens Firmware"),
    (InternalLensSerialNumber, 0x0a24, "Internal Lens Serial Number"),
    (DustDeleteData, 0x0a25, "Dust Delete Data"),
    (ColorSpace, 0x0a26, "Color Space"),
    (EncryptionKey, 0x0a27, "Encryption Key"),
    (WB_RGGBLevelsAuto, 0x0a28, "WB RGGB Levels Auto"),
    (WB_RGGBLevelsIlluminator1, 0x0a29, "WB RGGB Levels Illuminator1"),
    (WB_RGGBLevelsIlluminator2, 0x0a2a, "WB RGGB Levels Illuminator2"),
    (ImageCount, 0x0a2b, "Image Count"),
    (FlashMode, 0x0a2c, "Flash Mode"),
    (ColorTemperature, 0x0a2d, "Color Temperature"),
    (ColorMatrix, 0x0a2e, "Color Matrix", SamsungColorMatrix::from_value_to_string),
    (ColorMatrixSRGB, 0x0a2f, "Color Matrix sRGB", SamsungColorMatrix::from_value_to_string),
    (ColorMatrixAdobeRGB, 0x0a30, "Color Matrix Adobe RGB", SamsungColorMatrix::from_value_to_string),
    (ToneCurve1, 0x0a31, "Tone Curve 1"),
    (ToneCurve2, 0x0a32, "Tone Curve 2"),
    (ToneCurve3, 0x0a33, "Tone Curve 3"),
    (ToneCurve4, 0x0a34, "Tone Curve 4"),
    (FlashFired, 0x0a35, "Flash Fired"),
    (ImageUniqueID, 0x0a36, "Image Unique ID"),
    (RawImageUniqueID, 0x0a37, "Raw Image Unique ID"),
    (PreviewImageRelativeOrientation, 0x0a38, "Preview Image Relative Orientation"),
    (PreviewImageLength, 0x0a39, "Preview Image Length"),
    (SensorAreas, 0x0a3a, "Sensor Areas"),
    (BatteryLevel, 0x0a3b, "Battery Level"),
    (BlackLevel, 0x0a3c, "Black Level"),
    (WB_RGGBLevelsBlack, 0x0a3d, "WB RGGB Levels Black"),
    (CameraBright, 0x0a3e, "Camera Bright"),
    (CameraContrast, 0x0a3f, "Camera Contrast"),
    (CameraSaturation, 0x0a40, "Camera Saturation"),
    (CameraSharpness, 0x0a41, "Camera Sharpness"),
    (ImageStabilization, 0x0a42, "Image Stabilization"),
    ]
}
