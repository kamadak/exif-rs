//
// Sony MakerNote Tag definitions
// Based on https://exiftool.org/TagNames/Sony.html
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use strum::{Display, FromRepr};

/// Sony Creative Style setting
/// Based on Sony Tag9416 offset 0x0037
/// https://exiftool.org/TagNames/Sony.html#Tag9416
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum SonyCreativeStyle {
    #[strum(serialize = "Standard")]
    Standard = 0,
    #[strum(serialize = "Vivid")]
    Vivid = 1,
    #[strum(serialize = "Neutral")]
    Neutral = 2,
    #[strum(serialize = "Portrait")]
    Portrait = 3,
    #[strum(serialize = "Landscape")]
    Landscape = 4,
    #[strum(serialize = "B&W")]
    BW = 5,
    #[strum(serialize = "Clear")]
    Clear = 6,
    #[strum(serialize = "Deep")]
    Deep = 7,
    #[strum(serialize = "Light")]
    Light = 8,
    #[strum(serialize = "Sunset")]
    Sunset = 9,
    #[strum(serialize = "Night View/Portrait")]
    NightView = 10,
    #[strum(serialize = "Autumn Leaves")]
    AutumnLeaves = 11,
    #[strum(serialize = "Sepia")]
    Sepia = 13,
    #[strum(serialize = "FL")]
    FL = 15,
    #[strum(serialize = "VV2")]
    VV2 = 16,
    #[strum(serialize = "IN")]
    IN = 17,
    #[strum(serialize = "SH")]
    SH = 18,
    #[strum(serialize = "FL2")]
    FL2 = 19,
    #[strum(serialize = "FL3")]
    FL3 = 20,
    #[strum(serialize = "Off")]
    Off = 255,
}

impl_simple_enum_make_note_raw_parse!(SonyCreativeStyle, u8);

/// Sony Lens Mount type
/// Offset 0x0048, 0x004a
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum SonyLensMount {
    #[strum(serialize = "Unknown")]
    Unknown = 0,
    #[strum(serialize = "A-mount")]
    AMount = 1,
    #[strum(serialize = "E-mount")]
    EMount = 2,
    #[strum(serialize = "A-mount (3)")]
    AMount3 = 3,
}

impl_simple_enum_make_note_raw_parse!(SonyLensMount, u8);

/// Sony Lens Format
/// Offset 0x0049
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u8)]
pub enum SonyLensFormat {
    #[strum(serialize = "Unknown")]
    Unknown = 0,
    #[strum(serialize = "APS-C")]
    APSC = 1,
    #[strum(serialize = "Full-frame")]
    FullFrame = 2,
}

impl_simple_enum_make_note_raw_parse!(SonyLensFormat, u8);

/// Sony Tag9416 parsed structure
/// Valid for ILCE-1/6700/7CM2/7CR/7M4/7RM5/7SM3/9M3, ILME-FX2/FX3/FX30, ZV-E1/E10M2
#[derive(Debug, Clone, PartialEq)]
pub struct SonyTag9416 {
    /// Creative Style preset (offset 0x0037)
    pub creative_style: SonyCreativeStyle,
    /// Lens mount type (offset 0x0048)
    pub lens_mount: Option<SonyLensMount>,
    /// Lens format (offset 0x0049)
    pub lens_format: Option<SonyLensFormat>,
    /// Focal length in mm (offset 0x0071, int16u / 10)
    pub focal_length: Option<f32>,
    /// Min focal length in mm (offset 0x0073, int16u / 10)
    pub min_focal_length: Option<f32>,
    /// Max focal length in mm (offset 0x0075, int16u / 10)
    pub max_focal_length: Option<f32>,
}

impl std::fmt::Display for SonyTag9416 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CreativeStyle:{}", self.creative_style)?;

        if let Some(mount) = &self.lens_mount {
            write!(f, ", LensMount:{}", mount)?;
        }

        if let Some(format) = &self.lens_format {
            write!(f, ", LensFormat:{}", format)?;
        }

        if let Some(fl) = self.focal_length {
            write!(f, ", FocalLength:{:.1}mm", fl)?;
        }

        if let (Some(min_fl), Some(max_fl)) = (self.min_focal_length, self.max_focal_length) {
            if min_fl != max_fl {
                write!(f, ", FocalRange:{:.1}-{:.1}mm", min_fl, max_fl)?;
            }
        }

        Ok(())
    }
}

impl StructuredMakerNoteData for SonyTag9416 {
    /// Parse Sony Tag9416 from raw bytes
    ///
    /// Tag9416 has variable format with multiple fields
    fn raw_parse(data: &[u8], _le: Option<bool>) -> Option<SonyTag9416> {
        // Need at least 0x76 bytes to read all basic fields
        if data.len() < 0x76 {
            return None;
        }

        let creative_style = SonyCreativeStyle::from_repr(data[0x37])?;

        let lens_mount = if data.len() > 0x48 {
            SonyLensMount::from_repr(data[0x48])
        } else {
            None
        };

        let lens_format = if data.len() > 0x49 {
            SonyLensFormat::from_repr(data[0x49])
        } else {
            None
        };

        let focal_length = if data.len() >= 0x73 {
            let val = u16::from_le_bytes([data[0x71], data[0x72]]);
            if val > 0 {
                Some(val as f32 / 10.0)
            } else {
                None
            }
        } else {
            None
        };

        let min_focal_length = if data.len() >= 0x75 {
            let val = u16::from_le_bytes([data[0x73], data[0x74]]);
            if val > 0 {
                Some(val as f32 / 10.0)
            } else {
                None
            }
        } else {
            None
        };

        let max_focal_length = if data.len() >= 0x77 {
            let val = u16::from_le_bytes([data[0x75], data[0x76]]);
            if val > 0 {
                Some(val as f32 / 10.0)
            } else {
                None
            }
        } else {
            None
        };

        Some(SonyTag9416 {
            creative_style,
            lens_mount,
            lens_format,
            focal_length,
            min_focal_length,
            max_focal_length,
        })
    }
}

/// Sony Face Information parsed structure
/// Based on https://exiftool.org/TagNames/Sony.html
#[derive(Debug, Clone, PartialEq)]
pub struct SonyFaceInfo {
    pub faces_detected: i16,
    pub faces: Vec<SonyFacePosition>,
}

/// Sony face position with coordinates (top, left, height, width)
/// Coordinates are for full-sized unrotated image with Y increasing downwards
#[derive(Debug, Clone, PartialEq)]
pub struct SonyFacePosition {
    pub top: u16,
    pub left: u16,
    pub height: u16,
    pub width: u16,
}

impl std::fmt::Display for SonyFaceInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.faces_detected < 0 {
            return write!(f, "n/a");
        }
        write!(f, "{} face(s)", self.faces_detected)?;
        if !self.faces.is_empty() {
            write!(f, " [")?;
            for (i, face) in self.faces.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "(T:{},L:{} {}x{})", face.top, face.left, face.width, face.height)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl StructuredMakerNoteData for SonyFaceInfo {
    /// Parse Sony Face Info from Index2 format array
    /// Based on https://exiftool.org/TagNames/Sony.html
    /// Index 0: FacesDetected (int16s)
    /// Index 1, 6, 11, 16, 21, 26, 31, 36: Face positions (int16u[4])
    ///
    /// Sony uses little-endian by default
    fn raw_parse(data: &[u8], le: Option<bool>) -> Option<SonyFaceInfo> {
        let le = le.unwrap_or(true); // Default to little-endian for Sony
        // Need at least 2 bytes for FacesDetected
        if data.len() < 2 {
            return None;
        }

        let faces_detected = if le {
            i16::from_le_bytes([data[0], data[1]])
        } else {
            i16::from_be_bytes([data[0], data[1]])
        };

        if faces_detected < 0 {
            return Some(SonyFaceInfo {
                faces_detected,
                faces: Vec::new(),
            });
        }

        let num_faces = faces_detected.min(8) as usize;
        let mut faces = Vec::new();

        // Face positions are at indices 1, 6, 11, 16, 21, 26, 31, 36
        // Each index in Index2 format = 2 bytes, so byte offset = index * 2
        for i in 0..num_faces {
            let index = 1 + i * 5; // Index2 positions: 1, 6, 11, 16, 21, 26, 31, 36
            let offset = index * 2; // Byte offset

            if offset + 8 > data.len() {
                break;
            }

            let top = if le {
                u16::from_le_bytes([data[offset], data[offset + 1]])
            } else {
                u16::from_be_bytes([data[offset], data[offset + 1]])
            };

            let left = if le {
                u16::from_le_bytes([data[offset + 2], data[offset + 3]])
            } else {
                u16::from_be_bytes([data[offset + 2], data[offset + 3]])
            };

            let height = if le {
                u16::from_le_bytes([data[offset + 4], data[offset + 5]])
            } else {
                u16::from_be_bytes([data[offset + 4], data[offset + 5]])
            };

            let width = if le {
                u16::from_le_bytes([data[offset + 6], data[offset + 7]])
            } else {
                u16::from_be_bytes([data[offset + 6], data[offset + 7]])
            };

            faces.push(SonyFacePosition { top, left, height, width });
        }

        Some(SonyFaceInfo { faces_detected, faces })
    }
}

generate_maker_tags! {
    vendor: Sony,
    tags: [
    (CameraInfo, 0x0010, "Camera Info"),
    (FocusInfo, 0x0020, "Focus Info", SonyFaceInfo::from_value_to_string),
    (Quality, 0x0102, "Quality"),
    (FlashExposureComp, 0x0104, "Flash Exposure Comp"),
    (Teleconverter, 0x0105, "Teleconverter"),
    (WhiteBalanceFineTune, 0x0112, "White Balance Fine Tune"),
    (CameraSettings, 0x0114, "Camera Settings"),
    (WhiteBalance, 0x0115, "White Balance"),
    (ExtraInfo, 0x0116, "Extra Info"),
    (PrintIM, 0x0e00, "Print IM"),
    (MultiBurstMode, 0x1000, "Multi Burst Mode"),
    (MultiBurstImageWidth, 0x1001, "Multi Burst Image Width"),
    (MultiBurstImageHeight, 0x1002, "Multi Burst Image Height"),
    (Panorama, 0x1003, "Panorama"),
    (PreviewImage, 0x2001, "Preview Image"),
    (Rating, 0x2002, "Rating"),
    (Contrast, 0x2004, "Contrast"),
    (Saturation, 0x2005, "Saturation"),
    (Sharpness, 0x2006, "Sharpness"),
    (Brightness, 0x2007, "Brightness"),
    (LongExposureNoiseReduction, 0x2008, "Long Exposure Noise Reduction"),
    (HighISONoiseReduction, 0x2009, "High ISO Noise Reduction"),
    (HDR, 0x200a, "HDR"),
    (MultiFrameNoiseReduction, 0x200b, "Multi Frame Noise Reduction"),
    (PictureEffect, 0x200e, "Picture Effect"),
    (SoftSkinEffect, 0x200f, "Soft Skin Effect"),
    (VignettingCorrection, 0x2011, "Vignetting Correction"),
    (LateralChromaticAberration, 0x2012, "Lateral Chromatic Aberration"),
    (DistortionCorrectionSetting, 0x2013, "Distortion Correction Setting"),
    (WBShiftAB_GM, 0x2014, "WB Shift AB GM"),
    (AutoPortraitFramed, 0x2016, "Auto Portrait Framed"),
    (FlashAction, 0x2017, "Flash Action"),
    (ElectronicFrontCurtainShutter, 0x201a, "Electronic Front Curtain Shutter"),
    (FocusMode, 0x201b, "Focus Mode"),
    (AFAreaModeSetting, 0x201c, "AF Area Mode Setting"),
    (FlexibleSpotPosition, 0x201d, "Flexible Spot Position"),
    (AFPointSelected, 0x201e, "AF Point Selected"),
    (AFPointsUsed, 0x2020, "AF Points Used"),
    (AFTracking, 0x2021, "AF Tracking"),
    (FocalPlaneAFPointsUsed, 0x2022, "Focal Plane AF Points Used"),
    (MultiFrameNREffect, 0x2023, "Multi Frame NR Effect"),
    (WBShiftAB_GM_Precise, 0x2026, "WB Shift AB GM Precise"),
    (FocusLocation, 0x2027, "Focus Location"),
    (VariableLowPassFilter, 0x2028, "Variable Low Pass Filter"),
    (RAWFileType, 0x2029, "RAW File Type"),
    (PrioritySetInAWB, 0x202b, "Priority Set In AWB"),
    (MeteringMode2, 0x202c, "Metering Mode 2"),
    (ExposureStandardAdjustment, 0x202d, "Exposure Standard Adjustment"),
    (Quality2, 0x202e, "Quality 2"),
    (PixelShiftInfo, 0x202f, "Pixel Shift Info"),
    (SerialNumber, 0x2031, "Serial Number"),
    (Shadows, 0x2032, "Shadows"),
    (Highlights, 0x2033, "Highlights"),
    (Fade, 0x2034, "Fade"),
    (SharpnessRange, 0x2035, "Sharpness Range"),
    (Clarity, 0x2036, "Clarity"),
    (FocusFrameSize, 0x2037, "Focus Frame Size"),
    (JPEG_HEIFSwitch, 0x2039, "JPEG-HEIF Switch"),
    (FocusLocation2, 0x204a, "Focus Location 2"),
    (StepCropShooting, 0x205c, "Step Crop Shooting"),
    (ShotInfo, 0x3000, "Shot Info"),
    (AFInfo, 0x940e, "AF Info"),
    (Sony_0x9416, 0x9416, "Sony Tag9416", SonyTag9416::from_value_to_string),
    (FileFormat, 0xb000, "File Format"),
    (SonyModelID, 0xb001, "Sony Model ID"),
    (CreativeStyle, 0xb020, "Creative Style"),
    (ColorTemperature, 0xb021, "Color Temperature"),
    (ColorCompensationFilter, 0xb022, "Color Compensation Filter"),
    (SceneMode, 0xb023, "Scene Mode"),
    (ZoneMatching, 0xb024, "Zone Matching"),
    (DynamicRangeOptimizer, 0xb025, "Dynamic Range Optimizer"),
    (ImageStabilization, 0xb026, "Image Stabilization"),
    (LensType, 0xb027, "Lens Type"),
    (MinoltaMakerNote, 0xb028, "Minolta Maker Note"),
    (ColorMode, 0xb029, "Color Mode"),
    (LensSpec, 0xb02a, "Lens Spec"),
    (FullImageSize, 0xb02b, "Full Image Size"),
    (PreviewImageSize, 0xb02c, "Preview Image Size"),
    (Macro, 0xb040, "Macro"),
    (ExposureMode, 0xb041, "Exposure Mode"),
    (FocusMode2, 0xb042, "Focus Mode 2"),
    (AFAreaMode, 0xb043, "AF Area Mode"),
    (AFIlluminator, 0xb044, "AF Illuminator"),
    (JPEGQuality, 0xb047, "JPEG Quality"),
    (FlashLevel, 0xb048, "Flash Level"),
    (ReleaseMode, 0xb049, "Release Mode"),
    (SequenceNumber, 0xb04a, "Sequence Number"),
    (AntiBlur, 0xb04b, "Anti-Blur"),
    (FocusMode3, 0xb04e, "Focus Mode 3"),
    (DynamicRangeOptimizer2, 0xb04f, "Dynamic Range Optimizer 2"),
    (HighISONoiseReduction2, 0xb050, "High ISO Noise Reduction 2"),
    (IntelligentAuto, 0xb052, "Intelligent Auto"),
    (WhiteBalance2, 0xb054, "White Balance 2"),
    ]
}
