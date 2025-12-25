//
// Apple MakerNote Tag definitions
// Based on iPhoto information with iPhone 17 Pro
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use crate::Value;

/// Apple Acceleration Vector (3D acceleration in units of g)
/// Based on https://exiftool.org/TagNames/Apple.html
#[derive(Debug, Clone, PartialEq)]
pub struct AppleAccelerationVector {
    pub x: f64,  // Positive X = toward left side of phone
    pub y: f64,  // Positive Y = toward bottom of phone
    pub z: f64,  // Positive Z = into face of phone
}

impl std::fmt::Display for AppleAccelerationVector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "X:{:.3}g, Y:{:.3}g, Z:{:.3}g", self.x, self.y, self.z)
    }
}

impl StructuredMakerNoteData for AppleAccelerationVector {
    /// Parse from raw bytes (not applicable for rational data)
    /// This struct uses Value::SRational directly, so raw_parse returns None
    fn raw_parse(_data: &[u8], _le: Option<bool>) -> Option<Self> {
        None
    }

    /// Extract from Value type
    /// Apple stores acceleration as rational64s[3], so we handle SRational directly
    fn from_value(value: &Value, _le: Option<bool>) -> Option<Self> {
        match value {
            Value::SRational(rationals) => {
                if rationals.len() != 3 {
                    return None;
                }
                Some(AppleAccelerationVector {
                    x: rationals[0].to_f64(),
                    y: rationals[1].to_f64(),
                    z: rationals[2].to_f64(),
                })
            }
            _ => None,
        }
    }
}

generate_maker_tags! {
    vendor: Apple,
    tags: [
    (MakerNoteVersion, 0x0001, "Maker Note Version"),
    (AEMatrix, 0x0002, "AE Matrix"),
    (RunTime, 0x0003, "Run Time"),
    (AEStable, 0x0004, "AE Stable"),
    (AETarget, 0x0005, "AE Target"),
    (AEAverage, 0x0006, "AE Average"),
    (AFStable, 0x0007, "AF Stable"),
    (AccelerationVector, 0x0008, "Acceleration Vector", AppleAccelerationVector::from_value_to_string),
    (FocusDistanceRange, 0x000c, "Focus Distance Range"),
    (Apple_0x000d, 0x000d, "Apple 0x000d"),
    (Apple_0x000e, 0x000e, "Apple 0x000e"),
    (Apple_0x0010, 0x0010, "Apple 0x0010"),
    (ImageCaptureType, 0x0014, "Image Capture Type"),
    (Apple_0x0016, 0x0016, "Apple 0x0016"),
    (LivePhotoVideoIndex, 0x0017, "Live Photo Video Index"),
    (ImageProcessingFlags, 0x0019, "Image Processing Flags"),
    (QualityHint, 0x001a, "Quality Hint"),
    (LuminanceNoiseAmplitude, 0x001d, "Luminance Noise Amplitude"),
    (PhotosAppFeatureFlags, 0x001f, "Photos App Feature Flags"),
    (ImageCaptureRequestID, 0x0020, "Image Capture Request ID"),
    (HDRHeadroom, 0x0021, "HDR Headroom"),
    (AFPerformance, 0x0023, "AF Performance"),
    (SceneFlags, 0x0025, "Scene Flags"),
    (SignalToNoiseRatioType, 0x0026, "Signal To Noise Ratio Type"),
    (SignalToNoiseRatio, 0x0027, "Signal To Noise Ratio"),
    (PhotoIdentifier, 0x002b, "Photo Identifier"),
    (ColorTemperature, 0x002f, "Color Temperature"),
    (CameraType, 0x0033, "Camera Type"),
    (FocusPosition, 0x0034, "Focus Position"),
    (HDRGain, 0x0035, "HDR Gain"),
    (Apple_0x0036, 0x0036, "Apple 0x0036"),
    (Apple_0x0037, 0x0037, "Apple 0x0037"),
    (AFMeasuredDepth, 0x0038, "AF Measured Depth"),
    (Apple_0x0039, 0x0039, "Apple 0x0039"),
    (Apple_0x003a, 0x003a, "Apple 0x003a"),
    (Apple_0x003b, 0x003b, "Apple 0x003b"),
    (Apple_0x003c, 0x003c, "Apple 0x003c"),
    (AFConfidence, 0x003d, "AF Confidence"),
    (GreenGhostMitigationStatus, 0x003e, "Green Ghost Mitigation Status"),
    (SemanticStyleRenderingVer, 0x003f, "Semantic Style Rendering Ver"),
    (SemanticStylePreset, 0x0040, "Semantic Style Preset"),
    (Apple_0x0043, 0x0043, "Apple 0x0043"),
    (Apple_0x0044, 0x0044, "Apple 0x0044"),
    (Apple_0x0045, 0x0045, "Apple 0x0045"),
    (Apple_0x0046, 0x0046, "Apple 0x0046"),
    (Apple_0x0048, 0x0048, "Apple 0x0048"),
    (Apple_0x0049, 0x0049, "Apple 0x0049"),
    (Apple_0x004a, 0x004a, "Apple 0x004a"),
    (Apple_0x004d, 0x004d, "Apple 0x004d"),
    (Apple_0x004e, 0x004e, "Apple 0x004e"),
    (Apple_0x004f, 0x004f, "Apple 0x004f"),
    (Apple_0x0052, 0x0052, "Apple 0x0052"),
    (Apple_0x0053, 0x0053, "Apple 0x0053"),
    (Apple_0x0054, 0x0054, "Apple 0x0054"),
    (Apple_0x0055, 0x0055, "Apple 0x0055"),
    (Apple_0x0058, 0x0058, "Apple 0x0058"),
    (Apple_0x005a, 0x005a, "Apple 0x005a"),
    ]
}
