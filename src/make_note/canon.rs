//
// Canon MakerNote Tag definitions
// Based on https://exiftool.org/TagNames/Canon.html
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor};

// Canon Model and Lens type enums (auto-generated)
pub mod canon_model;

// Re-export for convenience
pub use canon_model::{CanonModelID, CanonLensType};

// TODO: Implement structured data parsing for Canon tags
//
// Canon stores several tags as arrays of int16s with index-based field access.
// These could be parsed into structured types similar to Nikon's PictureControlData.
//
// Key candidates for structured parsing:
//
// 1. CanonCameraSettings (0x0001)
//    - Array of int16s with known field positions
//    - Index 1: MacroMode
//    - Index 2: SelfTimer
//    - Index 4: FlashMode
//    - Index 5: ContinuousDrive
//    - Index 7: FocusMode
//    - Index 10: ImageSize
//    - Index 11: EasyMode
//    - Index 13: Contrast
//    - Index 14: Saturation
//    - Index 15: Sharpness
//    - Index 17: MeteringMode
//    - Index 19: AutoISO
//    - Index 20: ISOValue
//    - And many more...
//    - Reference: https://exiftool.org/TagNames/Canon.html#CameraSettings
//
// 2. CanonShotInfo (0x0004)
//    - Array of int16s/int32s with variable-length structure
//    - Different field positions based on camera model
//    - Reference: https://exiftool.org/TagNames/Canon.html#ShotInfo
//
// 3. CanonAFInfo (0x0012)
//    - Array structure with AF point information
//    - Reference: https://exiftool.org/TagNames/Canon.html#AFInfo
//
// 4. CanonFileInfo (0x0093)
//    - Array of int16s with file-related metadata
//    - Reference: https://exiftool.org/TagNames/Canon.html#FileInfo
//
// Implementation approach:
// - Unlike Nikon's fixed binary layout, Canon uses int16/int32 arrays
// - Cannot use simple #[repr(C)] transmute due to variable-length arrays
// - Consider implementing array indexing parser with struct output
// - May need model-specific parsing for some tags (especially ShotInfo)
//
// Example implementation pattern:
// ```rust
// pub struct CanonCameraSettings {
//     pub macro_mode: Option<i16>,        // Index 1
//     pub self_timer: Option<i16>,        // Index 2
//     pub flash_mode: Option<i16>,        // Index 4
//     pub continuous_drive: Option<i16>,  // Index 5
//     // ... more fields
// }
//
// fn parse_camera_settings(data: &[u8]) -> Option<CanonCameraSettings> {
//     // Parse as array of i16s and extract by index
// }
// ```

generate_maker_tags! {
    vendor: Canon,
    tags: [
    (CanonCameraSettings, 0x0001, "Canon Camera Settings"),
    (CanonFocalLength, 0x0002, "Canon Focal Length"),
    (CanonFlashInfo, 0x0003, "Canon Flash Info"),
    (CanonShotInfo, 0x0004, "Canon Shot Info"),
    (CanonPanorama, 0x0005, "Canon Panorama"),
    (CanonImageType, 0x0006, "Canon Image Type"),
    (CanonFirmwareVersion, 0x0007, "Canon Firmware Version"),
    (FileNumber, 0x0008, "File Number"),
    (OwnerName, 0x0009, "Owner Name"),
    (UnknownD30, 0x000a, "Unknown D30"),
    (SerialNumber, 0x000c, "Serial Number"),
    (CanonCameraInfo, 0x000d, "Canon Camera Info"),
    (CanonFileLength, 0x000e, "Canon File Length"),
    (CustomFunctions, 0x000f, "Custom Functions"),
    (CanonModelID, 0x0010, "Canon Model ID"),
    (MovieInfo, 0x0011, "Movie Info"),
    (CanonAFInfo, 0x0012, "Canon AF Info"),
    (ThumbnailImageValidArea, 0x0013, "Thumbnail Image Valid Area"),
    (SerialNumberFormat, 0x0015, "Serial Number Format"),
    (SuperMacro, 0x001a, "Super Macro"),
    (DateStampMode, 0x001c, "Date Stamp Mode"),
    (MyColors, 0x001d, "My Colors"),
    (FirmwareRevision, 0x001e, "Firmware Revision"),
    (Categories, 0x0023, "Categories"),
    (FaceDetect1, 0x0024, "Face Detect 1"),
    (FaceDetect2, 0x0025, "Face Detect 2"),
    (CanonAFInfo2, 0x0026, "Canon AF Info 2"),
    (ContrastInfo, 0x0027, "Contrast Info"),
    (ImageUniqueID, 0x0028, "Image Unique ID"),
    (WBInfo, 0x0029, "WB Info"),
    (FaceDetect3, 0x002f, "Face Detect 3"),
    (TimeInfo, 0x0035, "Time Info"),
    (BatteryType, 0x0038, "Battery Type"),
    (AFInfo3, 0x003c, "AF Info 3"),
    (RawDataOffset, 0x0081, "Raw Data Offset"),
    (RawDataLength, 0x0082, "Raw Data Length"),
    (OriginalDecisionDataOffset, 0x0083, "Original Decision Data Offset"),
    (CustomFunctions1D, 0x0090, "Custom Functions 1D"),
    (PersonalFunctions, 0x0091, "Personal Functions"),
    (PersonalFunctionValues, 0x0092, "Personal Function Values"),
    (CanonFileInfo, 0x0093, "Canon File Info"),
    (AFPointsInFocus1D, 0x0094, "AF Points In Focus 1D"),
    (LensModel, 0x0095, "Lens Model"),
    (SerialInfo, 0x0096, "Serial Info"),
    (DustRemovalData, 0x0097, "Dust Removal Data"),
    (CropInfo, 0x0098, "Crop Info"),
    (CustomFunctions2, 0x0099, "Custom Functions 2"),
    (AspectInfo, 0x009a, "Aspect Info"),
    (ProcessingInfo, 0x00a0, "Processing Info"),
    (ToneCurveTable, 0x00a1, "Tone Curve Table"),
    (SharpnessTable, 0x00a2, "Sharpness Table"),
    (SharpnessFreqTable, 0x00a3, "Sharpness Freq Table"),
    (WhiteBalanceTable, 0x00a4, "White Balance Table"),
    (ColorBalance, 0x00a9, "Color Balance"),
    (MeasuredColor, 0x00aa, "Measured Color"),
    (ColorTemperature, 0x00ae, "Color Temperature"),
    (CanonFlags, 0x00b0, "Canon Flags"),
    (ModifiedInfo, 0x00b1, "Modified Info"),
    (ToneCurveMatching, 0x00b2, "Tone Curve Matching"),
    (WhiteBalanceMatching, 0x00b3, "White Balance Matching"),
    (ColorSpace, 0x00b4, "Color Space"),
    (PreviewImageInfo, 0x00b6, "Preview Image Info"),
    (VRDOffset, 0x00d0, "VRD Offset"),
    (SensorInfo, 0x00e0, "Sensor Info"),
    (ColorData, 0x4001, "Color Data"),
    (CRWParam, 0x4002, "CRW Param"),
    (ColorInfo, 0x4003, "Color Info"),
    (Flavor, 0x4005, "Flavor"),
    (PictureStyleUserDef, 0x4008, "Picture Style User Def"),
    (PictureStylePC, 0x4009, "Picture Style PC"),
    (CustomPictureStyleFileName, 0x4010, "Custom Picture Style File Name"),
    (AFMicroAdj, 0x4013, "AF Micro Adj"),
    (VignettingCorr, 0x4015, "Vignetting Corr"),
    (VignettingCorr2, 0x4016, "Vignetting Corr 2"),
    (LightingOpt, 0x4018, "Lighting Opt"),
    (LensInfo, 0x4019, "Lens Info"),
    (AmbienceInfo, 0x4020, "Ambience Info"),
    (MultiExp, 0x4021, "Multi Exp"),
    (FilterInfo, 0x4024, "Filter Info"),
    (HDRInfo, 0x4025, "HDR Info"),
    (LogInfo, 0x4026, "Log Info"),
    (AFConfig, 0x4028, "AF Config"),
    (RawBurstModeRoll, 0x403f, "Raw Burst Mode Roll"),
    (LevelInfo, 0x4059, "Level Info"),
    ]
}
