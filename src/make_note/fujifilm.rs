//
// Fujifilm MakerNote Tag definitions
// Based on https://exiftool.org/TagNames/FujiFilm.html
//

use crate::make_note::maker_tag::{MakerTag, MakerNoteVendor, StructuredMakerNoteData};
use strum::{Display, FromRepr};

/// FujiFilm Macro (Tag 0x1020)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmMacro {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "On")]
    On = 1,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmMacro, u16);

/// FujiFilm Focus Mode (Tag 0x1021)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmFocusMode {
    #[strum(serialize = "Auto")]
    Auto = 0,
    #[strum(serialize = "Manual")]
    Manual = 1,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmFocusMode, u16);

/// FujiFilm Slow Sync (Tag 0x1030)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmSlowSync {
    #[strum(serialize = "Off")]
    Off = 0,
    #[strum(serialize = "On")]
    On = 1,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmSlowSync, u16);

/// FujiFilm Blur Warning (Tag 0x1300)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmBlurWarning {
    #[strum(serialize = "None")]
    None = 0,
    #[strum(serialize = "Blur Warning")]
    BlurWarning = 1,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmBlurWarning, u16);

/// FujiFilm Focus Warning (Tag 0x1301)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmFocusWarning {
    #[strum(serialize = "Good")]
    Good = 0,
    #[strum(serialize = "Out of focus")]
    OutOfFocus = 1,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmFocusWarning, u16);

/// FujiFilm Dynamic Range (Tag 0x1400)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmDynamicRange {
    #[strum(serialize = "Standard")]
    Standard = 1,
    #[strum(serialize = "Wide")]
    Wide = 3,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmDynamicRange, u16);

/// FujiFilm Film Mode (Tag 0x1401)
/// Film simulation modes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmFilmMode {
    #[strum(serialize = "F0/Standard (Provia)")]
    F0Standard = 0x0,
    #[strum(serialize = "F1/Studio Portrait")]
    F1StudioPortrait = 0x100,
    #[strum(serialize = "F1a/Studio Portrait Enhanced Saturation")]
    F1aStudioPortraitEnhanced = 0x110,
    #[strum(serialize = "F1b/Studio Portrait Smooth Skin Tone (Astia)")]
    F1bStudioPortraitSmooth = 0x120,
    #[strum(serialize = "F2/Fujichrome (Velvia)")]
    F2Fujichrome = 0x200,
    #[strum(serialize = "F4/Velvia")]
    F4Velvia = 0x400,
    #[strum(serialize = "Pro Neg. Std")]
    ProNegStd = 0x500,
    #[strum(serialize = "Classic Chrome")]
    ClassicChrome = 0x600,
    #[strum(serialize = "Eterna")]
    Eterna = 0x700,
    #[strum(serialize = "Classic Negative")]
    ClassicNegative = 0x800,
    #[strum(serialize = "Bleach Bypass")]
    BleachBypass = 0x900,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmFilmMode, u16);

/// FujiFilm Color Mode (Tag 0x1210)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, FromRepr)]
#[repr(u16)]
pub enum FujiFilmColorMode {
    #[strum(serialize = "Standard")]
    Standard = 0x0,
    #[strum(serialize = "Chrome")]
    Chrome = 0x10,
    #[strum(serialize = "B & W")]
    BW = 0x30,
}

impl_simple_enum_make_note_raw_parse!(FujiFilmColorMode, u16);

generate_maker_tags! {
    vendor: Fujifilm,
    tags: [
    (Version, 0x0000, "Version"),
    (InternalSerialNumber, 0x0010, "Internal Serial Number"),
    (Quality, 0x1000, "Quality"),
    (Sharpness, 0x1001, "Sharpness"),
    (WhiteBalance, 0x1002, "White Balance"),
    (Saturation, 0x1003, "Saturation"),
    (Contrast, 0x1004, "Contrast"),
    (ColorTemperature, 0x1005, "Color Temperature"),
    (Contrast2, 0x1006, "Contrast 2"),
    (WhiteBalanceFineTune, 0x100a, "White Balance Fine Tune"),
    (NoiseReduction, 0x100b, "Noise Reduction"),
    (NoiseReduction2, 0x100e, "Noise Reduction 2"),
    (Clarity, 0x100f, "Clarity"),
    (FujiFlashMode, 0x1010, "Fuji Flash Mode"),
    (FlashExposureComp, 0x1011, "Flash Exposure Comp"),
    (Macro, 0x1020, "Macro"),
    (FocusMode, 0x1021, "Focus Mode"),
    (AFMode, 0x1022, "AF Mode"),
    (FocusPixel, 0x1023, "Focus Pixel"),
    (PrioritySettings, 0x102b, "Priority Settings"),
    (FocusSettings, 0x102d, "Focus Settings"),
    (AFCSettings, 0x102e, "AFC Settings"),
    (SlowSync, 0x1030, "Slow Sync"),
    (PictureMode, 0x1031, "Picture Mode"),
    (ExposureCount, 0x1032, "Exposure Count"),
    (EXRAuto, 0x1033, "EXR Auto"),
    (EXRMode, 0x1034, "EXR Mode"),
    (MultipleExposure, 0x1037, "Multiple Exposure"),
    (ShadowTone, 0x1040, "Shadow Tone"),
    (HighlightTone, 0x1041, "Highlight Tone"),
    (DigitalZoom, 0x1044, "Digital Zoom"),
    (LensModulationOptimizer, 0x1045, "Lens Modulation Optimizer"),
    (GrainEffectRoughness, 0x1047, "Grain Effect Roughness"),
    (ColorChromeEffect, 0x1048, "Color Chrome Effect"),
    (BWAdjustment, 0x1049, "BW Adjustment"),
    (BWMagentaGreen, 0x104b, "BW Magenta Green"),
    (GrainEffectSize, 0x104c, "Grain Effect Size"),
    (CropMode, 0x104d, "Crop Mode"),
    (ColorChromeFXBlue, 0x104e, "Color Chrome FX Blue"),
    (ShutterType, 0x1050, "Shutter Type"),
    (CropFlag, 0x1051, "Crop Flag"),
    (CropTopLeft, 0x1052, "Crop Top Left"),
    (CropSize, 0x1053, "Crop Size"),
    (AutoBracketing, 0x1100, "Auto Bracketing"),
    (SequenceNumber, 0x1101, "Sequence Number"),
    (WhiteBalanceBracketing, 0x1102, "White Balance Bracketing"),
    (DriveSettings, 0x1103, "Drive Settings"),
    (PixelShiftShots, 0x1105, "Pixel Shift Shots"),
    (PixelShiftOffset, 0x1106, "Pixel Shift Offset"),
    (CompositeImageMode, 0x1150, "Composite Image Mode"),
    (CompositeImageCount1, 0x1151, "Composite Image Count 1"),
    (CompositeImageCount2, 0x1152, "Composite Image Count 2"),
    (PanoramaAngle, 0x1153, "Panorama Angle"),
    (PanoramaDirection, 0x1154, "Panorama Direction"),
    (AdvancedFilter, 0x1201, "Advanced Filter"),
    (ColorMode, 0x1210, "Color Mode"),
    (BlurWarning, 0x1300, "Blur Warning"),
    (FocusWarning, 0x1301, "Focus Warning"),
    (ExposureWarning, 0x1302, "Exposure Warning"),
    (GEImageSize, 0x1304, "GE Image Size"),
    (DynamicRange, 0x1400, "Dynamic Range"),
    (FilmMode, 0x1401, "Film Mode"),
    (DynamicRangeSetting, 0x1402, "Dynamic Range Setting"),
    (DevelopmentDynamicRange, 0x1403, "Development Dynamic Range"),
    (MinFocalLength, 0x1404, "Min Focal Length"),
    (MaxFocalLength, 0x1405, "Max Focal Length"),
    (MaxApertureAtMinFocal, 0x1406, "Max Aperture At Min Focal"),
    (MaxApertureAtMaxFocal, 0x1407, "Max Aperture At Max Focal"),
    (AutoDynamicRange, 0x140b, "Auto Dynamic Range"),
    (ImageStabilization, 0x1422, "Image Stabilization"),
    (SceneRecognition, 0x1425, "Scene Recognition"),
    (Rating, 0x1431, "Rating"),
    (ImageGeneration, 0x1436, "Image Generation"),
    (ImageCount, 0x1438, "Image Count"),
    (DRangePriority, 0x1443, "D Range Priority"),
    (DRangePriorityAuto, 0x1444, "D Range Priority Auto"),
    (DRangePriorityFixed, 0x1445, "D Range Priority Fixed"),
    (FlickerReduction, 0x1446, "Flicker Reduction"),
    (FujiModel, 0x1447, "Fuji Model"),
    (FujiModel2, 0x1448, "Fuji Model 2"),
    (WBRed, 0x144a, "WB Red"),
    (WBGreen, 0x144b, "WB Green"),
    (WBBlue, 0x144c, "WB Blue"),
    (RollAngle, 0x144d, "Roll Angle"),
    (VideoRecordingMode, 0x3803, "Video Recording Mode"),
    (PeripheralLighting, 0x3804, "Peripheral Lighting"),
    (VideoCompression, 0x3806, "Video Compression"),
    (FrameRate, 0x3820, "Frame Rate"),
    (FrameWidth, 0x3821, "Frame Width"),
    (FrameHeight, 0x3822, "Frame Height"),
    (FullHDHighSpeedRec, 0x3824, "Full HD High Speed Rec"),
    (FaceElementSelected, 0x4005, "Face Element Selected"),
    (FacesDetected, 0x4100, "Faces Detected"),
    (FacePositions, 0x4103, "Face Positions"),
    (NumFaceElements, 0x4200, "Num Face Elements"),
    (FaceElementTypes, 0x4201, "Face Element Types"),
    (FaceElementPositions, 0x4203, "Face Element Positions"),
    (FaceRecInfo, 0x4282, "Face Rec Info"),
    (FileSource, 0x8000, "File Source"),
    (OrderNumber, 0x8002, "Order Number"),
    (FrameNumber, 0x8003, "Frame Number"),
    (Parallax, 0xb211, "Parallax"),
    ]
}
